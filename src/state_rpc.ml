open! Core
open! Async_kernel
open! Import
open Deferred.Or_error.Let_syntax
include State_rpc_intf

module type S_plus = sig
  include S

  module Underlying_rpc : sig
    val dispatch
      :  Rpc.Connection.t
      -> query
      -> (state * update Pipe.Reader.t) Deferred.Or_error.t

    val dispatch'
      :  Rpc.Connection.t
      -> query
      -> (state * update Pipe.Reader.t) Or_error.t Deferred.Or_error.t

    val dispatch_with_rpc_result
      :  Rpc.Connection.t
      -> query
      -> ( (state * update Pipe.Reader.t) Or_error.t
           , Async_rpc_kernel.Rpc_error.t )
           Deferred.Result.t

    val dispatch_with_rpc_result_and_metadata
      :  Rpc.Connection.t
      -> query
      -> metadata:Rpc_metadata.V2.t
      -> ( (state * update Pipe.Reader.t) Or_error.t
           , Async_rpc_kernel.Rpc_error.t )
           Deferred.Result.t

    val implement
      :  ?on_exception:Rpc.On_exception.t
      -> ?leave_open_on_exception:bool
      -> ('c -> query -> (state * update Pipe.Reader.t) Deferred.Or_error.t)
      -> 'c Rpc.Implementation.t

    val implement_with_auth
      :  ?on_exception:Rpc.On_exception.t
      -> ?leave_open_on_exception:bool
      -> ('c
          -> query
          -> (state * update Pipe.Reader.t) Async_rpc_kernel.Or_not_authorized.t
               Deferred.t)
      -> 'c Rpc.Implementation.t

    val description : Rpc.Description.t
  end
end

type ('q, 's, 'u) t =
  (module S_plus with type query = 'q and type state = 's and type update = 'u)

module Part_or_done = struct
  type 'a t =
    | Part of 'a
    | Done
  [@@deriving bin_io]
end

module Response = struct
  type ('state_part, 'update_part) t =
    | State of 'state_part Part_or_done.t
    | Update of 'update_part Part_or_done.t
  [@@deriving bin_io]
end

module Direct_writer = struct
  module T = Rpc.Pipe_rpc.Direct_stream_writer

  type ('state_part, 'update_part) t =
    { writer : ('state_part, 'update_part) Response.t T.t
    ; state_finalised : unit Ivar.t
    }

  let wrap writer = { writer; state_finalised = Ivar.create () }
  let[@inline] is_state_finalised t = Ivar.is_full t.state_finalised
  let state_finalised t = Ivar.read t.state_finalised

  let[@cold] raise_state_write_after_finalising () =
    raise_s
      [%message
        "Cannot write state parts to State_rpc.Direct_writer after finalising initial \
         state"]
  ;;

  let[@inline] raise_if_finalised t =
    if is_state_finalised t then raise_state_write_after_finalising ()
  ;;

  let write_state_without_pushback_exn t state =
    raise_if_finalised t;
    T.write_without_pushback t.writer (Response.State (Part state))
  ;;

  let finalise_state_without_pushback_exn t =
    raise_if_finalised t;
    match T.write_without_pushback t.writer (Response.State Done) with
    | `Ok ->
      Ivar.fill_exn t.state_finalised ();
      `Ok
    | `Closed -> `Closed
  ;;

  let[@cold] raise_update_write_before_finalising () =
    raise_s
      [%message
        "Cannot write update parts to State_rps.Direct_writer before finalising initial \
         state"]
  ;;

  let[@inline] raise_if_not_finalised t =
    if not (is_state_finalised t) then raise_update_write_before_finalising ()
  ;;

  let write_update_without_pushback_exn t update =
    raise_if_not_finalised t;
    T.write_without_pushback t.writer (Response.Update (Part update))
  ;;

  let finalise_update_without_pushback_exn t =
    raise_if_not_finalised t;
    T.write_without_pushback t.writer (Response.Update Done)
  ;;

  let close t = T.close t.writer
  let closed t = T.closed t.writer
  let flushed t = T.flushed t.writer
  let is_closed t = T.is_closed t.writer

  module Expert = struct
    (* Nothing.bin_writer_t is used here so we don't have to ask for an update_bin_writer
       when serialising a state part and vice versa *)
    let create_state_part ~state_bin_writer state_part =
      Bin_prot.Writer.to_bigstring
        (Response.bin_writer_t state_bin_writer Nothing.bin_writer_t)
        (State (Part state_part))
    ;;

    let finalise_state_message =
      lazy
        (Bin_prot.Writer.to_bigstring
           (Response.bin_writer_t Nothing.bin_writer_t Nothing.bin_writer_t)
           (State Done))
    ;;

    let create_update_part ~update_bin_writer update_part =
      Bin_prot.Writer.to_bigstring
        (Response.bin_writer_t Nothing.bin_writer_t update_bin_writer)
        (Update (Part update_part))
    ;;

    let finalise_update_message =
      lazy
        (Bin_prot.Writer.to_bigstring
           (Response.bin_writer_t Nothing.bin_writer_t Nothing.bin_writer_t)
           (Update Done))
    ;;

    let write_without_pushback ?(pos = 0) ?len t buf =
      let len = Option.value_or_thunk len ~default:(fun () -> Bigstring.length buf) in
      T.Expert.write_without_pushback t.writer ~buf ~pos ~len
    ;;
  end

  module Group = struct
    module T_group = Rpc.Pipe_rpc.Direct_stream_writer.Group

    type ('state_part, 'update_part) t = ('state_part, 'update_part) Response.t T_group.t

    let create = T_group.create
    let flushed_or_closed = T_group.flushed_or_closed

    let add_exn t writer =
      if not (is_state_finalised writer)
      then
        raise_s
          [%message
            "Can't add writer to State_rpc.Direct_writer.Group until it has finalised \
             its initial state"];
      T_group.add_exn t writer.writer
    ;;

    let remove t writer = T_group.remove t writer.writer

    let write_update_without_pushback t update =
      T_group.write_without_pushback t (Response.Update (Part update))
    ;;

    let finalise_update_without_pushback t =
      T_group.write_without_pushback t (Response.Update Done)
    ;;

    let length = T_group.length

    let close_all t =
      (* We can't implement [to_list] without storing extra data (it seems wrong to return
         writers that are not [phys_equal] to the original, even though we know what all
         the fields would be).

         Instead we supply this to close the added writers.
      *)
      T_group.to_list t |> List.iter ~f:T.close
    ;;
  end
end

module Make (X : S) = struct
  module Underlying_rpc = struct
    type query = X.query [@@deriving bin_io]

    type response = (X.State.Intermediate.Part.t, X.Update.Intermediate.Part.t) Response.t
    [@@deriving bin_io]

    type error = Error.Stable.V2.t [@@deriving bin_io]

    let rpc =
      Rpc.Pipe_rpc.create
        ()
        ?client_pushes_back:(if X.client_pushes_back then Some () else None)
        ~name:X.name
        ~version:X.version
        ~bin_query
        ~bin_response
        ~bin_error
    ;;

    module type S = sig
      type part

      include Main.S_rpc with type Intermediate.Part.t = part
    end

    let write_msg w pipe ~constructor =
      let open Deferred.Let_syntax in
      let%bind () =
        Pipe.transfer pipe w ~f:(fun part -> constructor (Part_or_done.Part part))
      in
      Pipe.write_if_open w (constructor Done)
    ;;

    let read_msg
      (type a p)
      (module X : S with type t = a and type part = p)
      r
      ~match_
      ~noun
      =
      let rec loop acc =
        match%bind Pipe.read r |> Deferred.ok with
        | `Eof ->
          Deferred.Or_error.errorf
            "Streamable.State_rpc: EOF before receiving complete %s"
            noun
        | `Ok msg ->
          (match match_ msg with
           | Error e -> Deferred.return (Error e)
           | Ok (Part_or_done.Part part) -> loop (X.Intermediate.apply_part acc part)
           | Ok Done -> return (X.finalize acc))
      in
      loop (X.Intermediate.create ())
    ;;

    let description = Rpc.Pipe_rpc.description rpc

    module State = struct
      include X.State

      type part = Intermediate.Part.t
    end

    module Update = struct
      include X.Update

      type part = Intermediate.Part.t
    end

    let implement_with_pipes state_pipe update_pipes =
      Pipe.create_reader ~close_on_exception:true (fun w ->
        let open Deferred.Let_syntax in
        upon (Pipe.closed w) (fun () ->
          (match Pipe.read_now' update_pipes with
           | `Eof | `Nothing_available -> ()
           | `Ok queue ->
             Queue.iter queue ~f:(fun update_pipe -> Pipe.close_read update_pipe));
          Pipe.close_read update_pipes);
        let%bind () = write_msg w state_pipe ~constructor:(fun x -> Response.State x) in
        Pipe.iter update_pipes ~f:(fun update_pipe ->
          write_msg w update_pipe ~constructor:(fun x -> Update x)))
    ;;

    let implement' ?on_exception ?leave_open_on_exception f =
      Rpc.Pipe_rpc.implement
        ?on_exception
        ~leave_open_on_exception:(Option.value leave_open_on_exception ~default:true)
        rpc
        (fun c q ->
           let open Deferred.Or_error.Let_syntax in
           let%bind state_pipe, update_pipes = f c q in
           implement_with_pipes state_pipe update_pipes |> return)
    ;;

    let implement_with_auth' ?on_exception ?leave_open_on_exception f =
      Rpc.Pipe_rpc.implement_with_auth
        ?on_exception
        ?leave_open_on_exception
        rpc
        (fun c q ->
           let%map.Async_rpc_kernel.Or_not_authorized.Deferred state_pipe, update_pipes =
             f c q
           in
           implement_with_pipes state_pipe update_pipes |> Result.return)
    ;;

    let implement ?on_exception ?leave_open_on_exception f =
      implement' ?on_exception ?leave_open_on_exception (fun c q ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state, updates = f c q in
        return
          ( State.to_parts state |> Pipe.of_sequence
          , Pipe.map updates ~f:(fun update -> Update.to_parts update |> Pipe.of_sequence)
          ))
    ;;

    let implement_with_auth ?on_exception ?leave_open_on_exception f =
      implement_with_auth' ?on_exception ?leave_open_on_exception (fun c q ->
        let%map.Async_rpc_kernel.Or_not_authorized.Deferred state, updates = f c q in
        ( State.to_parts state |> Pipe.of_sequence
        , Pipe.map updates ~f:(fun update -> Update.to_parts update |> Pipe.of_sequence) ))
    ;;

    let read_state r =
      read_msg (module State) r ~noun:"state" ~match_:(function
        | Response.State x -> Ok x
        | Update _ -> Or_error.errorf "Streamable.State_rpc: incomplete state message")
    ;;

    let read_update r =
      read_msg (module Update) r ~noun:"update" ~match_:(function
        | Response.Update x -> Ok x
        | State _ -> Or_error.errorf "Streamable.State_rpc: incomplete update message")
    ;;

    let dispatch_gen dispatch conn query =
      let%bind.Deferred.Result server_response = dispatch rpc conn query in
      match server_response with
      | Error _ as error -> Deferred.Result.return error
      | Ok (r, _) ->
        (match%bind.Deferred read_state r with
         | Error _ as error -> Deferred.Result.return error
         | Ok initial_state ->
           let updates =
             Pipe.create_reader ~close_on_exception:true (fun w ->
               let open Deferred.Let_syntax in
               let rec loop () =
                 match%bind
                   Deferred.choose
                     [ Deferred.choice (read_update r) Result.ok
                     ; Deferred.choice (Pipe.closed w) (fun () -> None)
                     ]
                 with
                 | Some update ->
                   let%bind () = Pipe.write_if_open w update in
                   loop ()
                 | None -> return ()
               in
               let%bind () = loop () in
               Pipe.close_read r;
               return ())
           in
           Deferred.Result.return (Ok (initial_state, updates)))
    ;;

    let dispatch' conn query = dispatch_gen Rpc.Pipe_rpc.dispatch conn query
    let dispatch conn query = dispatch' conn query |> Deferred.map ~f:Or_error.join

    let dispatch_with_rpc_result conn query =
      dispatch_gen Rpc.Pipe_rpc.dispatch' conn query
    ;;

    let dispatch_with_rpc_result_and_metadata conn query ~metadata =
      dispatch_gen
        (Rpc.Pipe_rpc.Expert.dispatch_bin_prot_with_metadata' ~metadata)
        conn
        query
    ;;
  end

  module X_plus = struct
    include X
    module Underlying_rpc = Underlying_rpc
  end

  let rpc =
    (module X_plus : S_plus
      with type query = X.query
       and type state = X.state
       and type update = X.update)
  ;;

  let implement' = Underlying_rpc.implement' ~leave_open_on_exception:true

  let implement_direct ?on_exception f =
    Rpc.Pipe_rpc.implement_direct
      ?on_exception
      ~leave_open_on_exception:true
      Underlying_rpc.rpc
      (fun c q writer -> f c q (Direct_writer.wrap writer))
  ;;
end

let description (type q s u) ((module X) : (q, s, u) t) = X.Underlying_rpc.description
let dispatch (type q s u) ((module X) : (q, s, u) t) = X.Underlying_rpc.dispatch
let dispatch' (type q s u) ((module X) : (q, s, u) t) = X.Underlying_rpc.dispatch'

let dispatch_with_rpc_result (type q s u) ((module X) : (q, s, u) t) =
  X.Underlying_rpc.dispatch_with_rpc_result
;;

module Expert = struct
  let dispatch_with_rpc_result_and_metadata (type q s u) ((module X) : (q, s, u) t) =
    X.Underlying_rpc.dispatch_with_rpc_result_and_metadata
  ;;
end

let implement
  (type q s u)
  ?on_exception
  ?leave_open_on_exception
  ((module X) : (q, s, u) t)
  =
  X.Underlying_rpc.implement ?on_exception ?leave_open_on_exception
;;

let implement_with_auth
  (type q s u)
  ?on_exception
  ?leave_open_on_exception
  ((module X) : (q, s, u) t)
  =
  X.Underlying_rpc.implement_with_auth ?on_exception ?leave_open_on_exception
;;

let bin_query_shape (type q s u) ((module X) : (q, s, u) t) = X.bin_query.shape

let bin_state_shape (type q s u) ((module X) : (q, s, u) t) =
  X.State.Intermediate.Part.bin_t.shape
;;

let bin_update_shape (type q s u) ((module X) : (q, s, u) t) =
  X.Update.Intermediate.Part.bin_t.shape
;;

let version (type q s u) ((module X) : (q, s, u) t) = X.version
let name (type q s u) ((module X) : (q, s, u) t) = X.name
