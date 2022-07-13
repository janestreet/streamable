open! Core
open! Async_kernel
open! Import
open  Deferred.Or_error.Let_syntax
include State_rpc_intf

module type S_plus = sig
  include S

  module Underlying_rpc : sig
    val dispatch
      :  Rpc.Connection.t
      -> query
      -> (state * update Pipe.Reader.t) Deferred.Or_error.t

    val implement
      :  ('c -> query -> (state * update Pipe.Reader.t) Deferred.Or_error.t)
      -> 'c Rpc.Implementation.t

    val description : Rpc.Description.t
  end
end

type ('q, 's, 'u) t =
  (module S_plus with type query = 'q and type state = 's and type update = 'u)

module Make (X : S) = struct
  module Underlying_rpc = struct
    type query = X.query [@@deriving bin_io]

    type 'a part_or_done =
      | Part of 'a
      | Done
    [@@deriving bin_io]

    type response =
      | State  of X.State.Intermediate.Part.t  part_or_done
      | Update of X.Update.Intermediate.Part.t part_or_done
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
      let%bind () = Pipe.transfer pipe w ~f:(fun part -> constructor (Part part)) in
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
        | `Eof    ->
          Deferred.Or_error.errorf
            "Streamable.State_rpc: EOF before receiving complete %s"
            noun
        | `Ok msg ->
          (match match_ msg with
           | Error e        -> Deferred.return (Error e)
           | Ok (Part part) -> loop (X.Intermediate.apply_part acc part)
           | Ok Done        -> return (X.finalize acc))
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

    let implement' f =
      Rpc.Pipe_rpc.implement rpc (fun c q ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state_pipe, update_pipes = f c q in
        return
        @@ Pipe.create_reader ~close_on_exception:true (fun w ->
          let open Deferred.Let_syntax in
          upon (Pipe.closed w) (fun () ->
            (match Pipe.read_now' update_pipes with
             | `Eof | `Nothing_available -> ()
             | `Ok queue                 ->
               Queue.iter queue ~f:(fun update_pipe ->
                 Pipe.close_read update_pipe));
            Pipe.close_read update_pipes);
          let%bind () = write_msg w state_pipe ~constructor:(fun x -> State x) in
          Pipe.iter update_pipes ~f:(fun update_pipe ->
            write_msg w update_pipe ~constructor:(fun x -> Update x))))
    ;;

    let implement f =
      implement' (fun c q ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state, updates = f c q in
        return
          ( State.to_parts state |> Pipe.of_sequence
          , Pipe.map updates ~f:(fun update ->
              Update.to_parts update |> Pipe.of_sequence) ))
    ;;

    let read_state r =
      read_msg
        (module State)
        r
        ~noun:"state"
        ~match_:(function
          | State  x -> Ok x
          | Update _ -> Or_error.errorf "Streamable.State_rpc: incomplete state message")
    ;;

    let read_update r =
      read_msg
        (module Update)
        r
        ~noun:"update"
        ~match_:(function
          | Update x -> Ok x
          | State  _ -> Or_error.errorf "Streamable.State_rpc: incomplete update message")
    ;;

    let dispatch conn query =
      let%bind r, _ =
        Deferred.Let_syntax.(Rpc.Pipe_rpc.dispatch rpc conn query >>| Or_error.join)
      in
      let%bind initial_state = read_state r in
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
      return (initial_state, updates)
    ;;
  end

  module X_plus = struct
    include X
    module Underlying_rpc = Underlying_rpc
  end

  let rpc =
    (module X_plus : S_plus
      with type query = X.query
       and type state  = X.state
       and type update = X.update)
  ;;

  let implement' = Underlying_rpc.implement'
end

let description     (type q s u) ((module X) : (q, s, u) t) = X.Underlying_rpc.description
let dispatch        (type q s u) ((module X) : (q, s, u) t) = X.Underlying_rpc.dispatch
let implement       (type q s u) ((module X) : (q, s, u) t) = X.Underlying_rpc.implement
let bin_query_shape (type q s u) ((module X) : (q, s, u) t) = X.bin_query.shape

let bin_state_shape (type q s u) ((module X) : (q, s, u) t) =
  X.State.Intermediate.Part.bin_t.shape
;;

let bin_update_shape (type q s u) ((module X) : (q, s, u) t) =
  X.Update.Intermediate.Part.bin_t.shape
;;
