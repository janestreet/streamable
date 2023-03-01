open! Base
open! Import

type t = V1 [@@deriving enumerate]

let to_int = function
  | V1 -> 1
;;

let of_int_exn loc = function
  | 1 -> V1
  | n ->
    Location.raise_errorf
      ~loc
      !"Unknown ppx_streamable version: %d. The supported versions are: {%s}"
      n
      (all |> List.map ~f:(Fn.compose Int.to_string to_int) |> String.concat ~sep:", ")
;;

let module_name = function
  | V1 -> "V1"
;;
