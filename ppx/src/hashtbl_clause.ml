open! Base
open! Import

include Keyed_container_clause.Make (struct
    module Submodule_form = struct
      let name = "Table"
      let arity = 1
      let value_types ~type_parameters = [ List.nth_exn type_parameters 0 ]
    end

    module Parameterized_form = struct
      let name = "Hashtbl"
      let arity = 2
      let key_type ~type_parameters = Some (List.nth_exn type_parameters 0)
      let value_types ~type_parameters = [ List.nth_exn type_parameters 1 ]
    end

    module M_module_form = struct
      let name = "Hashtbl"
      let arity = 1
      let value_types ~type_parameters = [ List.nth_exn type_parameters 0 ]
    end
  end)
