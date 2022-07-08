open! Base
open! Import

include Keyed_container_clause.Make (struct
    module Submodule_form = struct
      let name  = "Total_map"
      let arity = 1
      let value_types ~type_parameters = [ List.nth_exn type_parameters 0 ]
    end

    module Parameterized_form = struct
      let name                         = "Total_map"
      let arity                        = 4
      let key_type ~type_parameters    = Some (List.nth_exn type_parameters 0)
      let value_types ~type_parameters = [ List.nth_exn type_parameters 1 ]
    end
  end)
