(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)
open Eliom_parameters
open Lwt

let service = lazy begin
  Eliom_predefmod.Text.register_new_service
    ~path:[""]
    ~get_params:unit
    (fun sp () () -> return ("Hello, world\n", "text/plain"))
end

let () =
  Eliom_services.register_eliom_module "hello"
    (fun () -> ignore (Lazy.force service))

let () = Ocsigen_server.start_server ()

