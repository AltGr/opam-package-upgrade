(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2017 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open Cmdliner

let upgrade_package_command =
  let files =
    Arg.(value & pos_all OpamArg.existing_filename_dirname_or_dash [] &
         info [] ~doc:
           "Package definition (\"opam\") files to update, or package \
            directory containing them, or \"-\" to read from stdin.")
  in
  let cmd files =
    OpamClientConfig.opam_init ();
    files |> List.map @@ fun f ->
    let upgrade_file f =
      let o1 = OpamFile.OPAM.read f in
      let o2 = OpamFormatUpgrade.opam_file o1 in
      if o2 <> o1 then
        (OpamFile.OPAM.write f;
         OpamConsole.formatted_msg "File %s upgraded to format %s"
           (OpamFile.to_string f)
           (OpamVersion.to_string OpamFormatUpgrade.latest_version))
      else
        OpamConsole.formatted_msg "File %s is already at latest version"
          (OpamFile.to_string f)
    in
    match f with
      | None ->
        OpamFile.OPAM.read_from_channel stdin |>
        OpamFormatUpgrade.opam_file |>
        OpamFile.OPAM.write_to_channel stdout
      | Some (F f) -> upgrade_file f
      | Some (D d) ->
        match OpamPinned.files_in_source d with
        | [] -> OpamConsole.error "No opam files found in %s"
                  (OpamFilename.Dir.to_string d)
        | fs -> List.map (fun (_, f) -> upgrade_file f) fs
  in
  Term.(pure cmd $ files)

let () =
  OpamSystem.init ();
  match Term.eval upgrade_package_command with
  | `Error _ -> exit 1
  | _ -> exit 0

