open Common

type config_file = {
    basedir : int option ;
  }

let ignored_packages = ref []

let default = { basedir = None }


let config_cache = Hashtbl.create 16

let read dir =
  try Hashtbl.find config_cache dir with Not_found ->
  try
    let file_name = dir ^ "/.perl_checker" in
    let fh = open_in file_name in
    let config =
      fold_lines (fun config line ->
	match words line with
	| [ "Basedir"; ".."    ] -> { config with basedir = Some 1 }
	| [ "Basedir"; "../.." ] -> { config with basedir = Some 2 }
	| [] -> config (* blank line *)
	| [ "Ignore"; pkg ]
	| [ pkg ] (* the deprecated form *)
	  -> lpush ignored_packages pkg; config
	| _ -> prerr_endline (Printf.sprintf "bad line \"%s\" in %s" line file_name); config
      ) default fh
    in
    Hashtbl.add config_cache dir config ;
    config
  with Sys_error _ -> default

let rec read_any dir depth =
  if depth = 0 then () else
  let _ = read dir in
  read_any (dir ^ "/..") (depth - 1)
