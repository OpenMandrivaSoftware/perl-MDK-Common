open Types

let _ = 
  let args = List.tl (Array.to_list Sys.argv) in
  List.iter (fun file ->
    try
      let lexbuf = Lexing.from_channel (open_in file) in
      let _ = 
	try
	  Info.start_a_new_file file ;
	  if false then
	    let t = Lexer.lexbuf2list Lexer.token lexbuf in
	    let _,_ = t, t in ""
	  else
	    Parser.prog Lexer.token lexbuf
	with Failure s -> (
	  prerr_endline s ;
	  exit 1
     ) in
      ()
    with _ -> prerr_endline ("bad file " ^ file)
  ) args
