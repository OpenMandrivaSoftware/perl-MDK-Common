open Types

let _ = 
  let args = List.tl (Array.to_list Sys.argv) in
  let args = if args = [] then ["/tmp/t.pl"] else args in
  List.iter (fun file ->
    try
      let lexbuf = Lexing.from_channel (Unix.open_process_in (Printf.sprintf "expand \"%s\"" file)) in
      try
	Info.start_a_new_file file ;
	if false then
	  let t = Lexer.lexbuf2list (Lexer.concat_bareword_paren (Lexer.concat_spaces Lexer.token)) lexbuf in
	  let _,_ = t, t in ()
	else
	  let t = Parser.prog (Lexer.concat_bareword_paren (Lexer.concat_spaces Lexer.token)) lexbuf in
	  let _,_ = t, t in ()
      with Failure s -> (
	prerr_endline s ;
	exit 1
     )
    with _ -> prerr_endline ("bad file " ^ file)
  ) args
