open Types

let _ = 
  let file = try Sys.argv.(2) with _ -> try Sys.argv.(1) with _ -> "/tmp/t.pl" in
  let lexbuf = Lexing.from_channel (open_in file) in
  let _ = 
    try
      Info.start_a_new_file file ;
      if false then
	let tokens = Lexer.lexbuf2list Lexer.token lexbuf in
	let _,_ = tokens, tokens in ""
      else
	Parser.prog Lexer.token lexbuf
    with Failure s -> (
      prerr_endline s ;
      exit 1
     ) in
  ()
