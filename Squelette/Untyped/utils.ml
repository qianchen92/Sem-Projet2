(** {2 Fonctions d'entrée/sortie fichiers} *)

(**/**)
(*  Voir 
    http://stackoverflow.com/questions/11276985/emulating-try-with-finally-in-ocaml *)
let unwind ~protect f x =
  let module E = struct type 'a t = Left of 'a | Right of exn end in
  let res = try E.Left (f x) with e -> E.Right e in
  let () = protect x in
  match res with
  | E.Left  y -> y
  | E.Right e -> raise e

let with_input_channel inch f =
  unwind ~protect:close_in f inch

let with_output_channel otch f =
  unwind ~protect:close_out f otch

let with_input_file fname =
  with_input_channel (open_in fname)

let with_output_file fname =
  with_output_channel (open_out fname)
(**/**)

(** [parse filename] analyse le fichier [filename] et produit la
    représentation abstraite du programme qu’il contient *)
let parse source =
  with_input_file source
  (fun ch ->
    Parser.make_term Lexer.token (Lexing.from_channel ch)
  )

