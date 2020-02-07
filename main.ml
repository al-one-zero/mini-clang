open Format

let ifile = ref ""

let set_file f s = f :=s 

let options = []

let usage = "usage: clang [filename]"

let _ = 
  Arg.parse options (set_file ifile) usage;
  if !ifile="" then (eprintf "Aucun fichier a compiler\n@?"; exit 1); 
  if not (Filename.check_suffix !ifile ".c") then 
    (eprintf "Le fichier d'entree doit avoir l'extension .c\n@?";
     Arg.usage options usage;exit 1); 
  
  let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  let _ = Parser.file Lexer.nexttoken buf in
  close_in f
