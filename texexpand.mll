{
(*
 * texexpand [-c] [-w] [-x] [file[.tex]]
 *
 *	This program is used to expand TeX or LaTeX \input or \include constructs
 *      in a source file.
 *
 * Copyright Roberto Di Cosmo <roberto@dicosmo.org>, 2003-
 *
 * Distributed under the General Public Licence, version 2 or later
 *
 *)

let inc r = r:= !r+1;;
let dec r = r:= !r-1;;

exception Eof;;

(* "macros" for later use *)

let havekpsewhich = ref false;;         (* do we have kpsewhich? *)
let defaultinputs = ref "";;            (* the default search path on this system *)
let curfn=ref"";;                       (* name of the currently scanned file *)
let bibMain=ref "";;		        (* holding place for bibliography file name *)
let depthprefix=ref "";;		(* string for depth prefix   *)
let incList = (ref []: string list ref);;	(* list of includeonly files *)
let inputPaths= (ref []: string list ref);; (* list of input paths in order *)
let progName=ref "";;			(* name we were invoked with *)
let fpstack=ref [];;	                (* stack of input/include files, file paths and echo strings triples *)
let cfp = ref 0;;			(* count of files in stack *)
let fDepend = ref false;;	        (* flag to indicate depend *)
let fExport = ref false;;	        (* flag to select export mode (insert the filenames of input/include-d files) *)
let fBibExport = ref false;;		(* flag to select bibliography export mode (insert the BibTeX-generated bibliography file ) *)
let fWord = ref false;;			(* flag for -w option *)
let fFollow = ref true;;	        (* flag to follow input/include *)
let fComments = ref false;;	        (* flag to echo comments *)
let paropen = ref 0;;		        (* open-parenthesis counter *)

let echo lexbuf = if (not !fDepend) then print_string (Lexing.lexeme lexbuf);;
let ignore ()	=	();;
let space s	=	if (not !fWord) then print_char ' ' else print_char '\n';;
let newline s	=	if (not !fWord) then print_char('\n');;

let dptstring lgt = String.make lgt '*';;
let dptblanks lgt = String.make lgt ' ';;

(* execute a Unix command and get its result as a string *)

let launch_command b name =
 let ic = Unix.open_process_in name in
 let ib = String.create 1024 in
 try
   while true do
    let n = input ic ib 0 1024 in
     if n=0 then raise End_of_file
     else Buffer.add_string b (String.sub ib 0 n)
    done;
    2
 with
 | End_of_file ->
    (match Unix.close_process_in ic with
     | Unix.WEXITED n -> n
     | _ -> 2);;

let chop s = try String.sub s 0 (String.rindex s '\n') with _ -> s;;

let string_command name =
  let b = Buffer.create 10 in
  let status = launch_command b name in
  if status = 0 then  chop (Buffer.contents b)
  else failwith (Printf.sprintf "Command %s failed to execute" name);;

(******
** setInputPaths -- sets rgsbInputPaths to the values indicated by the
**	TEXINPUTS environment variable if set or else DEFAULTINPUTS.
******)

let rec split_string s p start =
  let len = String.length s
  and i = ref start in
  while !i < len && p s.[!i] do incr i done;
  if !i >= len then [] else begin
    let i0 = !i in
    while !i < len && not (p s.[!i]) do incr i done;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string s p i1
  end;;


let tstkpsewhich () =   havekpsewhich:= 
    (match Unix.system "kpsewhich --version > /dev/null" with
      Unix.WEXITED 127 -> false
    | _ -> true);;


let setInputPaths () =  
  let paths = 
    match !havekpsewhich with
      true -> string_command "kpsewhich --show-path tex"
    | false ->  
	let envpaths = Sys.getenv("TEXINPUTS") in
	if envpaths = "" then !defaultinputs else envpaths
  in inputPaths := split_string paths (fun c -> c = ':') 0
;;

(******
** makeBibName -- take a pathname and turn it into a valid .bbl filename
******)
let makeBibName texname =
  let l = String.length texname in
  let pref = (String.sub texname (l-4) 4) in
  if pref = ".tex" then
    (* is a tex name *)
    (String.sub texname 0 (l-4))^".bbl"
  else
    (* is not a .tex name *)
    (texname^".bbl")


(******
** fullpath_to_file -- tries to open fn in each of the paths in turn.
**	For each input path the following order is used:
**		file.tex - must be as named, if not there go to the next path
**		file.ext - random extension, try it
**		file     - base name, add .tex and try it
**		file     - try it as is
**	Notice that if file exists in the first path and file.tex exists in
**	one of the other paths, file in the first path is what is opened.
**	If fn begins with a '/', no paths are searched.
**
**      If kpsewhich is present in the system, we use it instead.
******)

exception Found_texfile of string
exception No_texfile
exception False

let has_suffix suf str =
  let lsuf = String.length suf in
  let lstr =  String.length str in
  lstr >= lsuf &&
  begin
    try
      let d = lstr - lsuf in
      for i = 0 to lsuf - 1 do if str.[d + i] <> suf.[i] then raise False done;
      true;
    with False -> false
  end;;

let has_extension fn =
  let i = String.rindex fn '.' in
  if (i>0 &&
      let c = String.get fn (i-1) in (c<>'/' && c<> '.') &&
      i<(String.length fn)-1)
  then true
  else false
;;

let variants = [
  (* predicate, extensions, withpath, onlyonce, continue [if predicate succeeds, but file not found] *)
  ((fun fn -> String.get fn 0 = '/'), ["";".tex"], false, true,false);
  ((fun fn -> has_suffix fn ".tex"), [""], true, false,false);
  ((fun fn -> has_extension fn), [""], true, false,false);
  ((fun fn -> true), [".tex"], true, false,true);
  ((fun fn -> true), [""], true, false,true);
];;

let isreadable fn =
  Sys.file_exists fn &&
  (try
    let ic = open_in fn in close_in ic;true
   with _ -> false)
;;

let try_variants fn path =
  let continue =ref true in
  let test_variant (p,exts,wp,once,cnt) =
    if !continue && (try (p fn) with _ -> false) then
      begin
        continue:=cnt;
	let fp=if wp then (path^"/"^fn) else fn in
	List.iter
	  (fun ext -> let fext = fp^ext in if isreadable fext then raise (Found_texfile fext) else ())
	  exts;
	if once then raise No_texfile
      end
  in List.iter test_variant variants
;;

let fullpath_to_file fn paths = 
  match !havekpsewhich with
    true -> 
      (match string_command ("kpsewhich "^fn) 
       with 
	"" -> None
      |  s -> Some s)
  | false -> 
      (try
        List.iter (try_variants fn) paths; None
      with 
	No_texfile -> None
      | Found_texfile fn -> Some fn)
;;

exception TeXNotFound;;

(******
** texOpen -- resolve file name using TEXINPUTS
**            it calls kpsewhich if available on the system
******)

let texOpen fn =
    let path=fullpath_to_file fn !inputPaths in
    match path with
      None -> raise TeXNotFound
    | Some ffn -> 
	try 
	  if !fExport then Printf.printf "%% ***%sFULLNAME: %s\n" (dptstring !cfp) ffn;
          if !fDepend then Printf.printf "%s%s\n" (dptblanks !cfp) ffn;
	  (open_in ffn, ffn) 
	with Sys_error _ -> (prerr_endline ("error opening file: "^ffn); exit 1)

(******
** addInclude -- adds fn to the incList
**	
******)

let addInclude fn =
  if (!fFollow)
  then 
    begin
      incList := fn :: !incList;
      if !fExport then  Printf.printf "%% ***INCLUDEONLYFILE: %s\n" fn
    end;;

(******
** inList -- checks to see if fn is in the incList.  If the list is empty,
**	     all files are assumed to be "in the list".
******)

let inList fn = 
  match !incList with
    [] -> true
  | l  -> List.mem fn !incList;;

(******
** inputBibFile -- push the current yyin and open main.bbl.  If the open fails,
**	the bibliography file is ignored.
******)

let catfile fn =
  let inc = open_in fn in
  (* let nb = in_channel_length inc in *)
  try
    while true do output_string stdout (input_line inc); print_newline() done
  with End_of_file -> close_in inc

let inputBibFile() = 
	if !fFollow then
	  try
	    Printf.printf "%% ***%sEXPORTBEGS: \\bibliography{%s}\n" (dptstring (!cfp+1)) !bibMain;
            catfile !bibMain;
	    Printf.printf "%% ***%sEXPORTENDS: \\bibliography{%s}\n" (dptstring (!cfp+1)) !bibMain;
	  with _ -> prerr_endline("can't open \\bibliography file")

let getfile fn parser kind =
  if (!fFollow) then
      try 
        inc cfp;
        let (inch,fullfn) = texOpen fn in
        if List.exists (fun (_,oldfn,_) -> fullfn=oldfn) !fpstack
	then (prerr_endline (Printf.sprintf "ERROR: recursive includes through file %s!\n Aborting." fullfn); exit 1);
	fpstack:= (fn,fullfn,inch):: !fpstack; 
        curfn:= fullfn;
	let lexbuf = Lexing.from_channel inch
        in if (!fExport) then 
	  (Printf.printf "%% ***%sEXPORTBEGS: \\%s{%s}\n" (dptstring !cfp) kind fn);
           try 
	     while true do 
	       parser lexbuf
	     done
	   with Eof -> 
	     if (!fExport) then Printf.printf "%% ***%sEXPORTENDS: %s\n" (dptstring !cfp) fn;
	     (let (_,fullfn,_)::r = !fpstack in curfn:=fullfn;fpstack:=r; dec cfp; close_in inch)
      with _ -> prerr_endline ("Cannot open file: "^fn)

(******
** inputFile -- open fn and recurse on it.  If the open fails, then fn is ignored.
******)

let inputFile fn parser = getfile fn parser "input";;

(******
** includeFile -- if fn is in incList, open fn and recurse on it.  If the open fails, then fn is ignored.
******)

let includeFile fn parser =
  if (!incList=[] or (List.mem fn !incList)) then getfile fn parser "include";;

}

let S	   = [' ' '\t' '\n']*
let Spaces = [' ' '\t' '\n']+
let W	   = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '`' '^' '0'-'9' '-' '_']*

rule normal = parse
      |	"\\%"                                   {(* respect escaped % *) echo lexbuf; normal lexbuf;}
      | "%"[^'\n']*['\n']	                {(* ignore comments if required *)
                                                 if !fComments then echo lexbuf;} 
      | "\\include{"                            {(* process files *) paropen := !paropen + 1; laInc lexbuf; }
      | "\\include" [' ' '\t' '\n']             {(* process files *) laInc lexbuf; }
      |	"\\includeonly{" ([^'a'-'z' 'A'-'Z'])   {if !fExport then Printf.printf "%% ***EXPANDEDINCLUDEONLY\n";
                                                         paropen := !paropen + 1; incOnly lexbuf}
      |	"\\includeonly" [' ' '\t' '\n']		{if !fExport then Printf.printf "%% ***EXPANDEDINCLUDEONLY\n"; incOnly lexbuf}
      |	"\\input{"        			{paropen := !paropen + 1; input lexbuf; }
      |	"\\input" [' ' '\t' '\n']		{input lexbuf; }
      |	"\\bibliographystyle{"[^'}']*"}"	{if (!fBibExport) then Printf.printf "%% ***EXPANDEDBIBSTYLE: %s\n" (Lexing.lexeme lexbuf) else echo lexbuf}

      | "\\bibliography{" [^'}']* "}"	        {if (!fBibExport) then
	                                           begin 
						    Printf.printf "%% ***EXPANDEDBIBFILE: %s\n" (Lexing.lexeme lexbuf);
						    inputBibFile()
                                                   end
						 else echo lexbuf }
      | W                                       { echo lexbuf; if !fWord then print_char '\n' }
      | _                                       { if (not !fWord) then echo lexbuf}
      | eof                                     {raise Eof}

and laInc = parse
        S                                       { laInc lexbuf (*skip blanks up to file name *)}
      |  "{"      				{paropen := !paropen + 1;laInc lexbuf}
      |	[^ '{' ' ' '\t' '\n' '}' ]+		{includeFile (Lexing.lexeme lexbuf) normal;
						 nrmAfter lexbuf;
						}
      |	_					{laInc lexbuf}
      | eof                                     {prerr_endline ("Warning: end of file while parsing \\include command in "^ !curfn); raise Eof}

and incOnly = parse
	  "{"      				{paropen := !paropen + 1; incOnly lexbuf}
      |	[^ '{' ' ' '\t' ',' '\n' '}']+		{addInclude(Lexing.lexeme lexbuf);}
      |	"}"					{nrmAfter lexbuf;
						 if !fExport then Printf.printf "%% ***ENDEXPANDEDINCLUDEONLY\n"
						}
      |	"\n"					{newline ();incOnly lexbuf}
      |	_					{incOnly lexbuf}
      | eof                                     {prerr_endline ("Warning: end of file while parsing \\includeonly command in "^ !curfn); raise Eof}

and input = parse
      |	"{"      				{paropen := !paropen + 1; input lexbuf}
      |	[^ '{' ' ' '\t' '\n' '}' ]+		{inputFile (Lexing.lexeme lexbuf) normal;
						    nrmAfter lexbuf;
						}
      |	"\n"					{newline (); input lexbuf}
      |	_					{input lexbuf}
      | eof                                     {prerr_endline ("Warning: end of file while parsing \\input command in " ^ !curfn); raise Eof}

and nrmAfter = parse
	  "}"                                   {paropen:= !paropen - 1; if !paropen =0 then normal lexbuf else nrmAfter lexbuf}
      |	['\n']                                  {echo lexbuf; normal lexbuf;}
      |	S                                       {if !paropen = 0 then (echo lexbuf; normal lexbuf) else nrmAfter lexbuf}
      |	_                                       {echo lexbuf; normal lexbuf;}
      | eof                                     {if !paropen >0 then prerr_endline
	                                             (Printf.sprintf "Warning: end of file inside a group at level %d in %s\n"
							!paropen !curfn);
						 raise Eof}

{
(******
** main --
**	Set progName to the base of arg 0.
**	Set the input paths.
**	Check for options
**		-c		echo comments
**		-w		word only output
**		-x		keep trace of the expansions in the output file
**	Set the list of LaTeX environments to ignore.
**	Process each input file.
**	If no input files are specified on the command line, process stdin.
******)

let option_list = [
  ("-c", Arg.Set fComments,"echo comments in expanded file");
  ("-w", Arg.Set fWord,"word only output");
  ("-x", Arg.Set fExport,"keep trace of the expansions in the output");
  ("-b", Arg.Set fBibExport,"include bibliography file in the output")
];;

let usage_msg =
  Printf.sprintf "usage: %s [options] texfile ... " Sys.argv.(0);;

let process_tex_file opener fn =
  if !fExport then (Printf.printf "%% ***TOPLEVEL EXPORT\n";inc cfp);
    let lexbuf =   try
                    Lexing.from_channel (opener fn)
                   with 
                      TeXNotFound -> prerr_endline ("cannot find file: "^fn); exit 1
    in  
    try 
      while true do 
	normal lexbuf
      done
    with
      Eof -> ()
    |  _  -> prerr_endline (Printf.sprintf "parsing error around character %d in file %s" (Lexing.lexeme_end lexbuf) !curfn) ; exit 1

let _ = 
	(* get base name and decide what we are doing, texexpand or delatex *)
        let cmdname=Sys.argv.(0) in 
           progName:=
	      (try let i = String.rindex cmdname '/' in 
                String.sub cmdname (i+1) ((String.length cmdname) -i -1)
	       with _ -> cmdname);   

	(match !progName with
	| "texexpand" -> ()
	| "texdepend" -> fDepend:=true);

        (* see if we have kpsewhich *)
	tstkpsewhich ();

	(* set inputPaths for use with texOpen *)
	setInputPaths ();

	(* process command line options *)

        let texfiles = ref [] in
        let set_tex_filenames fn = texfiles := !texfiles@[fn] in
        Arg.parse option_list set_tex_filenames usage_msg;

        (* if necessary, output path to current working directory *)

	if !fExport then Printf.printf "%% ***CWD is: %s\n" (Sys.getenv "PWD");
        
	(* process input files *)

        List.iter (fun fn -> if !fBibExport then bibMain := makeBibName fn;process_tex_file (fun fn -> fst (texOpen fn)) fn) !texfiles;

	(* if there were no input files, assume stdin *)
	if (!texfiles=[] && !fBibExport) (* cannot handle bibliography export for stdin *)
                          then (prerr_endline "Cannot export bibliography from stdin ... Ignoring option -b"; fBibExport:=false);
	if !texfiles=[] then process_tex_file (fun _ -> stdin) "stdin";
	exit(0);;



}
