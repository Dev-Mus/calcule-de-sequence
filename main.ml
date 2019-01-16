open Prop_def;;
open Fichier;;
 
open List;;
open Prop_lexer;;

let boucle in_channel =
	let lexbuffer = Lexing.from_channel in_channel in
			let cl = Prop_parser.programme Prop_lexer.token lexbuffer in 
				let k p = 			
			print_string "\nproposition de depart :\t"; print_term p;
			print_string "\n\n\t\t *** execution ***\n"; 
				prouve_SEQUENCE p; 
			print_string "\n\n";
		  		in 
					k cl ;
	exit 0;;

boucle stdin;;



