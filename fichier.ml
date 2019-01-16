open Prop_def;;

let order l = 
	let rec local liste = function  
                |[] -> liste  
                |(Var x)::tl  -> local ((Var x)::liste) tl 
                |hd::tl -> hd::(liste @ tl) 
    in local [] l
;;

let rec derivS gam  del = 
  match (order gam ,  order del) with 
  (gam', (IMPLIQ(a,b))::del')  -> 
  						print_string "->Droit \t";
    					print_theoreme([(gam,del)]);
    					(derivS (a::gam') (b::del'))
  |((IMPLIQ(a,b))::gam', del') -> 
  						print_string "->Gauche\t";
    					print_theoreme([(gam,del)]);
    					((derivS (gam')(a::del'))@(derivS (b::gam')(del')))
  |(gam', (NEG(a))::del')  	 -> 
  						print_string "~Droit  \t";
    					print_theoreme([(gam,del)]);
    					(derivS (a::gam') del')
  |((NEG a)::gam',del') 	 -> 
  						print_string "~Gauche \t"; 
  						print_theoreme([(gam,del)]);
    					(derivS (gam') (a::del'))
  |((ET(a,b))::gam', del')  -> 
  						print_string "&Gauche \t";
    					print_theoreme([(gam,del)]);
    					(derivS (a::b::gam') del')
  |(gam',(OU(a,b))::del')  	 -> 
  						print_string "#Droit  \t";
    					print_theoreme([(gam,del)]);
    					(derivS gam' (a::b::del'))
  |((OU(a,b))::gam',del')	 -> 
  						print_string "#Gauche \t";
    					print_theoreme([(gam,del)]);
    					((derivS (a::gam') del') @ (derivS (b::gam')(del')))
  |(gam',(ET(a,b))::del')   -> 
  						print_string "&Droit  \t";
    					print_theoreme([(gam,del)]);
    					((derivS (gam')(a::del'))@(derivS (gam')(b::del')))                             
  |(_,_) 					 ->
  						print_string"\t \t"; 
  						print_theoreme([(gam,del)]);[(gam,del)]
;;


let rec demontrer = function
                |[] -> true
                |(gam, del)::l' -> (demontrer1  gam del) && (demontrer l')
    and demontrer1 gam del =
              match gam with
                [] -> false
                |a::gam' -> demontrer2 a del || demontrer1 gam' del
    and demontrer2 a del =
              match del with
                [] -> false
                |(b::del')-> a=b || demontrer2 a del'
;;


let prouve_SEQUENCE p = 
        if demontrer(derivS [] [p]) then
			print_string " un theoreme. \n"
		else 
			print_string " n'est pas un theoreme. \n"
;;













































































let rec fnc  term =
	match term with
		(Var x) 		 ->  
						 print_string x
		|Vrai 			 ->      
						 print_string "vrai";
		|Faux  			 ->     
						 print_string "faux";
		|NEG Vrai 			 ->      
						 print_string "faux";
		|NEG Faux  			 ->     
						 print_string "vrai";
		|OU( (ET(x,y)), z) -> (
						 fnc( ET( OU(x,z), OU(y,z) ) );			
						)

		|NEG OU(x,y) 		 ->   (
						 fnc (ET( (NEG x), (NEG y) ) );
						)

		|NEG ET(x,y) 		 ->   (
						 fnc (OU( (NEG x), (NEG y) ) );
						)
    |NEG IMPLIQ(x,y) -> (
             fnc ( ET( x, (NEG y)) );
						)          
		|NEG NEG x			 -> ( 
						 fnc x;
						)
		|OU(x,y) 		 ->   (
             print_string "(";
             fnc x;
             print_string "#";
             fnc y;
             print_string ")"; 
						)             
		|ET(x,y)  	 ->	(
             print_string "("; 
             fnc x;
             print_string "&";
             fnc y;
             print_string ")"; 
						)             
		|IMPLIQ(x,y) -> (
             fnc ( OU( (NEG x), y) );
						)             
		|NEG x			 -> (
						 print_string "~"; 
						 fnc x;
						)
;;
