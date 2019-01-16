open Prop_def;;

(******************** recuprie 1er proposion (et/ou/neg) et envoye les var vers la fin de la list  *********************************)
let order l = 
	let rec local liste = function  
                |[] -> liste  
                |(Var x)::tl  -> local ((Var x)::liste) tl 
                |hd::tl -> hd::(liste @ tl) 
    in local [] l
;;

(*
  @param1 : proposition list
  @return : proposition list 
  val print_term : proposition list -> proposition list = <fun>
*)
(************** appliqueles rehgles de calcul de SEQUENCE *****************)
let rec appliqueTheoreme gauche  droit = 
  match (order gauche ,  order droit) with 
  |(gauche1, (IMPLIQ(a,b))::tl_droit)-> 
  						print_string "->Droit \t";
    					print_theoreme([(gauche,droit)]);
    					(appliqueTheoreme (a::gauche1) (b::tl_droit))

  |((IMPLIQ(a,b))::tl_gauche, droit1) -> 
  						print_string "->Gauche\t";
              print_theoreme([(gauche,droit)]);
    					((appliqueTheoreme (tl_gauche)(a::droit1))@(appliqueTheoreme (b::tl_gauche)(droit1)))
  
  |(gauche1, (NEG(a))::tl_droit)  	 -> 
  						print_string "~Droit  \t";
              print_theoreme([(gauche,droit)]);
    					(appliqueTheoreme (a::gauche1) tl_droit)
  
  |((NEG a)::tl_gauche, droit1) 	 -> 
  						print_string "~Gauche \t"; 
              print_theoreme([(gauche,droit)]);
    					(appliqueTheoreme (tl_gauche) (a::droit1))
  
  |((ET(a,b))::tl_gauche, droit1)  -> 
  						print_string "&Gauche \t";
              print_theoreme([(gauche,droit)]);
    					(appliqueTheoreme (a::b::tl_gauche) droit1)
  
  |(gauche1,(OU(a,b))::tl_droit)  	 -> 
  						print_string "#Droit  \t";
              print_theoreme([(gauche,droit)]); 
    					(appliqueTheoreme gauche1 (a::b::tl_droit))
  
  |((OU(a,b))::tl_gauche,droit1)	 -> 
  						print_string "#Gauche \t";
              print_theoreme([(gauche,droit)]);  (* nv arb *)
    					((appliqueTheoreme (a::tl_gauche) droit1) @ (appliqueTheoreme (b::tl_gauche) (droit1)))
  
  |(gauche1,(ET(a,b))::tl_droit)   -> 
  						print_string "&Droit  \t";
              print_theoreme([(gauche,droit)]); (* nv arb *)
    					((appliqueTheoreme (gauche1)(a::tl_droit))@(appliqueTheoreme (gauche1) (b::tl_droit)))                             
  
  |(_,_) 					 -> (** le rest : vrai - faux - Var x**)
  						print_string"\t \t"; 
  						print_theoreme([(gauche,droit)]);[(gauche,droit)]
;;

(*
  @param1 : proposition list
  @param2 : proposition list
  @return : (proposition list * proposition list) list
  return list des theoremes .... apres evaluation
val appliqueTheoreme :
  proposition list ->
  proposition list -> (proposition list * proposition list) list = <fun>
*)

(******************** verfie si les clause sont des theoremes **************************************)
let demontrer l= 
    let rec appartient_droit a droit =
              match droit with
                |[] -> false
                |hd::tl_droit -> a=hd || appartient_droit a tl_droit
    in let rec verfie_list gauche droit =
              match gauche with
                |[] -> false
                |hd::tl_gauche -> appartient_droit hd droit || verfie_list tl_gauche droit
    in let rec local = function
                |[] -> true
                |(gauche, droit)::tl -> (verfie_list gauche droit) && (local tl)
    in local l            
;;

(*
  @param1 : ('a list * 'a list) list
  @return : bool true si tout les clause  sont des theoremes sinon false 
  val demontrer : ('a list * 'a list) list -> bool = <fun>
*)
(******************** test prouve .. afficher theoreme ou non *********************************)    
let prouve_SEQUENCE p = 
    if demontrer(appliqueTheoreme [] [p]) then (* plasse theoreme a debut de proposition *)
			print_string " un theoreme ... \n"
		else 
			print_string " n'est pas un theoreme ... \n"
;;

(*
  @param1 : proposition
  @return : unit 
  val prouve_SEQUENCE : proposition -> unit = <fun>
*)









































































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
