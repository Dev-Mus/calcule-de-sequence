exception EndSession;;
exception LexError;;
exception EOF;;

type proposition = 
Vrai
  |Faux
  |Var of string 
  |ET of proposition * proposition
  |NEG of proposition 
  |OU of proposition * proposition
  |IMPLIQ of proposition * proposition
;;


let lines = ref 0;;
let words = ref 0;;

(******************** afficher type proposiiton *********************************)
let rec print_term = function
      |Var x    ->    print_string x
      |Vrai       ->    print_string "vrai"
      |Faux       ->    print_string "faux"
      |OU(x,y)    ->   
                       print_string "(";
                       print_term x;
                       print_string "#";
                       print_term y;
                       print_string ")"; 
                   
      |ET(x,y)    ->  
                       print_string "("; 
                       print_term x;
                       print_string "&";
                       print_term y;
                       print_string ")"; 
                   
      |IMPLIQ(x,y)-> 
                       print_string "("; 
                       print_term x;
                       print_string "->";
                       print_term y;
                       print_string ")";                   
      |NEG x      -> 
                       print_string "~"; 
                       print_term x
;;

(*
  @param1 : proposition
  @return : unit 
  val print_term : proposition -> unit = <fun>
*)

(******************** afficher list des sequence de type proposiiton *********************************)
let rec print_sequence = function 
      |[]         ->   print_string " "; 
      |[hd]       ->   print_term hd; 
      |hd::tl     -> 
                       print_term hd; 
                       print_string ","; 
                       print_sequence tl 
;; 

(*
  @param1 : proposition list
  @return : unit 
  val print_term : proposition list -> unit = <fun>
*)

(******************** afficher la position de gauche theoreme droit  *********************************)
let rec print_theoreme = function
      |[]         ->  print_string " "
      |[(gauche, droit)]-> 
                      print_string"\t"; 
                      print_sequence gauche; 
                      print_string " |- "; 
                      print_sequence droit;
                      print_string "\n\n";
      |(gauche, droit)::tl ->  
                      print_string " \t"; 
                      print_sequence gauche; 
                      print_string " |- "; 
                      print_sequence droit;  
                      print_string "\n\t";
                      print_theoreme tl
;;

(*
  @param1 : proposition list * proposition list
  @return : unit 
  val print_term : proposition list * proposition list -> unit = <fun>
*)



