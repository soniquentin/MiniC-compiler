open Lexing
open Ast
open Tools

exception TypingError of string * (position * position)


(* stocke les identificateurs de variable et leur type.  *)
module VarState = Map.Make(String)


(* Le type env_decl définit les types d'éléments stockés dans l'environnement. 
   L'environnement est utilisé pour stocker les déclarations de variables, de structures et de fonctions. *)
type env_decl =
  | Env_var of typ
  | Env_struct of (env_decl VarState.t)
  | Env_fun of typ * typ list


(* Initialise VarState vide *)
let env = ref VarState.empty


(* retourne vrai si les types sont compatibles
   i.e si une expression d'un type peut être convertie en une expression de l'autre type. *)
let rec compare_type t1 t2 = match (t1, t2) with
  (* Même type sont compatible évidement *)
  | (TInt, TInt) | (Tnull, Tnull)  -> true
  | TStruct s, TStruct t when s = t -> true
  | TStar TVoid, TStar _ -> true
  | TStar _, TStar TVoid -> true
  | TStar typ1, TStar typ2 -> compare_type typ1 typ1
  (* Compatibilités croisées*)
  | (TInt, Tnull) | (Tnull, TInt)  -> true
  | Tnull, TStruct _ | TStruct _, Tnull -> true
  | TVoid, TStruct _ | TStruct _, TVoid -> true
  (* Le reste est incompatible*)
  | _, _ -> false



(* L'expression "lvalue" est utilisée en programmation pour 
    désigner une expression qui peut apparaître à gauche d'une affectation, 
    c'est-à-dire qui peut recevoir une valeur. 
    En d'autres termes, une lvalue est une expression qui identifie 
    un emplacement de mémoire où une valeur peut être stockée. *)
let l_value e = match e.desc_expr with
  | Eident _ | Eassign (_,_) -> true
  | _ -> false


(* vérifie si le type est bien formé
   i.e si tous les types de structures utilisés dans le type sont définis dans l'environnement. *)
let est_bien_forme gamma tau loc = match tau with
  | TStruct s -> begin try match VarState.find s gamma with
        | Env_struct _ -> ()
        | _ -> raise (TypingError ("struct " ^ s ^ " pas bien formé : pas une structure", loc) )
      with
        | Not_found -> raise (TypingError ("struct " ^ s ^ " pas bien formé : manquant dans l'environnement", loc) ) 
      end
  | _ -> ()



(* Typage des expressions*)
let rec type_expr gamma e = match e.desc_expr with
  | Eint 0 -> (TEint 0, Tnull)
  | Eint i -> (TEint i, TInt)
  | Eident x -> begin try match VarState.find x gamma with 
        | Env_var v -> (TEident x, v)   (* dans l'event, x est une variable. C'est bon *)
        | _ -> raise (TypingError (x ^ " : n'est pas un identifier/variable", e.loc) )  (*dans l'event, x est une fonction ou une structure (attention, pas une variable de fonction ou de structure !)*)
      with
        | Not_found -> raise (TypingError (x ^ " : variable non déclarée", e.loc) )  (* pas trouvé dans l'environnement *)
      end
  | Eassign (e1, e0) -> if not (l_value e1) then raise(TypingError ("cannot assign to a non left value", e.loc)) else
                        let tid, type_id = type_expr gamma e1 in let te0, typ_e0 = type_expr gamma e0 in if compare_type type_id typ_e0 then (TEassign (tid, te0), type_id) else raise (TypingError (string_of_expr e ^ " : types pas compatibles", e.loc) )
  | Eaccess (e0, x) -> begin let te0,typ_e0 = type_expr gamma e0 in match typ_e0 with  (* on cherche le type de e0. Pour l'instant, seul les strucutres peuvent supporter l'accès à un champs: e->s *)
        | TStruct s (* e0 est bien une structure de type s *) -> 
            begin try match VarState.find s gamma with (* on veut alors accéder à l'environnement de la structure *)
              | Env_struct env_of_s -> 
                begin try match VarState.find x env_of_s with (* On cherche x dans l'environnement de s *)
                    | Env_var typ_of_x (* est bien une variable de l'environnement de s*) -> (TEaccess (te0, x), typ_of_x) 
                    | _ -> raise (TypingError (x ^ " : n'est pas un identifier/variable pour la structure" , e.loc) )
                  with 
                    | Not_found -> raise (TypingError (x ^ " : n'est pas déclarée dans la structure" , e.loc) )
                end
              | _ -> raise (TypingError (string_of_expr e ^ " : doit être une structure" , e.loc) )
            with 
              | Not_found -> raise (TypingError (s ^ " : n'est pas déclarée dans l'environnement" , e.loc) )
          end
        | _ -> raise (TypingError (string_of_expr e ^ " : Access possible que pour les structures" , e.loc) )
        end
  | Ecall (id, args) -> begin try match VarState.find id gamma with
        | Env_fun (fun_type, args_type_list) -> 
            (* Introduit une fonction auxilère aux qui va vérifier que les arguments appelés correspondent à ce que la fonction est sensé prendre en arguments*)
            let rec aux accu env args_types params = begin match args_types, params with 
              | [] , [] -> List.rev accu, true
              | t1 :: q1 , t2 :: q2 -> let tt2, t2_type = type_expr env t2 in if compare_type t1 t2_type then aux (tt2 :: accu) env q1 q2 else [], false
              | _ , _ -> [], false
            end in begin match aux [] gamma args_type_list args with 
              | _, false -> raise (TypingError (id ^ " : erreur typage appelle fonction (pas le bon type ou pas bon nombres d'arguments)" , e.loc) )
              | texpr_list , _ -> (TEcall (id, texpr_list) , fun_type)
              end
        | _ -> raise (TypingError (string_of_expr e ^ " : n'est pas une fonction" , e.loc) )
      with 
        | Not_found ->raise (TypingError (string_of_expr e ^ " : fonction non déclarée" , e.loc) ) 
      end
  | Ebinop (op, e1, e2) -> begin let te1, e1_type = type_expr gamma e1 in let te2, e2_type = type_expr gamma e2 in match op with
        | Badd | Bsub | Bmul | Bdiv -> if (compare_type e1_type TInt) && (compare_type e2_type TInt) then ( TEbinop ( convert_binop op, te1, te2 ) , TInt) else raise (TypingError (string_of_expr e ^ " : impossible opération élémentaire entre les deux expressions" , e.loc) )
        | Beqq | Bneq | Blt | Ble | Bgt | Bge -> if compare_type e1_type e2_type then ( TEbinop ( convert_binop op, te1, te2 ) , TInt) else raise (TypingError (string_of_expr e ^ " : impossible opération comparaison entre les deux expressions" , e.loc) )
        | _ -> ( TEbinop ( convert_binop op, te1, te2 ) , TInt)
        end
  | Esizeof ct -> begin match ct with
        | Void -> raise (TypingError (string_of_expr e ^ " : cannot do sizeof(void)" , e.loc) )
        | Int | Struct s | Star ct2 -> (TEsizeof (convert_ctype ct), TInt)
        end
  | Eunop (Uneg, e) -> let te, e_type = type_expr gamma e in if compare_type e_type TInt then (TEunop (TUneg, te), TInt ) else raise (TypingError (string_of_expr e ^ " : expression peut pas être passée en négative" , e.loc) )
  | Eunop (Unot, e) -> let te, e_type = type_expr gamma e in (TEunop (TUnot, te), TInt)
    


