open Lexing
open Ast
open Utils

exception TypingError of string * (position * position)
exception Error of string


(* stocke les identificateurs de variable et leur type.  *)
module VarState = Map.Make(String)


(* Le type env_decl définit les types d'éléments stockés dans l'environnement. 
   L'environnement est utilisé pour stocker les déclarations de variables, de structures et de fonctions. *)
type env_decl =
  | Env_var of typ
  | Env_struct of (env_decl VarState.t)
  | Env_fun of typ * typ list


(* Initialise VarState vide *)
let my_env = ref VarState.empty

(* Dictionnaire pour garder en mémoire les structures qui ont été créées {ident : tstruct} *)
let all_structs = (Hashtbl.create 17 : (ident, tstruct) Hashtbl.t)

let rec convert_ctype = function
  | Void -> TVoid
  | Int -> TInt
  | Struct s -> TStruct (Hashtbl.find all_structs s)
  | Star ct -> TStar (convert_ctype ct)



(* ===== QUELQUES FONCTIONS AUXILIERES ======== *)

(* retourne vrai si les types sont compatibles
   i.e si une expression d'un type peut être convertie en une expression de l'autre type. *)
let rec compare_type t1 t2 = match (t1, t2) with
  (* Même type sont compatible évidement *)
  | (TInt, TInt) | (Tnull, Tnull)  -> true
  | TStruct s, TStruct t when s.name = t.name -> true
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
  | TStruct s -> begin try match VarState.find s.name gamma with
        | Env_struct _ -> ()
        | _ -> raise (TypingError ("struct " ^ s.name ^ " pas bien formé : pas une structure", loc) )
      with
        | Not_found -> raise (TypingError ("struct " ^ s.name ^ " pas bien formé : manquant dans l'environnement", loc) ) 
      end
  | _ -> ()



(* ====== TYPAGE ====== *)

(* Typage des expressions*)
let rec type_expr (gamma:env_decl VarState.t) (e:expr) = match e.desc_expr with
  | Eint 0l -> (TEint 0l, Tnull)
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
            begin try match VarState.find s.name gamma with (* on veut alors accéder à l'environnement de la structure *)
              | Env_struct env_of_s -> 
                begin try match VarState.find x env_of_s with (* On cherche x dans l'environnement de s *)
                    | Env_var typ_of_x (* est bien une variable de l'environnement de s*) -> let field_x = Hashtbl.find s.fields x in (TEaccess (te0, field_x), typ_of_x) 
                    | _ -> raise (TypingError (x ^ " : n'est pas un identifier/variable pour la structure" , e.loc) )
                  with 
                    | Not_found -> raise (TypingError (x ^ " : n'est pas déclarée dans la structure" , e.loc) )
                end
              | _ -> raise (TypingError (string_of_expr e ^ " : doit être une structure" , e.loc) )
            with 
              | Not_found -> raise (TypingError (s.name ^ " : n'est pas déclarée dans l'environnement" , e.loc) )
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
        | _ -> (TEsizeof (convert_ctype ct), TInt)
        end
  | Eunop (Uneg, e) -> let te, e_type = type_expr gamma e in if compare_type e_type TInt then (TEunop (TUneg, te), TInt ) else raise (TypingError (string_of_expr e ^ " : expression peut pas être passée en négative" , e.loc) )
  | Eunop (Unot, e) -> let te, e_type = type_expr gamma e in (TEunop (TUnot, te), TInt)
    

  
(* Typage des instructions *)
let rec type_desc_vars (gamma:env_decl VarState.t) (l:localisation) (d:decl_vars) = match d.desc_vars with
  | Decl_solo (ct, id, e) ->  begin try match VarState.find id gamma with
          | _ -> raise (TypingError ("Tentative de déclaration alors que variable " ^ id ^ " déjà existante" ,  l) )
        with
          | Not_found -> (* La variable n'a pas encore été déclarée dans l'environnement *)
              let te, type_e = type_expr gamma e in
              if (compare_type (convert_ctype ct) type_e) then (VarState.add id (Env_var type_e) gamma , TDecl_solo (type_e , id, te) ) else raise (TypingError ("Impossible de déclarer. Expression de type " ^ string_of_T type_e ^ ", type attendu " ^ string_of_type ct , l) )
        end
  | Decl_multi (ct, id_list) -> 
      let rec aux_add_to_env env_accu list_of_id = match list_of_id with
        | [] -> env_accu
        | t :: q -> begin try match VarState.find t env_accu with
            | _ -> raise (TypingError ("Tentative de déclaration alors que variable " ^ t ^ " déjà existante" ,  l) )
          with
            | Not_found -> aux_add_to_env (VarState.add t (Env_var (convert_ctype ct)) env_accu) q (* La variable n'a pas encore été déclarée dans l'environnement *)
          end
      in (aux_add_to_env gamma id_list , TDecl_multi (convert_ctype ct, id_list) )


let type_decl_typ (gamma:env_decl VarState.t) (l:localisation) (d:decl_typ) = let id, d_list = d.desc_typ in 
    let new_struct = {name = id; fields = (Hashtbl.create 17 : (ident,field) Hashtbl.t)} in 
    let rec aux struct_env_accu loca li num = begin match li with 
      | [] -> struct_env_accu
      | t :: q -> begin match t.desc_vars with 
                  | Decl_solo (ct, id, e) -> begin try match VarState.find id struct_env_accu with
                                                | _ -> raise (TypingError ("Tentative de déclaration alors que variable " ^ id ^ " déjà existante" ,  l) )
                                              with
                                                | Not_found -> (* La variable n'a pas encore été déclarée dans l'environnement *)
                                                    let te, type_e = type_expr struct_env_accu e in 
                                                    if (compare_type (convert_ctype ct) type_e) then let new_env = VarState.add id (Env_var type_e) struct_env_accu in Hashtbl.add new_struct.fields id {field_typ = (convert_ctype ct); field_pos = num} ; aux new_env loca q (num + 1)
                                                    else raise (TypingError ("Impossible de déclarer. Expression de type " ^ string_of_T type_e ^ ", type attendu " ^ string_of_type ct , l) )
                                                end
                  | _ -> raise (TypingError ("Définition d'une strucutre qu'avec des lignes de la formes 'int a = 2;' (i.e Decl_solo)", l) )
                end
      end
    in let struct_env = aux gamma [] d_list 0 in Hashtbl.add all_structs id new_struct; (VarState.add id (Env_struct struct_env) gamma, new_struct)



let rec type_instr (gamma:env_decl VarState.t) (tau_0:typ) (instr:decl_instr) = match instr.instruction with 
  | Inone -> Tnone 
  | Iexpr e -> let te, type_e = type_expr gamma e in Texpr te  (* ça check en même temps le type de e *)
  | Iif (e, i) -> let te, type_e = type_expr gamma e in if compare_type type_e TVoid then raise(TypingError ("La condition d'un If doit être un entier", instr.loc)) else let ti = type_instr gamma tau_0 i in Tif (te,ti)
  | Iifelse (e, i1, i2) -> let te, type_e = type_expr gamma e in if compare_type type_e TVoid then raise(TypingError ("La condition d'un If doit être un entier", instr.loc)) else let ti1 = type_instr gamma tau_0 i1 in let ti2 = type_instr gamma tau_0 i2 in Tifelse(te, ti1, ti2)
  | Iwhile (e, i) -> let te, type_e = type_expr gamma e in if compare_type type_e TVoid then raise(TypingError ("La condition d'un While doit être un entier", instr.loc)) else let ti = type_instr gamma tau_0 i in Twhile (te, ti)
  | Iret None -> if tau_0 == TVoid then Tret (None) else raise(TypingError ("Fonction void retourne un objet non-Void", instr.loc) )
  | Iret (Some e) -> let te, type_e = type_expr gamma e in Tret (Some te)
  | Iblock b -> let tb, final_env = type_block gamma instr.loc tau_0 b in Tblock tb
and type_block env loc t0 bloc = 
  let rec aux_type_block accu envi loca tau0 block = match block with
    | [] -> (List.rev accu, envi)
    | d :: q -> begin match d with 
          | Dvar d_vars -> let new_env, type_d = type_desc_vars envi loca d_vars in aux_type_block ( (TDvar type_d) :: accu) new_env loca tau0 q
          | Dtyp d_typ -> let new_env, type_d = type_decl_typ envi loca d_typ in aux_type_block ( (TDtyp type_d) :: accu) new_env loca tau0 q
          | Dfct d_fct -> let new_env, type_d = type_decl_fct envi loca tau0 d_fct in aux_type_block ( (TDfct type_d) :: accu) new_env loca tau0 q
          | Decl_instr d_instr -> let type_d = type_instr envi tau0 d_instr in aux_type_block ( (TDecl_instr type_d) :: accu) envi loca tau0 q
        end
  in aux_type_block [] env loc t0 bloc
and type_decl_fct (gamma:env_decl VarState.t) (l:localisation) (t0:typ) (d:decl_fct) = let ct, id, param_list, body = d.desc_fct in
    begin try match VarState.find id gamma with
        | _ -> raise (TypingError ("Tentative de déclaration de fonction alors que variable " ^ id ^ " déjà existante" ,  l) )
      with
        | Not_found -> (* La variable qui porte le nom de la fonction n'a pas encore été déclarée dans l'environnement *)
          let tbody, env = type_block gamma l t0 body in 
          (VarState.add id (Env_fun (convert_ctype ct, List.map (fun (c, id) -> convert_ctype c) param_list)) gamma,      ( convert_ctype ct, id, List.map (fun (c, id) -> (convert_ctype c, id ) ) param_list, tbody)  )
      end





(* ====== Build file ====== *)

(* Initialise l'environnement en ajoutant les 2 fonctions qui sont connues par défaut *)
let init_env () =
  let temp_env1 = VarState.add "putchar" (Env_fun (TInt, [TInt]) ) !my_env in let temp_env2 = VarState.add "malloc" (Env_fun (TStar TVoid, [TInt]) ) temp_env1 in my_env := temp_env2


(* Check si la fonction main est bien présente et bien typée *)
let check_main_exists (env:env_decl VarState.t) = match VarState.find "main" env with 
  | Env_fun (TInt, _ ) -> ()
  | Env_fun (_,_) ->  raise (Error ("La fonction main n'est pas de type Int") )
  | _ -> raise (Error ("Aucune fonction main trouvée") )


(* Construit le fichier final *)
(* Ce truc va traiter le fichier comme un gros block et ça va renvoyer l'environnement final. Puis on check si y'a bien une focnction main *)
let type_fichier (d:fichier) = begin
  init_env (); 
  let tdecl_list, final_env = type_block !my_env (d.loc) TVoid (d.decl_list) in my_env := final_env; check_main_exists !my_env; 
  tdecl_list;
  end