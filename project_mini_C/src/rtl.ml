(* Programme qui transforme l'arbre astrait en un arbre de type RTLTree *)

open Rtltree
open Utils

exception Error of string

(* Initialise le créer un dictionnaire vide *)
let graph = ref Label.M.empty

(* Ajoute un nouveau label dans le dictionnaire *)
let add_label_in_graph (i:instr) =
  let new_label = Label.fresh () in graph := Label.M.add new_label i !graph; new_label


(* Dictionnaire pour garder les fonctions créées {ident : [info sur les fonctions]} *)
let all_fun = (Hashtbl.create 17 : (Ast.tident, deffun) Hashtbl.t)


(* On traduit les expressions *)
(* NB : r_env, c'est l'environnement des registres. Autrement dit, un dictionnaire {ident : register} *)
let rec translate_expr (e:Ast.texpr) (rd:register) (ld:label) (r_env:(string,register) Hashtbl.t) = match e with 
  | Ast.TEint n -> add_label_in_graph (Econst (n, rd, ld)) (* Int32.of_int n *)
  | Ast.TEident id -> let r_id = Hashtbl.find r_env id in add_label_in_graph (Embinop (Ops.Mmov, r_id, rd, ld))
  | Ast.TEunop (uop, te) -> begin match uop with 
        | Ast.TUneg -> (*   L_{d-1} := Sub(#r_zero , #rd ) -> Ld
                            L_{d-2} := E_const 0, #r_zero -> L_{d-1}
                            L_{d-3} :=   translate_expr te #rd  -> L_{d-2} 
                            Renvoie L_{d-3} *)
                      let r_zero = Register.fresh() in let l_d_1 = add_label_in_graph (Embinop (Ops.Msub, r_zero, rd, ld)) in 
                      let l_d_2 = add_label_in_graph (Econst (Int32.zero, r_zero, l_d_1)) in 
                      translate_expr te rd l_d_2 r_env
        | Ast.TUnot -> let l_d_1 = add_label_in_graph (Emunop ((Ops.Msetei Int32.zero), rd, ld)) in translate_expr te rd l_d_1 r_env
      end
  | Ast.TEbinop (biop, te1, te2) -> begin match biop with
                (* L_{d-1} := BIOP(#r_temp , #rd ) -> Ld
                L_{d-2} := translate_expr te2 #rd  -> L_{d-1} 
                L_{d-3} := translate_expr te1 #r_temp  -> L_{d-2} 
                Renvoie L_{d-3}      *)
        | (Ast.TBadd|Ast.TBdiv|Ast.TBsub|Ast.TBeqq|Ast.TBneq|Ast.TBge|Ast.TBgt|Ast.TBle|Ast.TBlt) -> let r_temp = Register.fresh() in let l_d_1 = add_label_in_graph (Embinop  ( tast_to_ops biop, r_temp, rd, ld )) in
                                                                                                    let l_d_2 = translate_expr te2 rd l_d_1 r_env in
                                                                                                    translate_expr te1 r_temp l_d_2 r_env
        | Ast.TBmul (* Cas particulier : on veut simplifier. 0 x e = 0 et 1 x e = e *) -> begin match te1, te2 with
                | Ast.TEint 0l, _ |_, Ast.TEint  0l -> add_label_in_graph (Econst (Int32.zero, rd, ld))
                | Ast.TEint 1l, _ -> translate_expr te2 rd ld r_env
                | _, Ast.TEint 1l -> translate_expr te1 rd ld r_env
                | _ (* Sinon, aucune simplification -> binop normal *) -> let r_temp = Register.fresh() in let l_d_1 = add_label_in_graph (Embinop  ( tast_to_ops biop, r_temp, rd, ld )) in
                                                                          let l_d_2 = translate_expr te2 rd l_d_1 r_env in
                                                                          translate_expr te1 r_temp l_d_2 r_env
              end
        | Ast.TBand -> (* On va faire des égalités:
                        rd != 0 -> rd
                        rd == r_temp -> rd
                        rd = te1
                        r_temp = te2 -> r_temp *) 
                        let r_zero = Register.fresh() in let r_temp = Register.fresh() in let l_d_1 = add_label_in_graph (Embinop  (Ops.Msete, r_zero, rd, ld )) in
                        let l_d_2 = add_label_in_graph (Econst (Int32.zero, r_zero, l_d_1)) in 
                        let l_d_3 = add_label_in_graph (Embinop  (Ops.Msete, r_temp, rd, l_d_2 )) in 
                        let l_d_4 = translate_expr te1 r_temp l_d_3 r_env in
                        translate_expr te2 rd l_d_4 r_env
        | Ast.TBor ->  (* On va faire des égalités:
                        rd == 0 -> rd
                        r_temp == 0 -> r_temp
                        te2 -> rd
                        te1 -> r_temp *) 
                        raise (Error "TODO")
      end
  | Ast.TEsizeof t -> begin match t with 
      | Ast.TInt | Ast.TStar _  -> add_label_in_graph (Econst (Int32.of_int 8, rd, ld))
      | Ast.TVoid -> add_label_in_graph (Econst (Int32.zero, rd, ld))
      | Ast.TStruct id_struct -> add_label_in_graph (Econst (Int32.of_int (8* (Hashtbl.length id_struct.fields)), rd, ld))
      | Ast.Tnull -> raise (Error "Impossible de savoir la taille d'un objet de type null")
    end
  | Ast.TEassign (t_assigned, te) -> begin match t_assigned with (* On va déjà checker que te est une valeur à gauche *)
      | Ast.TEident id -> begin try let r_id = Hashtbl.find r_env id in
                            let l_d_1 = add_label_in_graph (Embinop (Ops.Mmov, r_id, rd, ld)) in translate_expr te r_id l_d_1 r_env
                        with Not_found -> raise (Error ("Variable "^id^" n'a pas encore été défini."))
                      end
      | Ast.TEaccess (te_struct, field_x) -> 
                                      (* 
                                      l_{d-1} : Mov (r_expr, rd)    ==>  l_d 
                                      l_{d-2} : Estore (r_expr, r_struct, 8*pos)    ==>  l_{d-1} 
                                      l_{d-3} : te -> r_expr             ==>  l_{d-2} 
                                      l_{d-4} : te_struct -> r_struct    ==>  l_{d-3}
                                      *)
                                      let pos = field_x.Ast.field_pos in let r_struct = Register.fresh() in let r_expr = Register.fresh() in
                                      let l_d_1 = add_label_in_graph (Embinop (Ops.Mmov, r_expr, rd, ld)) in
                                      let l_d_2 = add_label_in_graph (Estore (r_expr, r_struct, 8*pos, l_d_1)) in
                                      let l_d_3 = translate_expr te r_expr l_d_2 r_env in 
                                      let l_d_4 = translate_expr te_struct r_struct l_d_3 r_env in l_d_4
      | _ -> raise (Error "On ne peut assigner une valeur qu'à une valeur à gauche")
    end
  | Ast.TEaccess (te, field_x) -> let pos = field_x.Ast.field_pos in let r_temp = Register.fresh() in
                                                        let l_d_1 = add_label_in_graph (Eload (r_temp, 8*pos, rd, ld)) in
                                                        let l_d_2 = translate_expr te r_temp l_d_1 r_env in l_d_2
  | Ast.TEcall (id, e_list) ->
                              (*
                              l_{d-1} : Call rd <- f(r_1,r_2,...,r_p)  ==> l_d
                              l_{d-2} : e1 -> r_1 ==> l_{d-1}
                              ...
                              l_{d-p-1} : ep -> r_p ==> l_{d-p} 
                              *)  
                              begin try let f_info = Hashtbl.find all_fun id in 
                                let rec gen_p_reg accu = function (* Créer la liste r_1, ..., r_p *)
                                  | [] -> List.rev accu
                                  | t :: q -> gen_p_reg (Register.fresh() :: accu) q in
                                let list_of_p_registers = gen_p_reg [] f_info.fun_formals in let l_d_1 = add_label_in_graph (Ecall (rd, id, list_of_p_registers, ld)) in 
                                let rec store_all_expr expr_list reg_list final_l = begin match expr_list, reg_list with 
                                    | [],[] -> final_l
                                    | e1::q1 , r2::q2 -> let next_l = store_all_expr q1 q2 final_l in translate_expr e1 r2 next_l r_env
                                    | [] , _ | _, [] ->  raise (Error ("Nombre d'arguments passés dans la fonction "^id^" Incorrect"))
                                  end in store_all_expr e_list list_of_p_registers l_d_1
                                with Not_found -> raise (Error ("Fonction "^id^" n'a pas encore été définie."))
                              end


(* On traduit les instructions *)
and translate_instr (i:Ast.tdecl) (rd:register) (ld:label) (rret:register) (r_env:(string,register) Hashtbl.t)  = match i with
    | Ast.TDvar td_vars -> translate_decl_vars td_vars rd ld r_env
    | Ast.TDtyp td_typ -> add_label_in_graph (Egoto ld) (* Rien à faire *)
    | Ast.TDfct td_fct ->   translate_decl_fct td_fct rd ld r_env
    | Ast.TDecl_instr td_instr -> translate_decl_instr td_instr rd ld rret r_env
and translate_decl_instr (x:Ast.tdecl_instr) (rd:register) (ld:label) (rret:register) (r_env:(string,register) Hashtbl.t) = begin match x with
      | Ast.Tnone -> add_label_in_graph (Egoto ld)
      | Ast.Texpr te -> translate_expr te rd ld r_env
      | Ast.Tblock tb -> translate_block tb rd ld rret r_env 
      | Ast.Tifelse (te, t_instr1, t_instr2) -> (* l_true := instruction t_instr1 -> Ld
                                                    l_false := instruction t_instr1 -> Ld
                                                    l_d_2 := r_temp vaut 0 -> l_false, sinon l_true
                                                    l_d_3 := te -> r_temp  -> l_d_2
                                                *)
                                                let l_true = translate_decl_instr (t_instr1) rd ld rret r_env in
                                                let l_false = translate_decl_instr t_instr2 rd ld rret r_env in 
                                                let r_temp = Register.fresh() in let l_d_2 = add_label_in_graph (Emubranch (Ops.Mjnz, r_temp, l_true, l_false)) in 
                                                translate_expr te r_temp l_d_2 r_env
      | Ast.Tif (te, t_instr) -> translate_decl_instr (Ast.Tifelse (te, t_instr, Ast.Tnone)) rd ld rret r_env
      | Ast.Twhile (te, t_instr) ->   let l_d_2  = Label.fresh() in let r_temp = Register.fresh() in
                                      let test_e = translate_expr te r_temp l_d_2 r_env in
                                      let l_true = translate_decl_instr t_instr rd l_d_2 rret r_env in
                                      let l_false = add_label_in_graph (Egoto ld) in 
                                      graph := Label.M.add l_d_2 (Emubranch (Ops.Mjnz, r_temp, l_true, l_false)) !graph; test_e
      | Tret None -> add_label_in_graph (Egoto ld)
      | Tret (Some te) -> translate_expr te rret ld r_env
    end
and translate_block (tb:Ast.tblock) (rd:register) (ld:label) (rret:register) (r_env:(string,register) Hashtbl.t) = let rec clean_vars_space var_list =  begin match var_list with (* Fonction pour retirer toutes les variables définies dans le block *)
      | [] -> ()
      | t :: q -> Hashtbl.remove r_env t;
      end in
      let update_def_var (var:Ast.tdecl_vars) (def_var:Ast.tident list) = begin match var with (* Fonction pour ajouter une (ou plusieurs variables selon decl_solo ou decl_multi) dans la liste des variables définies dans le block*)
          | TDecl_solo (_,id,_) -> id :: def_var
          | TDecl_multi(_,id_list) -> let rec concat l1 l2 = begin match l1 with 
                                        | [] -> l2
                                        | t :: q -> concat q (t :: l2)
                                      end in concat (List.rev id_list) def_var
          end in
      let rec aux reverse_tdecl_list rd_next_instr ld_next_instr def_var = begin match reverse_tdecl_list with (* On parcourt la liste d'instructions du block*) (* def_var track les variables définies*)
          | [] -> ld_next_instr, def_var
          | Ast.TDecl_instr t :: q -> let r_temp = Register.fresh() in let l_entrance = translate_decl_instr t rd_next_instr ld_next_instr rret r_env in aux q r_temp l_entrance def_var
          | Ast.TDvar t :: q -> let r_temp = Register.fresh() in let l_entrance = translate_decl_vars t rd_next_instr ld_next_instr r_env in aux q r_temp l_entrance (update_def_var t def_var)
          | _ :: q -> raise (Error "Chelou de définir une fonction ou une structure dans une block...")
          end
      in let l_d_1, defined_var = aux (List.rev tb) rd ld [] in clean_vars_space defined_var; l_d_1
and translate_decl_vars (x:Ast.tdecl_vars) (rd:register) (ld:label) (r_env:(string,register) Hashtbl.t) = match x with
      | TDecl_solo (t, tid, te) -> let r_id = Register.fresh() in begin match t with
            (* POUR INT
              L_{d-1} := Mov r_id rd -> L_d
              L_{d-2} := te -> r_id     -> L_{d-1}
            *)
            | TInt -> Hashtbl.add r_env tid rd ; let l_d_1 = add_label_in_graph (Embinop (Ops.Mmov, r_id, rd, ld)) in translate_expr te r_id l_d_1 r_env
            | _ ->  raise (Error "Cannot declare something else than int")
          end
      | TDecl_multi (t, id_list) -> begin match t with 
            | TInt -> let rec aux remain_id_to_treat = begin match remain_id_to_treat with 
            (* Il n'y pas vraiment d'instructions ici, on ajoute juste les id au dictionnaire des registres *)
                  | [] -> add_label_in_graph (Egoto ld)
                  | [t] -> Hashtbl.add r_env t rd; aux []
                  | t :: q -> let r_id = Register.fresh() in Hashtbl.add r_env t r_id; aux q
                end in aux (List.rev id_list)
            | _ ->  raise (Error "Cannot declare something else than int")
          end
and translate_decl_fct (x:Ast.tdecl_fct) (rd:register) (ld:label) (r_env:(string,register) Hashtbl.t) = let typ, id_fun, args_list, body = x in 
          let rec add_args_to_env accu = function (* Ajoute les arguments à l'environnement et renvoie la liste des pseudo-registres : chaque argument = registre frais *)
              | [] -> List.rev accu
              | (_, id) :: q -> let arg_r = Register.fresh() in Hashtbl.add r_env id arg_r; add_args_to_env (arg_r :: accu) q
          in let return_r = Register.fresh() in let exit_label = Label.fresh() in 
          let f_info = {
            fun_name   = id_fun;
            fun_formals= add_args_to_env [] args_list (* liste des registres pour chaque arguments *);
            fun_result = return_r ; (* registre avec valeur de renvoyée *)
            fun_locals = Register.set_of_list ([]);
            (** toutes les variables locales de la fonction maintenant regroupées ici *)
            fun_entry  = translate_block body rd exit_label return_r r_env;
            fun_exit   = exit_label;
            fun_body   = !graph;
          } in 
          Hashtbl.add all_fun id_fun f_info; ld (* Renvoie ld parce que la définition d'une fonction ne fait rien *)



(* On traduit le programme *)
and program (p:Ast.tfichier) = let r_env = (Hashtbl.create 17 : (string,register) Hashtbl.t) in let final_r = Register.fresh() in let final_l = Label.fresh() in
    let rec aux r_next l_next = function 
          | [] -> ()
          | t :: q -> let r_temp =  Register.fresh() in let prev_l = translate_instr t r_next l_next r_next r_env in aux r_temp prev_l q
    in aux final_r final_l (List.rev p); 
    let all_fun_info = ref [] in Hashtbl.iter (fun _ v -> all_fun_info := v :: !all_fun_info) all_fun; { funs = List.rev !all_fun_info} (* On loop sur la hashtable pour récupérer toutes ses valeurs qui sont ici les infos des fonctions *)

