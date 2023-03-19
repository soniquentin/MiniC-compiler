open Ertltree
open Ops
open Register

exception Error of string

(* ======= RLT TO ERTL =======  *)
(* Initialise le créer un dictionnaire vide *)
let graph = ref Label.M.empty

(* Ajoute un nouveau label dans le dictionnaire *)
let add_label_in_graph (i:instr) =
  let new_label = Label.fresh () in graph := Label.M.add new_label i !graph; new_label

(* Donne équivalents instructions rtl en instructions erl : excetpion pour la division *)
let instr = function
  | Rtltree.Econst (n, r, l) -> Econst (n, r, l)
  | Rtltree.Eload (r1, n, r2, l) -> Eload (r1, n, r2, l)
  | Rtltree.Estore (r1, r2, n, l) -> Estore (r1, r2, n, l)
  | Rtltree.Emunop  (m, r, l) -> Emunop (m, r, l)
  | Rtltree.Embinop (Mdiv, r1, r2, l) -> let l_d_1 = add_label_in_graph (Embinop (Mmov, Register.rax, r2 , l)) in
                                         let l_d_2 = add_label_in_graph (Embinop (Mdiv, r1, Register.rax, l_d_1)) in Embinop (Mmov, r2, Register.rax, l_d_2)
  | Rtltree.Embinop (m, r1, r2, l) -> Embinop (m, r1, r2, l)
  | Rtltree.Emubranch (m, r, l1, l2) -> Emubranch (m, r, l1, l2)
  | Rtltree.Embbranch (m, r1, r2, l1, l2) -> Embbranch (m, r1, r2, l1, l2)
  | Rtltree.Ecall (r, f, rl, l) -> let nb_args = List.length rl in let nb_args_in_pass_args = min 6 nb_args in 
                                  let rec pass_args register_parameters register_list next_l = begin match register_parameters, register_list with  (* Fonction pour passer les premiers 6 arguments dans Register.parameters et le reste sur la pile *)
                                    | _, [] -> next_l
                                    | [], t :: q -> let l_d_1 = add_label_in_graph (Epush_param (r, next_l)) in pass_args [] q l_d_1
                                    | r1 :: p, t :: q -> let l_d_1 = add_label_in_graph (Embinop (Mmov, r, r1, next_l)) in pass_args p q l_d_1
                                  end in 
                                  (*
                                    l_d_1 : déplier les 7ème, 8ème, ... arguments qui ont été rajoutés sur la pile ==> l_d
                                    l_d_2 : Mov r rax ==> l_d_1
                                    l_d_3 : r <- call ==> l_d_2
                                    l_d_4 : Passe les arguments en rdi, rsi, ... ou sur la pile ==> l_d_3
                                  *)
                                  let l_d_1 = if nb_args > 6 then add_label_in_graph (Emunop (Maddi ( Int32.of_int ((nb_args - 6)*8) ), Register.rsp, l)) else l in
                                  let l_d_2 = add_label_in_graph (Embinop(Mmov, Register.result, r, l_d_1) ) in
                                  let l_d_3 = add_label_in_graph (Ecall (f, nb_args_in_pass_args, l_d_2)) in
                                  let l_d_4 = pass_args Register.parameters rl l_d_3 in 
                                  Egoto l_d_4
  | Rtltree.Egoto l -> Egoto l



(* Convertit les instructions rtl en instructions erl *)
(* Reçoit une instruction RTL :   rtl_l : i --> next_l *)
let rtl_to_ertl_instr rtl_l rtl_i = let ertl_i = instr rtl_i in graph := Label.M.add rtl_l ertl_i !graph


(* Convertit les fonctions : consiste à ajouter des instructions avant et après les instructions RTL de la fonction *)
let translate_fct (f:Rtltree.deffun) = Label.M.iter rtl_to_ertl_instr f.Rtltree.fun_body ; (* Convertit chaque instruction RTL en appliquant la fonction de convertion sur le dictionnaire Label.M = {étiquette : instruction RTL} *)
  let nb_callee_saved = List.length Register.callee_saved in
  let rec create_n_rfresh accu = function (* Créer une liste de nouveaux registres pour sauvegarder les callee_saved *)
    | 0 -> accu
    | n -> create_n_rfresh (Register.fresh()::accu) (n-1)
  in let callee_saved_storage = create_n_rfresh [] nb_callee_saved in 

  (* SORTIE *)
  (* l_d_1 : Ereturn
     l_d_2 : Edelete_frame ==> l_d_1
     l_d_3 : restaurer les registres callee-saved  ==> l_d_2
     l_exit : copier le résultat de la fonction dans Register.rax ==> l_d_3
  *)
  let l_d_1 = add_label_in_graph (Ereturn) in
  let l_d_2 = add_label_in_graph (Edelete_frame l_d_1) in
  let rec aux_restore_cs storage cs next_l = begin match storage, cs with
    | [], [] -> next_l
    | r_store :: q1, r_callee_saved :: q2 -> let l_temp = add_label_in_graph (Embinop (Mmov, r_store, r_callee_saved, next_l)) in aux_restore_cs q1 q2 l_temp
    | _, _ -> raise (Error "List.length ne marche pas ?? Chelou là...")
  end in let l_d_3 = aux_restore_cs callee_saved_storage Register.callee_saved l_d_2 in
  let i_exit = Embinop (Mmov, f.Rtltree.fun_result, Register.result, l_d_3) in graph := Label.M.add f.Rtltree.fun_exit i_exit !graph; (* l_exit existait pas dans le graphe ! On prolonge le graphe de flow de la fonction *)


  (* ENTREE *)
  (* l_k_1 : récupérer les arguments
     l_k_2 : sauvegarder les registres callee-saved ==> l_k_1
     l_k_3 : allouer le tableau d'activation ==> l_k_2
  *) 
  let rec get_args register_parameters register_list next_l count = begin match register_parameters, register_list with  (* C'est l'homologue de pass_args *)
    | _, [] -> next_l
    | [], t :: q -> let l_temp = add_label_in_graph (Eget_param (count*8 + 16, t, next_l)) in get_args [] q l_temp (count + 1)
    | r1 :: p, t :: q -> let l_temp = add_label_in_graph (Embinop (Mmov, t, r1, next_l)) in get_args p q l_temp count
  end in let l_k_1 = get_args Register.parameters f.fun_formals f.Rtltree.fun_entry 1 in 
  let rec aux_store_cs storage cs next_l = begin match storage, cs with
    | [], [] -> next_l
    | r_store :: q1, r_callee_saved :: q2 -> let l_temp = add_label_in_graph (Embinop (Mmov, r_callee_saved, r_store,  next_l)) in aux_store_cs q1 q2 l_temp
    | _, _ -> raise (Error "List.length ne marche pas ?? Chelou là...")
  end in let l_k_2 = aux_store_cs callee_saved_storage Register.callee_saved l_k_1 in
  let l_k_3 = add_label_in_graph (Ealloc_frame l_k_2) in 


  {
  fun_name = f.Rtltree.fun_name;
  fun_formals = List.length f.Rtltree.fun_formals; (* nb total d'arguments *)
  fun_locals = f.Rtltree.fun_locals;
  fun_entry = l_k_3;
  fun_body = !graph;
  }


(*  ======= ANALYSE DE DUREE DE VIE =======  *)
(* Table de hashage qui associe chaque label à son live_info : utile pendant toute l'analyse de durée de vie*)
let live_info_tab = (Hashtbl.create 32 : (label, live_info) Hashtbl.t)

(* L'exo veut qu'on rende un labelmap *)
let live_info_map = ref Label.M.empty

(* Fonction pour initialiser tout les live info de live_info_dict --> reste encore à modifie pred !*)
let init_live_info (graph_map:cfg) = 
    let init_solo l i = 
      let defs_i, uses_i = Ertltree.def_use i in 
      let new_live_info_to_add = {
                                  instr = i;
                                  succ = Ertltree.succ i;    
                                  pred = Label.S.empty;
                                  defs = Register.set_of_list defs_i;    
                                  uses = Register.set_of_list uses_i;    
                                  ins = Register.S.empty; 
                                  outs = Register.S.empty; 
                                } in
      Hashtbl.add live_info_tab l new_live_info_to_add; ()
    in Label.M.iter init_solo graph_map

(* Modifie pred: pour chaque étiquette l, regarde la liste des successeurs et à chacun de ces successeurs, ajoute l'étiquette l comme pred  *)
let modify_pred() = 
    let update_solo l live_info_l = 
      let rec loop_on_succ successeur_list = begin match successeur_list with (* Permet de looper sur la liste des successeurs *)
        | [] -> ()
        | t :: q -> if Hashtbl.mem live_info_tab t (* vérifie si live_info_tab contient déjà des informations pour l'étiquette t *)
                    then let one_succ = Hashtbl.find live_info_tab t in one_succ.pred <- Label.S.add l one_succ.pred (* Si oui, ajoute l comme pred de ce successeur *); 
                    loop_on_succ q
      end in loop_on_succ live_info_l.succ; () (* Fin fonction update_solo *)
    in Hashtbl.iter update_solo live_info_tab; ()


(* Algo de Kildall *)
let ws = ref Label.S.empty
let kildall() = 
  (*initialise WS avec tous les sommets (i.e étiquette) *)
  Hashtbl.iter (fun l live_info_l -> (ws := Label.S.add l !ws) ) live_info_tab;

  while (not (Label.S.is_empty !ws)) do
    (* extraire un sommet l de WS *)
    let l = Label.S.choose !ws in ws := Label.S.remove l !ws; 

    (* old_in <- in(l)*)
    let live_info_l = Hashtbl.find live_info_tab l in let old_in = live_info_l.ins in 

    (* out(l) <- union sur tous les successeurs s de s.ins*)
    let rec look_ins_of_succ succ_list = begin match succ_list with
        | [] -> ()
        | t :: q -> let succ = Hashtbl.find live_info_tab t in live_info_l.outs <- Register.S.union live_info_l.outs succ.ins; look_ins_of_succ q
      end in look_ins_of_succ live_info_l.succ;

    (* in(l) <- use(l) union (out privé de def)*)
    let out_minus_def = Register.S.diff live_info_l.outs live_info_l.defs in live_info_l.ins <- Register.S.union live_info_l.uses out_minus_def;
    
    (* si in(l) est diff´erent de old_in(l) alors ajouter tous les pr´ed´ecesseurs de l dans WS *)
    if not (Register.S.equal old_in live_info_l.ins) then ws := Label.S.union live_info_l.pred !ws
  done


let liveness (graph_map:cfg) = 
  init_live_info(graph_map);
  modify_pred();
  kildall();
  Hashtbl.iter (fun l live_info_l -> live_info_map := Label.M.add l live_info_l !live_info_map  ) live_info_tab; (* L'exo veut qu'on rende un labelmap *)
  !live_info_map


(* On traduit le programme *)
let rec program p =
  {
    funs = List.map translate_fct p.Rtltree.funs;
    liveness_analyze = liveness !graph;
    ertl_graph = !graph
  }
