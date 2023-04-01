open Ltltree
open Ops

exception Error of string


(* CONSTRUCTION GRAPHE D'INTERFERENCE *)

let inter_graph = ref Register.M.empty


(* Initialise le graphe d'interférence *)
let make (liveness_analysis:Ertltree.live_info Label.map) = 
    (* Ajoute un arc de préférence v --> w *)
    let add_prefs v w = 
      if Register.M.mem v !inter_graph 
    then let arcs_v = Register.M.find v !inter_graph in arcs_v.prefs <- Register.S.add w arcs_v.prefs
        else let new_arcs = {prefs = Register.set_of_list ([w]); intfs = Register.S.empty} in inter_graph := Register.M.add v new_arcs !inter_graph;
    
    (* Ajoute un arc d'interférence v --> w *)
    in let add_inters v w =
      if Register.M.mem v !inter_graph 
  then begin let arcs_v = Register.M.find v !inter_graph in arcs_v.intfs <- Register.S.add w arcs_v.intfs; if Register.S.mem w arcs_v.prefs then arcs_v.prefs <- Register.S.remove w arcs_v.prefs   end
        else let new_arcs = {prefs = Register.S.empty; intfs = Register.set_of_list ([w])} in inter_graph := Register.M.add v new_arcs !inter_graph;


    (* 1er parcours pour ajouter une arête de préférence pour chaque instruction mov x y *)
    (*  A toutes instructions mov(w,v), créer l'arc w <-> v  *)
    in let premier_parcours l live_info_l = begin match live_info_l.Ertltree.instr with
      | Ertltree.Embinop (Mmov, w, v , _) -> if w <> v then add_prefs v w; add_prefs w v; (* Ajoute un arc pref entre v et w *)                
      | _ -> ()
      end
    in Label.M.iter premier_parcours liveness_analysis;
    

    (* 2nd parcours pour ajouter les arêtes d'interférences *)
    (* Itère sur les live_info (qui definit une variable) :
         - Si l'instruction est mov(w,v) : pour les outs différents de w et de v, créer l'arc out <-> v
         - Sinon : pour les defs, pour les outs != def, créer l'arc out <-> def
    *)
    let second_parcours l live_info_l = begin match live_info_l.Ertltree.instr with
      | Ertltree.Embinop (Mmov, w, v , _) -> let apply_on_out out =
                                                if out <> w && out <> v then begin add_inters out v; add_inters v out end;
                                             in Register.S.iter apply_on_out live_info_l.Ertltree.outs
      | _ -> let apply_on_def def = 
                  let aux out = if out <> def then begin add_inters out def; add_inters def out end;
                  in Register.S.iter aux live_info_l.Ertltree.outs
             in Register.S.iter apply_on_def live_info_l.Ertltree.defs

     end in Label.M.iter second_parcours liveness_analysis;
     
    (* Renvoie le graphe d'interférence finale *)
    !inter_graph


(* CONSTRUCTION GRAPHE D'INTERFERENCE *)
let spill_number = ref 0

let reg_colorization = ref Register.M.empty (* Les registres physiques affectés à chaque registres *)

let colorization = ref Register.M.empty (* Les opérand à chaque registres *)

let init_reg_colorization (graph:igraph) = Register.M.iter (fun reg arc -> 
                                          if not(Register.S.mem reg Register.allocatable) 
                                          then reg_colorization :=  Register.M.add reg (Register.set_of_list ([])) !reg_colorization
                                          else reg_colorization :=  Register.M.add reg (Register.set_of_list ([reg])) !reg_colorization
                                        ) graph

let init_colorization (graph:igraph) = Register.M.iter (fun reg arc -> 
                                          if Register.S.mem reg Register.allocatable
                                          then colorization :=  Register.M.add reg (Reg reg) !colorization
                                        ) graph


(* Création de todo *)
let todo = ref Register.M.empty

let make_todo (graph:igraph) = (* Rajoute tous les pseudo-registres dans todo. 1 signifie que reg € todo*)
  Register.M.iter  (fun reg arc -> if not(Register.S.mem reg Register.allocatable) then todo := Register.M.add reg 1 !todo) graph

let is_empty_todo() =
  let nb_ones = ref 0 in 
  let count_ones reg is_in_todo = if is_in_todo = 1 then incr nb_ones
  in Register.M.iter count_ones !todo; !nb_ones == 0


let choose_in_todo() (* Choisit un registre arbitraire par les registre € todo*) =
  let current_reg = ref Register.rax in
  let takes_one reg is_in_todo = if is_in_todo = 1 then current_reg := reg
  in Register.M.iter takes_one !todo; !current_reg

(* Créer un dictionnaire des couleurs possibles pour chaque registres *)
let possible_colors = ref Register.M.empty

let init_possible_colors (graph:igraph) = 
    let rec attr_color_interf (accu:Register.set) (reg_list:Register.set) = 
        if (Register.S.is_empty reg_list) 
            then accu 
            else begin 
              let pick_on_inter = Register.S.choose reg_list in (* Prend dans la liste *)
              let phy_reg = Register.M.find pick_on_inter !reg_colorization in (* Trouve sa couleur associée/ son registre physique*)
              let new_set = Register.S.remove pick_on_inter reg_list in (* Le retire de l'ensemble des interférés *)
              attr_color_interf (Register.S.union phy_reg accu) new_set
            end in
    let solo_init reg arc = 
        (* On créé un objet de type potential_color *)
        let interf_physical = attr_color_interf Register.S.empty arc.intfs in  (* Prends la liste des couleurs attribuées aux interférés *)
        let pref_physical = attr_color_interf Register.S.empty arc.prefs in (* Prends la liste des couleurs attribuées aux prefs*)
        if not(Register.S.mem reg Register.allocatable)
          then begin
              let reg_list = Register.S.diff Register.allocatable interf_physical in  (* Calcul la liste des couleurs possibles *)
              (* Construit l'objet final *)
              if Register.S.is_empty reg_list then 
                    let pot_color_obj = { possible_reg = reg_list;
                    priority_1 = false;
                    priority_2 = false;
                    priority_3 = false;
                    priority_4 = false;
                  }
                  in possible_colors := Register.M.add reg pot_color_obj !possible_colors
              else begin
                let pick_one = Register.S.choose reg_list in
                let pot_color_obj = { possible_reg = reg_list;
                                      priority_1 = ((Register.S.cardinal reg_list) = 1) && (Register.S.exists (fun elem -> elem = pick_one) pref_physical);
                                      priority_2 = (Register.S.cardinal reg_list) = 1;
                                      priority_3 = (Register.S.cardinal pref_physical) > 0;
                                      priority_4 = (Register.S.cardinal reg_list) > 0;
              
                }
                in possible_colors := Register.M.add reg pot_color_obj !possible_colors
                end
            end
    in Register.M.iter solo_init graph


let choose_register_to_color () = 
    (* Les priorités de choix reviennet à checker les flags dans potential_color :
      1) un registre avec une seule couleur possible, pour lequel on a une arête de préférence vers cette couleur :
          priority_1
      2) un registre avec une seule couleur possible ;
          priority_2
      3) un registre avec une préférence dont la couleur est connue ;
          priority_3
      4) un registre avec au moins une couleur possible, choisie arbitrairement ;
          priority_4
    *)
    let get_highest_priority (pot_color:potential_color) : int = (* Donne la priorité la plus haute pour un objet de type potential_color*)
        if pot_color.priority_1 
        then 1
        else begin
          if pot_color.priority_2
          then 2
          else begin
            if pot_color.priority_3
            then 3
            else begin
              if pot_color.priority_4
              then 4
              else 5
            end
          end
        end in
    let current_priority = ref 4 in let current_reg = ref Register.rax in (* #rax pour initialiser, j'aurais pu mettre n'importe quoi *)
    let examine_prority reg is_in_todo = if is_in_todo = 1 
                                         then begin
                                            let pot_color =  Register.M.find reg !possible_colors in
                                            let priority = get_highest_priority pot_color in
                                            if priority < !current_priority then begin current_priority := priority; current_reg := reg end
                                         end;
    in Register.M.iter examine_prority !todo;

    (* Maintenant qu'on a itéré sur todo, on a trouvé un registre de priorité la plus élevée *)
    if !current_reg = Register.rax 
    then begin(* Cas où il n'y a pas de registre à colorier *) 
        let reg_choosen = choose_in_todo() in todo := Register.M.add reg_choosen 0 !todo ; reg_choosen, true
        end
    else begin todo := Register.M.add !current_reg 0 !todo ; !current_reg, false end (* Il y a bien un registre à colorier *) 


let update_possible_color (color_to_remove) =
    let solo_update reg pot_color = pot_color.possible_reg <- Register.S.remove color_to_remove pot_color.possible_reg
    in Register.M.iter solo_update !possible_colors
   

let color (graphi:igraph) = 
    let nb_spills = ref 0 in 

    (* Initialise le dictionnaire reg_colorization = {register : color}. Ne sert que pendant l'initialisation de possible_colors *)
    init_reg_colorization(graphi);

    (* Initialise le dictionnaire colorization = {register : operand}  *)
    init_colorization(graphi);

    (* Initialise le dictionnaire todo = {register : 0 ou 1}  *)
    make_todo(graphi);

    (* Initialise le dictionnaire possible_colors = {register : potential_color }  *)
    init_possible_colors(graphi);



    while not(is_empty_todo()) do
      (* Choisit un registre à colorier s'il y en a selon les priorités *)
      let picked_reg, spilled = choose_register_to_color () in 

      (* Selon s'il est spilled ou pas, retirer sa couleur des couleurs possibles de toutes ses interférences *)
      if not(spilled) 
      then begin
          (* Choisit une couleur parmi les couleurs possibles de picked_reg *)
          let pot_color_of_picked = Register.M.find picked_reg !possible_colors in
          let choosen_color = Register.S.choose pot_color_of_picked.possible_reg in

          (* Retirer sa couleur des couleurs possibles de toutes ses interférences *)
          update_possible_color(choosen_color); 

          (* Colorie avec un opérand Reg *)
          colorization := Register.M.add picked_reg (Reg choosen_color) !colorization;
        end 
      else begin
          (* Dans le cas de spill, on colorie avec un opérand Spill *)
          incr nb_spills;
          colorization := Register.M.add picked_reg (Spilled (- 8 - !nb_spills * 8)) !colorization
        end
    done;
    graphi, !nb_spills

let print ig =
  Register.M.iter (fun r arcs ->
    Format.printf "%s: prefs=@[%a@] intfs=@[%a@]@." (r :> string)
      Register.print_set arcs.prefs Register.print_set arcs.intfs) ig

(* TRADUCTION ERTL -> LTL *)


let nb_spilled = ref 0

(* Initialise le créer un dictionnaire vide *)
let graph = ref Label.M.empty

(* Ajoute un nouveau label dans le dictionnaire *)
let add_label_in_graph (i:instr) =
  let new_label = Label.fresh () in graph := Label.M.add new_label i !graph; new_label

(* Fonction qui récupère la couleur d'un registre ERTL et qui renvoie true si c'est spilled *)
let lookup r = if Register.is_hw r 
                  then Reg r, false 
                  else begin
                    let color_find = Register.M.find r !colorization in match color_find with
                      | Reg _ -> color_find, false
                      | Spilled _ -> color_find, true
                  end




let instru = function
  | Ertltree.Econst(n, r, l) -> Econst (n, fst (lookup r), l)
  | Ertltree.Ereturn -> Ereturn
  | Ertltree.Egoto (l)-> Egoto l
  | Ertltree.Ecall (ident, num_args, l) -> Ecall (ident, l)
  | Ertltree.Emunop (unop, r, l) -> Emunop (unop, fst (lookup r), l) 
  | Ertltree.Emubranch (branch, r, l1, l2) -> Emubranch (branch, fst (lookup r), l1, l2)

  | Ertltree.Embbranch (branch, r1, r2, l1, l2) -> let op1 = fst (lookup r1) in
                                                  let op2 = fst (lookup r2) in
                                                  Embbranch (branch, op1, op2, l1, l2)
                                                
  | Ertltree.Embinop (binop, r1, r2, l) -> let op1, is_spilled1 = lookup r1 in let op2, is_spilled2 = lookup r2 in begin match binop, is_spilled1, is_spilled2 with
                                              | Mmov, _, _ when op1 = op2 -> Egoto l
                                              | Mmov, true, true  -> let temp_r = Register.tmp1 in
                                                                    let l_d_1 = add_label_in_graph (Embinop (Mmov, Reg (temp_r), op2, l)) in
                                                                    Embinop (Mmov, op1 , Reg(temp_r), l_d_1)
                                              | Mmov,_,_ -> Embinop (Mmov, op1, op2, l)
                                              | Mmul, _, true  -> let temp_r = Register.tmp1 in
                                                                  let l_d_1 = add_label_in_graph (Embinop (Mmov, Reg(temp_r), op2, l)) in
                                                                  let l_d_2 = add_label_in_graph (Embinop (Mmul, op1, Reg(temp_r), l_d_1)) in
                                                                  Embinop (Mmov, op2,  Reg(temp_r) , l_d_2)
                                              | _, true, true -> let temp_r = Register.tmp1 in
                                                                  let l_d_1= add_label_in_graph (Embinop (Mmov, Reg(temp_r) , op2 , l)) in
                                                                  let l_d_2= add_label_in_graph (Embinop (binop, op1, Reg(temp_r), l_d_1)) in
                                                                  Embinop (Mmov, op2 ,Reg(temp_r) , l_d_2)
                                              | _, _,_ -> Embinop (binop, op1, op2, l)
                                              end
                                
  | Ertltree.Eload (r, n, rd, l) -> let op_d = fst (lookup rd) in
                                    let src_op = fst (lookup r) in
                                    let l_d_1, dest_r = match op_d with
                                        | Reg r -> l, r
                                        | Spilled i -> let temp_op = Reg (Register.tmp1) in
                                                      let l_temp = add_label_in_graph (Embinop (Mmov, temp_op, op_d, l)) in l_temp, Register.tmp1
                                    in begin match src_op with 
                                        | Reg r -> Eload (r, n, dest_r, l_d_1)
                                        | Spilled i -> let temp_r = Register.tmp2 in
                                                      let src_op = Reg (temp_r) in
                                                      let l_d_2 = add_label_in_graph (Eload(temp_r, n, dest_r, l_d_1)) in
                                                      Embinop (Mmov, src_op, src_op, l_d_2)
                                        end

  | Ertltree.Estore (r, rd, n, l) -> let op_d = fst (lookup rd) in
                                     let src_op = fst (lookup r) in
                                     let l_d_1, dest_r = match op_d with
                                        | Reg (r) -> l, r
                                        | Spilled (i) -> let temp_op = Reg (Register.tmp1) in
                                                        let l_temp= add_label_in_graph (Embinop (Mmov, temp_op, op_d, l)) in l_temp, Register.tmp1
                                      in begin match src_op with 
                                        | Reg r -> Estore (r, dest_r, n, l_d_1) 
                                        | Spilled i -> let temp_r = Register.tmp2 in
                                                        let src_op = Reg (temp_r) in
                                                        let l_d_2 = add_label_in_graph (Estore(temp_r, dest_r, n, l_d_1)) in
                                                        Embinop (Mmov, src_op, src_op, l_d_2)
                                      end

  | Ertltree.Epush_param (r, l) -> Epush (fst (lookup r), l)
  | Ertltree.Ealloc_frame (l) -> let l_d_1 = if !nb_spilled <> 0
                                            then add_label_in_graph (Emunop(Maddi (Int32.of_int(- 8 * !nb_spilled)), Reg(Register.rsp), l))
                                            else l in
                                let l_d_2 = add_label_in_graph (Embinop(Mmov, Reg(Register.rsp), Reg(Register.rbp), l_d_1)) in
                                Epush (Reg (Register.rbp), l_d_2)
  | Ertltree.Edelete_frame (l) -> let label = add_label_in_graph (Epop (Register.rbp, l)) in
                                  Embinop(Mmov, Reg(Register.rbp), Reg(Register.rsp), label)
  | Ertltree.Eget_param (n, r, l) -> let op, is_spilled = lookup r in
                                    if is_spilled
                                    then let temp_r = Register.tmp1 in
                                          let l_d_1 = add_label_in_graph (Embinop (Mmov, Reg(temp_r), op, l)) in
                                          Embinop (Mmov, Spilled(n) , Reg(temp_r), l_d_1)
                                    else Embinop(Mmov, Spilled(n), op , l)


(* Convertit les instructions ertl en instructions ltl *)
(* Reçoit une instruction ERTL :   ertl : i --> next_l *)
let ertl_to_ltl_instr ertl_l ertl_i = let ltl_i = instru ertl_i in graph := Label.M.add ertl_l ltl_i !graph

(* Convertit les fonctions *)
let translate_fct (f:Ertltree.deffun) = Label.M.iter ertl_to_ltl_instr f.Ertltree.fun_body ; (* Convertit chaque instruction RTL en appliquant la fonction de convertion sur le dictionnaire Label.M = {étiquette : instruction RTL} *)
{
  fun_name = f.Ertltree.fun_name;
  fun_entry = f.Ertltree.fun_entry;
  fun_body = !graph;
}


let program p = 

  let inter_graph_final = make p.Ertltree.liveness_analyze in
  let colors, n = color inter_graph_final in nb_spilled := n; 
  let funlist = List.map translate_fct p.Ertltree.funs in
  {
    funs = funlist;
  }