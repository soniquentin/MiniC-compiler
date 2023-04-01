open X86_64
open Ops
open Format
open Utils 

exception Error of string

let visited = Hashtbl.create 17

type instr = Code of X86_64.text | Label of Label.t

let code = ref []

let emit l i = code := Code i :: Label l :: !code

let emit_wl i = code := Code i :: !code

let labels = Hashtbl.create 17

let need_label l = Hashtbl.add labels l ()




let rec lin g l =
  if not (Hashtbl.mem visited l) then begin
    Hashtbl.add visited l ();
    instru g l (Label.M.find l g)
  end else begin
    need_label l;
    emit_wl (jmp (l :> string))
  end


(* On traduit chaque insruction Ltl en émettant dans code *)
and instru g l = function
  | Ltltree.Econst (n, r, l1) -> emit l (movq (imm32 n) (operand r)); lin g l1
  | Ltltree.Egoto (l1) -> code := Label l :: !code; lin g l1
  | Ltltree.Eload (op1 ,n , op2, l1) -> emit l (movq (X86_64.ind ~ofs:n (string_to_operand op1)) (reg (string_to_operand op2))); lin g l1
  | Ltltree.Estore (op1, op2, n, l1) -> emit l (movq (reg (string_to_operand op1)) (X86_64.ind ~ofs:n (string_to_operand op2))); lin g l1
  | Ltltree.Ereturn -> emit l ret
  | Ltltree.Emunop (unop, op, l1) -> begin match unop with 
                                        | Maddi i ->  emit l (addq (imm32 i) (operand op))
                                        | Msetei i -> emit l (cmpq (imm32 i) (operand op)); emit_wl (sete (b_operand op))
                                        | Msetnei i -> emit l (cmpq (imm32 i) (operand op)) ; emit_wl (setne (b_operand op))
                                     end; lin g l1

  | Ltltree.Embinop (binop, op1, op2, l1) -> begin match binop with  
                                                | Mmov | Madd | Msub | Mmul -> emit l (convert_binop_ltl binop (operand op1) (operand op2))
                                                | Mdiv -> emit l cqto; emit_wl (idivq (operand op1)); 
                                                | _ -> begin match op2 with | Ltltree.Reg(reg2) -> emit l (cmpq (operand op1) (operand op2)); 
                                                                                                  emit_wl (b_convert_binop_ltl binop (reg r11b)); 
                                                                                                  emit_wl (movzbq (reg r11b) (string_to_operand reg2))
                                                                            | Ltltree.Spilled (i) -> let pile = ind ~ofs:i X86_64.rbp in 
                                                                                                    emit l (cmpq (operand op1) (operand op2)); 
                                                                                                    emit_wl (b_convert_binop_ltl binop (reg r11b));
                                                                                                    emit_wl (movzbq (reg r11b) (string_to_operand (Register.tmp2)));
                                                                                                    emit_wl (movq (operand (Ltltree.Reg(Register.tmp2))) pile)
                                                      end
                                            end

  | Ltltree.Epush (op, l1) -> emit l (pushq (operand op)); lin g l1
  | Ltltree.Epop (op, l1) -> emit l (popq (string_to_operand (op))); lin g l1
  | Ltltree.Emubranch (mubranch, op, l1, l2) -> (* On commence par tester op pour trigger les flags *)
                                                begin match mubranch with
                                                  | Mjz | Mjnz -> emit l (testq (operand op) (operand op))
                                                  | Mjlei i | Mjgi i -> emit l (cmpq (imm32 i) (operand op))
                                                end;
                                                
                                                (* On ajoute une instruction de saut *)
                                                if not (Hashtbl.mem visited l2) 

                                                then begin (* Le saut en l2 n'a encore été visité : visitons L1 et L2 ! *)
                                                    need_label l1;
                                                    let instruc = begin match mubranch with
                                                      | Mjz -> jz 
                                                      | Mjnz -> jnz
                                                      | Mjlei _ -> jle
                                                      | Mjgi _ -> jg 
                                                    end in emit_wl (instruc (l1 :> string)); lin g l2; lin g l1;
                                                end

                                                else begin
                                                    need_label l2; (* Le saut en l2 a déjà été visité. On ajoute un jump sur L2 si la condition n'est pas vérifiée *)
                                                    if not (Hashtbl.mem visited l1) 
                                                    then begin
                                                        let instruc = begin match mubranch with
                                                          | Mjz -> jnz 
                                                          | Mjnz -> jz
                                                          | Mjlei _ -> jg
                                                          | Mjgi _ -> jle 
                                                        end in emit_wl (instruc (l2 :> string)); lin g l1;
                                                    end
                                                    else begin (* Les 2 ont été visités : jump L1 si condition OK, jump L2 sinon*)
                                                        need_label l1; 
                                                        let instruc = begin match mubranch with
                                                          | Mjz -> jz 
                                                          | Mjnz -> jnz
                                                          | Mjlei _ -> jle
                                                          | Mjgi _ -> jg 
                                                        end in emit_wl (instruc (l1 :> string)); emit_wl (jmp (l2 :> string))
                                                    end
                                                end

  | Ltltree.Embbranch (mbbranch, op1, op2, l1, l2) -> (* On procède comme pour Emubranch *)
                                                      emit l (cmpq (operand op1) (operand op2)); (* Compare les 2 *)

                                                      if not (Hashtbl.mem visited l2) (* L2 n'a pas été visité *)                                        

                                                      then begin
                                                          need_label l1;
                                                          let instruc = begin match mbbranch with
                                                            | Mjl -> jl
                                                            | Mjle -> jle
                                                          end in emit_wl (instruc (l1 :> string)); lin g l2; lin g l1
                                                      end
                                                      else begin
                                                        need_label l2;
                                                        if not (Hashtbl.mem visited l1) 
                                                        then begin
                                                          need_label l1;
                                                          let instruc = begin match mbbranch with
                                                            | Mjl -> jle
                                                            | Mjle -> jl 
                                                          end in emit_wl (instruc (l2 :> string)); lin g l1
                                                        end 
                                                        else begin
                                                          let instruc = begin match mbbranch with
                                                            | Mjl -> jl
                                                            | Mjle -> jle
                                                          end in emit_wl (instruc (l1 :> string)); emit_wl (jmp (l2 :> string))
                                                        end
                                                      end
                                                      
  | Ltltree.Ecall (ident , l1) -> emit l (call ident); lin g l1

let program p = 
    (* Fonction pour linéariser une liste de deffun *)
    let rec linearize_functions (list_of_function:Ltltree.deffun list) = begin match list_of_function with
        | [] -> ()
        | t :: q -> emit_wl (label t.Ltltree.fun_name); lin t.Ltltree.fun_body t.Ltltree.fun_entry; linearize_functions q
        end in linearize_functions p.Ltltree.funs;
    
    (* On filtre les labels qui ne seront plus utilisés dans le code *)
    let filter_instr (asm1:text) (instr:instr) = begin match instr with 
      | Code cd -> (++) cd asm1
      | Label lb -> (* Si l'étiquette est important i.e dans labels, on la garde *) if Hashtbl.mem labels lb then (++) (label (lb :> string)) asm1 else asm1
    end in let final_text = List.fold_left filter_instr nop !code in
    {text = (++) (globl "main") final_text; data = nop}