
open Ast
open Format

(* Exception levée pour signaler une erreur pendant l'interprétation *)
exception Error of string
let error s = raise (Error s)

(* Les valeurs de Mini-Python

   - une différence notable avec Python : on
     utilise ici le type int alors que les entiers de Python sont de
     précision arbitraire ; on pourrait utiliser le module Big_int d'OCaml
     mais on choisit la facilité
   - ce que Python appelle une liste est en réalité un tableau
     redimensionnable ; dans le fragment considéré ici, il n'y a pas
     de possibilité d'en modifier la longueur, donc un simple tableau OCaml
     convient *)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Affichage d'une valeur sur la sortie standard *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Interprétation booléenne d'une valeur

   En Python, toute valeur peut être utilisée comme un booléen : None,
   la liste vide, la chaîne vide et l'entier 0 sont considérés comme
   False et toute autre valeurs comme True *)

let is_false v = match v with
  | Vnone -> true
  | Vbool b -> not b
  | Vint n -> n = 0
  | Vstring s -> s = ""
  | Vlist l -> l = [||]

let is_true v = not (is_false v)

(* Les fonctions sont ici uniquement globales *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* L'instruction 'return' de Python est interprétée à l'aide d'une exception *)

exception Return of value

(* Les variables locales (paramètres de fonctions et variables introduites
   par des affectations) sont stockées dans une table de hachage passée en
   arguments aux fonctions suivantes sous le nom 'ctx' *)

type ctx = (string, value) Hashtbl.t

(* Interprétation d'une expression (renvoie une valeur) *)

(* Fonction auxilière pour comparer des listes *)
(* compare a b = -1 si a < b , = 0 si a = b et = 1 si a > b *)
let rec compare_list a1 n1 a2 n2 i =
  if i = n1 && i = n2 then 0
  else if i = n1 then -1
  else if i = n2 then 1
  else let c = compare a1.(i) a2.(i) in
       if c <> 0 then c else compare_list a1 n1 a2 n2 (i + 1)

let rec expr ctx = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  (* arithmétique *)
  | Ecst (Cint n) ->
      Vint n
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
        | Bdiv, Vint n1, Vint n2 when n2 = 0 -> error "division by zero"
        | Bdiv, Vint n1, Vint n2 -> Vint (n1 / n2)
        | Bmod, Vint n1, Vint n2 when n2 = 0 -> error "division by zero"
        | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
        | Beq, Vlist l1, Vlist l2 -> Vbool ((compare_list l1 (Array.length l1) l2 (Array.length l2) 0) = 0)
        | Beq, _, _  -> Vbool (compare v1 v2 = 0)
        | Bneq, Vlist l1, Vlist l2 -> Vbool ((compare_list l1 (Array.length l1) l2 (Array.length l2) 0) <> 0)
        | Bneq, _, _  -> Vbool (compare v1 v2 <> 0)
        | Blt, Vlist l1, Vlist l2 -> Vbool ((compare_list l1 (Array.length l1) l2 (Array.length l2) 0) < 0)
        | Blt, _, _  -> Vbool (compare v1 v2 < 0)
        | Ble, Vlist l1, Vlist l2 -> Vbool ((compare_list l1 (Array.length l1) l2 (Array.length l2) 0) <= 0)
        | Ble, _, _  -> Vbool (compare v1 v2 <= 0)
        | Bgt, Vlist l1, Vlist l2 -> Vbool ((compare_list l1 (Array.length l1) l2 (Array.length l2) 0) > 0)
        | Bgt, _, _  -> Vbool (compare v1 v2 > 0)
        | Bge, Vlist l1, Vlist l2 -> Vbool ((compare_list l1 (Array.length l1) l2 (Array.length l2) 0) >= 0)
        | Bge, _, _  -> Vbool (compare v1 v2 >= 0)
        | Badd, Vstring s1, Vstring s2 -> Vstring (s1^s2)  (* concatène 2 chaines *)
        | Badd, Vlist l1, Vlist l2 -> Vlist (Array.append l1 l2)
        | _ -> error "unsupported operand types"
      end
  | Eunop (Uneg, e1) -> let v = expr ctx e1 in 
    begin match v with
        | Vint n -> Vint (-n)
        | _ -> error "neg with non int expression"
    end
  (* booléens *)
  | Ecst (Cbool b) -> Vbool b
  | Ebinop (Band, e1, e2) -> let v1 = expr ctx e1 in
      if (is_true v1) then (expr ctx e2) else v1
  | Ebinop (Bor, e1, e2) -> let v1 = expr ctx e1 in
      if (is_true v1) then v1 else (expr ctx e2)
  | Eunop (Unot, e1) -> Vbool (is_false (expr ctx e1))
  | Eident id -> 
    begin match Hashtbl.mem ctx id with 
        | true -> Hashtbl.find ctx id
        | _ -> error "variable doesn't exist"
    end 
  (* appel de fonction *)
  | Ecall ("len", [e1]) ->
      begin match expr ctx e1 with
        | Vlist l -> Vint (Array.length l)
        | Vstring s -> Vint (String.length s)
        | _ -> error "cannot use len"
      end
  | Ecall ("list", [Ecall ("range", [e1])]) -> 
    let n = expr ctx e1 in
    let rec aux accu i = begin match i with 
      | Vint 0 -> accu
      | Vint v -> aux ((Vint (v-1)) :: accu) (Vint (v-1))
      | _ -> error "range has to take an integer"
    end in Vlist (Array.of_list (aux [] n))
  | Ecall (f, el) -> 
    let (args, instr) = Hashtbl.find functions f in
    let new_env = Hashtbl.create 16 in (* initialise un nouvel environnement *)
    begin
      List.iter2 (fun id e -> Hashtbl.add new_env id (expr ctx e)) args el; (*  args_1 <- el_1 , ... , args_n <- el_n  *)
      try begin stmt new_env instr; Vnone end
      with Return v -> v 
      (* Si aucune instruction return n'est rencontrée dans le corps de la fonction, alors l'instruction Vnone *)
      (* En revanche, si l'instruction return est rencontrée avec une valeur v, l'exception Return v est levée et cette valeur v est renvoyée en tant que résultat de l'appel de fonction. *)
    end

  | Elist el ->
      Vlist (Array.of_list (List.map (expr ctx) el))  (*  Trouve la valeur de toutes les expressions dans la liste et convertit la liste en array  *)
  | Eget (e1, e2) -> begin match expr ctx e1 with
      | Vlist l -> let i = expr ctx e2 in 
        begin match i with
          | Vint index -> (try l.(index) with Invalid_argument _ -> error "index out of bounds")
          | _ -> error "index must be an integer"
        end
      | _ -> error "syntax only for list" 
    end

(* interprétation d'une instruction ; ne renvoie rien *)

and stmt ctx = function
  | Seval e ->
      ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sif (e, s1, s2) -> if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
  | Sassign (id, e1) -> Hashtbl.replace ctx id (expr ctx e1) (* Hashtbl.replace a b c, c'est l'équivalent de a[b] = c en Python*)
  | Sreturn e -> raise (Return (expr ctx e))
  | Sfor (x, e, s) ->
    begin match expr ctx e with
      | Vlist l -> Array.iter (fun v -> Hashtbl.replace ctx x v; stmt ctx s) l (* Fonction lambda qui remplace x dans l'environnement et fait l'instruction s *)
      | _ -> error "list expected" 
    end
  | Sset (e1, e2, e3) -> begin match expr ctx e1 with
      | Vlist l -> let v2 = expr ctx e2 in
        begin match v2 with
          | Vint i2 -> l.(i2) <- expr ctx e3 (* e1 : liste, e2 : indice d'affection, e3 : expression à mettre dans la liste *)
          | _ -> error "index must be an integer"
        end
      | _ -> error "syntax only for list" 
    end

(* interprétation d'un bloc i.e. d'une séquence d'instructions *)

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* interprétation d'un fichier
   - dl est une liste de définitions de fonction (cf Ast.def)
   - s est une instruction, qui représente les instructions globales
 *)

let file (dl, s) =
  List.iter (fun (f, args, instr) -> Hashtbl.add functions f (args, instr)) dl; (* Apply the lambda function to dl *)
  stmt (Hashtbl.create 16) s
