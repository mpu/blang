type low_idnt = string

type low_type =
  | LTint of int (* number of bytes *)

type low_binop = LBadd | LBsub

type low_expr =
  | LEvar of low_idnt
  | LEnum of int
  | LEbinop of low_expr * low_binop * low_expr

type low_stmt =
  | LSdecl of low_idnt * low_type
  | LSscope of low_stmt list
  | LSassign of low_idnt * low_expr

module Export = struct
  open Printf

  type qbe_val =
    | Tmp of string
    | Num of int

  let printval f = function
    | Tmp t -> fprintf f "%%%s" t
    | Num n -> fprintf f "%d" n

  let fresh_tmp, fresh_lbl =
    let fresh_num = ref (-1) in
    (fun () ->
      incr fresh_num;
      Tmp ("v" ^ string_of_int !fresh_num)),
    (fun () ->
      incr fresh_num;
      "@lbl" ^ string_of_int !fresh_num)

  let rec lookup v = function
    | [] -> failwith "oos"
    | `Var (v', t) :: _ when v' = v -> t
    | _ :: l -> lookup v l

  let rec do_expr env = function
    | LEvar v ->
      let t = fresh_tmp () in
      printf "\t%a =w load %a\n" printval t printval (lookup v env);
      t
    | LEnum n ->
      Num n
    | LEbinop (el, op, er) ->
      let vl = do_expr env el in
      let vr = do_expr env er in
      let t = fresh_tmp () in
      let sop = match op with LBadd -> "add" | LBsub -> "sub" in
      printf "\t%a =w %s %a, %a\n" printval t sop printval vl printval vr;
      t

  let rec do_stmt env = function
    | LSdecl (v, _) ->
      let t = fresh_tmp () in
      env := `Var (v, t) :: !env;
      printf "\t%a =q alloc4 4\n" printval t
    | LSscope s ->
      do_scope env s
    | LSassign (v, e) ->
      let tv = lookup v !env in
      let ve = do_expr !env e in
      printf "\tstorew %a, %a\n" printval ve printval tv

  and do_scope env scope =
    begin
      env := `Enter :: !env;
      List.iter (do_stmt env) scope;
      while
        let hd = List.hd !env in
        env := List.tl !env;
        hd <> `Enter
      do () done
    end

  let as_function name scope =
    begin
      printf "export function %s()\n{\n" name;
      let env = ref [] in
      do_scope env scope;
      printf "}\n";
    end

end
