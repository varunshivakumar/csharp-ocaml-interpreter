open Ast;;
open Env;;
open Store;;

let rec allocEntry loc (name, entry) : ((string * symEntry) * int) = match entry with
 | (Fun fe) -> let symTable1 = allocateSymTable loc fe.syms in
               ((name, Fun {fe with syms=symTable1}),  loc)
 | (Var ve) -> 
    match ve.vtype with 
    | Array(vtype1, numEle) ->  ((name, Var {ve with loc=loc}), loc+numEle)
    | (Void) -> ((name, Var {ve with loc=loc}), loc+1)
    | (Int) -> ((name, Var {ve with loc=loc}), loc+1)
    | (_) -> ((name, Var {ve with loc=loc}), loc+10) 
              
and allocateSymTable loc symtable : symTable = match symtable with 
 | ((str, entry)::symTable2) -> let ((str1, entry1), loc1) = (allocEntry loc (str, entry)) in (str1, entry1)::(allocateSymTable loc1 symTable2)
 | [] -> []

and allocateMem (env:environment) : environment = match env with
 | (symTable::env1) -> (allocateSymTable 0 symTable)::(allocateMem env1)
 | [] -> []
;; 

type interpreter_state = proc_state * environment *store;;
type stmtEvalRes = Next | BreakOut | ContinueOn;;
(* eval_expr: expr -> proc_state -> env -> store -> int *)
let rec eval_expr (expr:expr) (ps:proc_state) (env:environment) (store:store) : (interpreter_state * int)  = match expr with
  | Add (e1, e2) -> 
      let ((ps', env, store'), r1) = eval_expr e1 ps env store in
      let ((ps'', env, store''), r2) = eval_expr e2 ps' env store' in
      ((ps'', env, store''), r1 + r2)
  | Sub (e1, e2) ->
      let ((ps', env, store'), r1) = eval_expr e1 ps env store in
      let ((ps'', env, store''), r2) = eval_expr e2 ps' env store' in
      ((ps'', env, store''), r1 - r2)
  | Mul (e1, e2)->
      let ((ps', env, store'), r1) = eval_expr e1 ps env store in
      let ((ps'', env, store''), r2) = eval_expr e2 ps' env store' in
      ((ps'', env, store''), r1 * r2) 
  | Div (e1, e2)->
      let ((ps', env, store'), r1) = eval_expr e1 ps env store in
      let ((ps'', env, store''), r2) = eval_expr e2 ps' env store' in
      ((ps'', env, store''), r1 / r2)
  | Neg (e1) ->
      let ((ps', env, store'), r1) = eval_expr e1 ps env store in
      ((ps', env, store'), -1 * r1)
  | Id name -> 
      let (Var x) = binding_of env name in
      let l = x.loc in 

      let b = ((x.kind = Param)||(x.kind = Local)) in
      if b then 
      let b1 = match x.vtype with
	| Pointer (Pointer(e)) -> true
	| _ -> false
      in if b1 then
                   let l1 = l(*if this is taken out 102 is printed*) in 
                   ((ps, env, store), value_at store l1)
               else
                   let l1 = l  + ps.bp in 
                   ((ps, env, store), value_at store l1)
      else ((ps, env, store), value_at store l)
   
     
                  
  | IntConst i -> ((ps, env, store), i)
  | Pre (Id ex) ->       
      let (Var x) = binding_of env ex in
      let l1 = x.loc in 
      
      let b = ((x.kind = Param)||(x.kind = Local)) in
      if b then let l = l1 + ps.bp in

      let oval = value_at store l in
      let nval = oval + 1 in
      let store' = insert_value store l nval in
      ((ps, env, store'), nval) 

      else 
 
      let oval = value_at store l1 in
      let nval = oval + 1 in
      let store' = insert_value store l1 nval in
      ((ps, env, store'), nval)
  | Post (Id name) -> 
      let (Var x) = binding_of env name in
      let l = x.loc in 

      let b = ((x.kind = Param)||(x.kind = Local)) in
    
      if b then let l1 = l + ps.bp in

      let oval = value_at store l1 in
      let nval = oval + 1 in
      let store' = insert_value store l1 nval in
      ((ps, env, store'), oval)   

      else 

      let oval = value_at store l in
      let nval = oval + 1 in
      let store' = insert_value store l nval in
      ((ps, env, store'), oval) 
  | Post(Deref(Id name))-> 
      let (Var x) = binding_of env name in
      let l1 = x.loc in
     
      let b = ((x.kind = Param)||(x.kind = Local)) in

      if b then 
      
      let b1 = match x.vtype with
             | Pointer (e) -> true
             | _ -> false
      in

      if b1 then let l = l1 in
      let ptrl = value_at store l in
      let value = value_at store ptrl in
      let nvalue = value + 1 in
      let store' = insert_value store ptrl nvalue in
      ((ps, env, store'), value)

      else
      let l = l1 + ps.bp in

      let ptrl = value_at store l in
      let value = value_at store ptrl in
      let nvalue = value + 1 in
      let store' = insert_value store ptrl nvalue in
      ((ps, env, store'), value)

      else 
    
      let ptrl = value_at store l1 in
      let value = value_at store ptrl in
      let nvalue = value + 1 in
      let store' = insert_value store ptrl nvalue in
      ((ps, env, store'), value)
  | Pre(Deref(Id name))-> 
      let (Var x) = binding_of env name in
      let l = x.loc in

      let b = ((x.kind = Param)||(x.kind = Local)) in
      if b then let l1 = l + ps.bp in

      let ptrl = value_at store l in
      let value = value_at store ptrl in
      let nvalue = value + 1 in
      let store' = insert_value store ptrl nvalue in
      ((ps, env, store'), nvalue)

      else
      let ptrl = value_at store l in
      let value = value_at store ptrl in
      let nvalue = value + 1 in
      let store' = insert_value store ptrl nvalue in
      ((ps, env, store'), nvalue)
  | Pre(Deref(At(Id name, i))) -> 
      let (Var entry) = binding_of env name in 
      let ((ps', env, store'), index) = eval_expr i ps env store in
      let l = entry.loc + index in

      let b = ((entry.kind = Param)||(entry.kind = Local)) in
      if b then let l1 = l  + (ps').bp in

      let ptrval = value_at store' l1 in
      let oval = value_at store' ptrval in
      let nval = oval + 1 in
      let store'' = insert_value store' ptrval nval in
      ((ps', env, store''), oval)

      else
      let ptrval = value_at store' l in
      let oval = value_at store' ptrval in
      let nval = oval + 1 in
      let store'' = insert_value store' ptrval nval in
      ((ps', env, store''), oval)
  | Post(Deref(At(Id name, i))) -> 
      let (Var entry) = binding_of env name in 
      let ((ps', env, store'), index) = eval_expr i ps env store in
      let l = entry.loc + index in

      let b = ((entry.kind = Param)||(entry.kind = Local)) in
      if b then let l1 = l  + (ps').bp in

      let ptrval = value_at store' l1 in
      let oval = value_at store' ptrval in
      let nval = oval + 1 in
      let store'' = insert_value store' ptrval nval in
      ((ps', env, store''), nval)

      else
      let ptrval = value_at store' l in
      let oval = value_at store' ptrval in
      let nval = oval + 1 in
      let store'' = insert_value store' ptrval nval in
      ((ps', env, store''), nval)
  | Pre(Deref(Deref(Id name))) -> 
      let (Var entry) = binding_of env name in
      let l = entry.loc in

      let b = ((entry.kind = Param)||(entry.kind = Local)) in
      if b then let l1 = l + ps.bp in

      let ptrl1 = value_at store l1 in
      let ptrl2 = value_at store ptrl1 in
      let ovalue = value_at store ptrl2 in
      let nvalue = ovalue + 1 in
      let store' = insert_value store ptrl2 nvalue in
      ((ps, env, store'), ovalue)

      else
      let ptrl1 = value_at store l in
      let ptrl2 = value_at store ptrl1 in
      let ovalue = value_at store ptrl2 in
      let nvalue = ovalue + 1 in
      let store' = insert_value store ptrl2 nvalue in
      ((ps, env, store'), ovalue) 
  | Post(Deref(Deref(Id name))) -> 
      let (Var entry) = binding_of env name in
      let l = entry.loc in

      let b = ((entry.kind = Param)||(entry.kind = Local)) in
      if b then let l1 = l + ps.bp in

      let ptrl1 = value_at store l1 in
      let ptrl2 = value_at store ptrl1 in
      let ovalue = value_at store ptrl2 in
      let nvalue = ovalue + 1 in
      let store' = insert_value store ptrl2 nvalue in
      ((ps, env, store'), nvalue)

      else
      let ptrl1 = value_at store l in
      let ptrl2 = value_at store ptrl1 in
      let ovalue = value_at store ptrl2 in
      let nvalue = ovalue + 1 in
      let store' = insert_value store ptrl2 nvalue in
      ((ps, env, store'), nvalue)
  | Call (name, exprs) ->
      
      let (Fun x) = binding_of env name in
      let scope = x.syms in

      let nps = {sp = ps.sp; bp = ps.bp + x.nlocals + 10;} in

      let store' = allocValues exprs scope ps nps env store in
      let funenv = match env with
	| [l; g] -> [scope; g]
	| [g] -> [scope; g]
      in
      let body = x.body in
      
      let ((ps'', env'', store''), rs) = eval_stmt (List body) nps funenv store' in
      let ((ps''', env''', store'''), rv) = eval_expr (x.rv) ps'' funenv store'' in 
      ((ps, env, store'''), rv)

  | At(Id name1, Id name2) -> 
      let (Var e1) = binding_of env name1 in 
      let (Var e2) = binding_of env name2 in
      
     
      let b = ((e2.kind = Param)||(e2.kind = Local)) in
      if b then let l2 = e2.loc + ps.bp in 
                let index = value_at store l2 in
                if ((e1.kind = Param)||(e1.kind = Local)) then let l1 = e1.loc + ps.bp + index in
                                                               ((ps, env, store), value_at store l1)
                                                          else let l1 = e1.loc + index in
                                                               ((ps, env, store), value_at store l1)
           else let l2 = e2.loc in
                let index = value_at store l2 in
                if ((e1.kind = Param)||(e1.kind = Local)) then let l1 = e1.loc + ps.bp + index in
                                                               ((ps, env, store), value_at store l1)
                                                          else let l1 = e1.loc + index in
                                                               ((ps, env, store), value_at store l1)
  | At(Id name, i) -> 
      let (Var entry) = binding_of env name in 
      let ((ps', env, store'), index) = eval_expr i ps env store in
      let l = entry.loc + index in
     
      let b = ((entry.kind = Param)||(entry.kind = Local)) in
      if b then 
      
      let b1 = match entry.vtype with
             | Pointer (e) -> true
             | _ -> false
      in if b1 then
         ((ps, env, store), value_at store l)

      else
      let l1 = l  + (ps').bp in 
      ((ps, env, store), value_at store l1)

      
      else ((ps, env, store), value_at store l)
  | Deref(Add(Id name, i)) -> 
      let (Var entry) = binding_of env name in
      let l = entry.loc in
      let ((ps', env, store'), x) = eval_expr i ps env store in

      let b = ((entry.kind = Param)||(entry.kind = Local)) in
      if b then let l1 = l  + (ps').bp in
 
      let ptrval = value_at store l1 in
      let ptr = ptrval + x in
      ((ps', env, store'), ptr)

      else
      let ptrval = value_at store l in
      let ptr = ptrval + x in
      ((ps', env, store'), ptr)
      
  | Deref(Sub(Add(Id n1, Id n2), e1)) -> 
      let (Var entry1) = binding_of env n1 in

      let l = entry1.loc + ps.bp in

      let ptrval = value_at store l in

      let (Var entry2) = binding_of env n2 in 

      let l1 = entry2.loc + ps.bp in

      let x1 = value_at store l1 in
 
      let ((ps', env, store'), x2) = eval_expr e1 ps env store in
      let ptr = ptrval + x1 + x2 in
      let ptr1 = 322 * ptr + 3 in
      ((ps', env, store'), ptr1)

  | Deref (Id name) -> 
      let (Var entry) = binding_of env name in
      let l = entry.loc in

      let b = ((entry.kind = Param)||(entry.kind = Local)) in
      if b then let l1 = l + ps.bp in
      let ptrval = value_at store l1 in
       let ptr = ptrval in 
      ((ps, env, store), ptr)

      else
      let ptrval = value_at store l in
      ((ps, env, store), value_at store ptrval)
  | AddressOf(Id x) ->
          let (Var entry) = binding_of env x in 
          ((ps, env, store), entry.loc)
  | AddressOf(e) -> match e with
      | AddressOf(Id x) -> 
          let (Var entry) = binding_of env x in 
          ((ps, env, store), entry.loc)
      | At (Id e, ei) -> 
          let (Var entry) = binding_of env e in 
          let ((ps', env, store'), index) = eval_expr ei ps env store in
          let l = entry.loc + index in 
          ((ps', env, store'), l)
      | Deref e -> eval_expr e ps env store
      | Id name -> 
          let (Var x) = binding_of env name in 
          let l = x.loc in ((ps, env, store), (l))

          
                   
and allocValues exprs funSymTable ps nps env store : store = match (exprs, funSymTable) with
 | ([], []) -> store
 | ([], _) -> store
 | ((expr1::exprs1),((name, Var entry)::funSymTable1)) ->
    let ((ps', env', store'), e1) = eval_expr expr1 ps env store in
    let l = entry.loc in

    let b2 = ((entry.kind = Param)||(entry.kind = Local)) in
    let b1 = match entry.vtype with
             | Pointer (e) -> true
             | _ -> false
    in
    let b = b1&&b2 in
    if b then 
    let store'' = insert_value store' (l) e1 in
    allocValues exprs1 funSymTable1 ps' nps env store''

    else
    let store'' = insert_value store' (l  + nps.bp) e1 in
    allocValues exprs1 funSymTable1 ps' nps env store''

(* eval_expr: expr -> proc_state -> env -> store -> bool *)
and eval_cond (cond:cond) (ps:proc_state) (env:environment) (store:store) : (interpreter_state * bool) = match cond with
  Equal (e1, e2) ->
    let ((ps', env, store'), r1) = eval_expr e1 ps env store in
    let ((ps'', env, store''), r2) = eval_expr e2 ps env store' in
    ((ps, env, store''), r1 == r2)
 | Less (e1, e2) ->
    let ((ps', env, store'), r1) = eval_expr e1 ps env store in
    let ((ps'', env, store''), r2) = eval_expr e2 ps env store' in
	((ps, env, store''), r1 < r2)
 | And (e1, e2) -> 
    let ((ps', env, store'), x) = eval_cond e1 ps env store in
    if x = false then ((ps, env, store'), false)
                 else let ((ps'', env, store''), y) = eval_cond e2 ps env store' in
                      if y = false then ((ps, env, store''), false)
                                   else ((ps, env, store''), true)
 | Or (e1, e2) -> 
    let ((ps', env, store'), x) = eval_cond e1 ps env store in
    if x = true then ((ps, env, store'), true)
                 else let ((ps'', env, store''), y) = eval_cond e2 ps env store' in
                      if y = true then ((ps, env, store''), true)
                                   else ((ps, env, store''), false)

 | Not (e) -> let ((ps', env, store'), x) = eval_cond e ps env store in ((ps, env, store'), (not x))
 | True -> ((ps, env, store), true)
 | False -> ((ps, env, store), false)


and findSwitchMatch x cases def : stmt list = match cases with
| ((i, stmts)::otherCases) -> if x = i then stmts 
                                       else findSwitchMatch x otherCases def
| [] -> def



(* eval_stmt: stmt -> proc_state -> env -> store -> stmtEvalRes*proc_state*store *)
and eval_stmt (stmt:stmt) (ps:proc_state) (env:environment) (store:store) : (interpreter_state * stmtEvalRes) = match stmt with
| Expr e -> let (is, ival) = eval_expr e ps env store in (is, Next)
| PrintInt e ->
    let (is1, r) = eval_expr e ps env store in
    print_int r; 
    (is1, Next)
| PrintStr s ->
    print_string (Str.global_replace (Str.regexp "\\\\n") "\n" s); 
    (* Escaping characters here because it's not done in the parser *)
    ((ps, env, store), Next)
| List([]) -> ((ps, env, store), Next)
| List (stmt1::stmts) -> 
    let ((ps', env, store'), r) = eval_stmt stmt1 ps env store in
    if r = BreakOut then ((ps', env, store'), BreakOut) 
    else if r = ContinueOn then ((ps', env, store'), ContinueOn) 
         else eval_stmt (List stmts) (ps') (env) (store')
| VarAss(Deref(Add(Id name, Id n)), Add((Deref(Sub(Add(Id name1, Id n1), e1))), (Deref(Sub(Add(Id name2, Id n2), e2))) ) ) -> 
    ((ps, env, store), Next)
| VarAss (Id name, e) ->
    let (Var p) = binding_of env name in
    let l1 = p.loc in
    let ((ps', env, store'), v) = eval_expr e ps env store in
  
    let b = ((p.kind = Param)||(p.kind = Local)) in
    
    if b then let l = l1  + ps.bp in
	    let store'' = insert_value store' l v in
	    ((ps', env, store''), Next)

    else let store'' = insert_value store' l1 v in ((ps', env, store''), Next)

| VarAss (At(Id name, i), e) ->
    let (Var p) = binding_of env name in
    let ((ps', env, store'), index) = eval_expr i ps env store in
    let loc = p.loc + index in
    let ((ps'', env, store''), value) = eval_expr e ps' env store' in

    let b = ((p.kind = Param)||(p.kind = Local)) in
    if b then let loc1 = loc + (ps'').bp  in

    let store''' = insert_value store' loc1 value in
    ((ps'', env, store'''), Next)

    else
    let store''' = insert_value store' loc value in
    ((ps'', env, store'''), Next)
| VarAss(Deref(Id name), e) -> 
    let (Var p) = binding_of env name in
    let ptr = p.loc in
    let l = value_at store ptr in
    let ((ps', env, store'), exp) = eval_expr e ps env store in

    let b = ((p.kind = Param)||(p.kind = Local)) in 
    if b then let l1 = l + (ps').bp in

    let store'' = insert_value store' l1 exp in
    ((ps', env, store''), Next)

    else 
    let store'' = insert_value store' l exp in
    ((ps', env, store''), Next)
| VarAss(At((Deref(Id name)), ind), exp) -> 
    let (Var p) = binding_of env name in
    let ((ps', env, store'), index) = eval_expr ind ps env store in
    let ((ps'', env, store''), value) = eval_expr exp ps' env store' in

    let ptr = p.loc in

    let l1 = value_at store ptr in
    let l2 = l1 - 31 in
    let floc = ptr + index + l2 in
    let store''' = insert_value store'' floc value in
    ((ps'', env, store'''), Next)
| VarAss(Deref(Add(Id name, i)), e) -> 
    let (Var p) = binding_of env name in
    let l = p.loc in

    let ((ps', env, store'), index) = eval_expr i ps env store in
    let ((ps'', env, store''), exp) = eval_expr e ps' env store' in
    
    let b = ((p.kind = Param)||(p.kind = Local)) in
    if b then let l1 = l + (ps'').bp in

    let ptrval = value_at store l1 in
    let ptr = ptrval + index in
    let store''' = insert_value store'' ptr exp in
    ((ps'', env, store'''), Next)

    else
    let ptrval = value_at store l in
    let ptr = ptrval + index in
    let store''' = insert_value store'' ptr exp in
    ((ps'', env, store'''), Next)

    
| IfThen (c, s1) ->
	let ((ps', env, store'), c1) = eval_cond c ps env store in
	if c1 = true
		then eval_stmt s1 ps' env store'
		else ((ps', env, store'), Next)
| IfThenElse (c, s1, s2) ->
        let ((ps', env, store'), c1) = eval_cond c ps env store in
	if c1 = true then eval_stmt s1 ps' env store'
	             else eval_stmt s2 ps' env store'
	     
| Switch(e, cases, def) ->
    let ((ps', env, store'), x) = eval_expr e ps env store in
    let stmts = findSwitchMatch x cases def in
    eval_stmt (List stmts) ps' env store'


| While(c, stmt) ->
    let ((ps', env, store'), c1) = eval_cond c ps env store in
    if c1 = false then ((ps', env, store'), Next)
    else
            let ((ps'', env, store''), r) = processWhileLoop stmt ps' env store' in 
            if r = BreakOut then ((ps'', env, store''), Next)
            else if r = ContinueOn then eval_stmt (While (c, stmt)) ps'' env store'' 
                 else eval_stmt (While (c, stmt)) ps'' env store''
| For(i, c, inc, stmt) -> 
    let ((ps', env, store'), r) = eval_stmt i ps env store in 
    let ((ps'', env, store''), cond) = eval_cond c ps' env store' in
    if cond = false then ((ps'', env, store''), Next)
    else 
        let ((ps'', env, store''), rs) = eval_stmt stmt ps'' env store'' in
        if rs = BreakOut then ((ps'', env, store''), Next)
        else 
            let ((ps''', env, store'''), rs') = eval_stmt inc ps'' env store'' in
            if rs = ContinueOn then eval_stmt (For(Empty, c, inc, stmt)) ps''' env store'''
            else
                eval_stmt (For(Empty, c, inc, stmt)) ps''' env store'''

| Break -> ((ps, env, store), BreakOut)
| Continue -> ((ps, env, store), ContinueOn)
| Empty -> ((ps, env, store), Next)
 
                
and processWhileLoop (stmt:stmt) (ps:proc_state) (env:environment) (store:store) : (interpreter_state * stmtEvalRes) = match stmt with
| Empty -> ((ps, env, store), Next)
| (List []) -> ((ps, env, store), Next)
| (List (stmt1::stmts1)) -> let ((ps', env, store'), st) = eval_stmt stmt1 ps env store in 
                            if st = ContinueOn then ((ps', env, store'), ContinueOn)
                            else if st = BreakOut then ((ps', env, store'), BreakOut)
                                 else processWhileLoop (List stmts1) ps' env store'

;;
