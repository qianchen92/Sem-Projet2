open CibleType

let step ((a: acc), (p: stack), (e: env), (c: code)): (acc * stack * env * code) =
  assert(false)
 
let rec run (a, p, e, c) =
  match c with
  | [] -> a
  | _ -> run (step (a, p, e, c)) 

let top t = run (Int 0, [], init_env, t)
