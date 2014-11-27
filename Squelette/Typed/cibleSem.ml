open CibleType

let step ((a: acc), (p: pile), (e: env), (c: code)): (acc * pile * env * code) =
 
let rec run (a, p, e, c) =
  match t with
  | [] -> a
  | _ -> run (step (a, p, e, t)) 

let top t = run (Int 0, [], init_env, t)
