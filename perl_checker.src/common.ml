open Stack
open List

exception Found
exception Not_comparable
exception GraphSort_circular_deps

type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) or_option = Or_some of 'a | Or_error of 'b

(**********************************************************************************)

let internal_error s = failwith ("internal error: " ^ s)

let id x = x
let double a = a,a
let swap (x,y) = (y,x)
let safe_tl l = try tl l with _ -> []
let fstfst ((e, _), _) = e
let sndfst ((_, e), _) = e
let fstsnd (_, (e, _)) = e
let sndsnd (_, (_, e)) = e

let fst3 (e, _, _) = e
let snd3 (_, e, _) = e
let ter3 (_, _, e) = e
let sndter3 (_, a, b) = (a, b)

let o f g x = f (g x)
let curry f x y = f (x,y)
let uncurry f (x, y) = f x y

let is_int n = ceil n = n

let uncons = function
  | [] -> failwith "uncons"
  | e::l -> e,l

let has_env var = 
  try 
    let _ = Sys.getenv var in true
  with Not_found -> false

let some = function 
  | Some e -> e
  | None -> failwith "some"

let some_or = function
  | None -> id
  | Some e -> fun _ -> e

let option2l = function
  | None -> []
  | Some e -> [e]

let prefer_some f a b =
  match a, b with
  | Some a, Some b -> Some (f a b)
  | None, _ -> b
  | _, None -> a

let rec collect_accu f accu = function
  | [] -> accu
  | e::l -> collect_accu f (rev_append (f e) accu) l

let collect f l = rev (collect_accu f [] l)

let merge_some merge a b = 
  match a,b with
  | None, None -> None
  | _, None -> a
  | None, _ -> b
  | Some(a), Some(b) -> Some(merge a b)

let rec uniq = function
  | [] -> []
  | e::l -> if mem e l then uniq l else e :: uniq l

let rec uniq_ eq = function
  | [] -> []
  | e::l -> 
      try 
	let _ = find (eq e) l in
	uniq_ eq l
      with Not_found -> e :: uniq_ eq l

let rec non_uniq = function
  | [] -> []
  | e::l -> if mem e l then e :: non_uniq l else non_uniq l

let rec member_ eq e = function
  | [] -> false
  | e'::l -> if eq e e' then true else member_ eq e l

let rec find_some p = function
  | [] -> raise Not_found
  | x :: l -> 
      match p x with
      |	Some v -> v
      |	None -> find_some p l

let fold_left1 f = function
  | [] -> failwith "fold_left1"
  | e :: l -> fold_left f e l

let find_index e l =
  let rec find_index_ i = function
    | [] -> raise Not_found
    | e'::l -> if e=e' then i else find_index_ (i+1) l
  in
  find_index_ 0 l

let rec find_some_ p = function
  | [] -> None
  | x :: l -> 
      match p x with
      |	Some v -> Some v
      |	None -> find_some_ p l

let rec fpartition p l =
  let rec part yes no = function
  | [] -> (rev yes, rev no)
  | x :: l -> 
      (match p x with
      |	None -> part yes (x :: no) l
      |	Some v -> part (v :: yes) no l) in
  part [] [] l

let partition_either f l =
  let rec part_either left right = function
  | [] -> (rev left, rev right)
  | x :: l -> 
      (match f x with
      |	Left  e -> part_either (e :: left) right l
      |	Right e -> part_either left (e :: right) l) in
  part_either [] [] l

let rec keep_best f = 
  let rec partition e = function
    | [] -> e, []
    | e' :: l ->
	match f(e,e') with
	| None -> let (e'', l') = partition e l in e'', e' :: l'
	| Some e'' -> partition e'' l
  in function
  | [] -> []
  | e::l -> 
      let (e', l') = partition e l in
      e' :: keep_best f l'

let rec keep_bests f l = 
  let rec once e unchanged = function
    | [] -> None
    | e' :: l ->
	match f(e,e') with
	| None -> once e (e' :: unchanged) l
	| Some e'' -> Some(e'', unchanged @ l)
  in
  let rec as_many_as_possible e l =
    match once e [] l with
    | None -> None
    | Some(e', l') -> Some(some_or (as_many_as_possible e' l') (e', l'))
  in
  let rec try_with e l_done l_next =
    match as_many_as_possible e l_next with
    | None -> try_with_next (e :: l_done) l_next
    | Some(e2, l_next2) -> 
	match as_many_as_possible e2 l_done with
	| None -> try_with_next (e2 :: l_done) l_next2
	| Some(e3, l_done2) -> try_with e3 l_done2 l_next2
  and try_with_next l_done = function
    | [] -> rev l_done
    | e::l_next -> try_with e l_done l_next
  in
  try_with_next [] l

let rec fold_right1 f = function
  | [] -> failwith "fold_right1"
  | [e] -> e
  | e::l -> f e (fold_right1 f l)

let rec for_all2_ p l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2_ p l1 l2
  | (_, _) -> false

let rec for_all2_true p l1 l2 =
  match (l1, l2) with
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2_true p l1 l2
  | (_, _) -> true

let maxl l = fold_right1 max l
  
let rec stack2list s =
  let l = ref [] in
  Stack.iter (fun e -> l := e :: !l) s ;
  !l
  
let rec stack_exists f s =
  try
    Stack.iter (fun e -> if f e then raise Found) s ;
    false
  with Found -> true

let rec queue2list q = rev (Queue.fold (fun b a -> a :: b) [] q)

let rec fix_point f p =
  let p' = f p in
  if p = p' then p else fix_point f p'

let rec fix_point_withenv f env p =
  let p', env' = f env p in
  if p = p' then (p, env') else fix_point_withenv f env' p'

let rec fix_point_ nb f p =
  let p' = f p in
  if p = p' then p, nb else fix_point_ (nb+1) f p'

let rec group_by_2 = function
  | [] -> []
  | a :: b :: l -> (a, b) :: group_by_2 l
  | _ -> failwith "group_by_2"

(*
let rec lfix_point f e =
  let e' = f(e) in
  if e = e' then e :: lfix_point f e' else [e]
*)

let fluid_let ref value f =
  let previous_val = !ref in
  ref := value ;
  let v = f() in
  ref := previous_val ;
  v

let do0_withenv doit f env l =
  let r_env = ref env in
  doit (fun e -> r_env := f !r_env e) l ;
  !r_env

let do0_withenv2 doit f env l =
  let r_env = ref env in
  doit (fun e e' -> r_env := f !r_env e e') l ;
  !r_env

let do_withenv doit f env l =
  let r_env = ref env in
  let l' = doit (fun e -> 
    let e', env' = f !r_env e in
    r_env := env' ; e'
  ) l in
  l', !r_env

let do2_withenv doit f env l1 l2 =
  let r_env = ref env in
  let l' = doit (fun e1 e2 -> 
    let e', env' = f !r_env e1 e2 in
    r_env := env' ; e'
  ) l1 l2 in
  l', !r_env

let do_collect doit f l1 =
  let l = ref [] in
  doit (fun i t -> l := f i t @ !l) l1 ;
  !l

let map_withitself f l =
  let rec map_withitself_ done_ = function
    | [] -> done_
    | e :: l -> 
	let e' = f (done_ @ e :: l) e in
	map_withitself_ (done_ @ [ e' ]) l
  in map_withitself_ [] l

let map_t2 f (x,y) = f x, f y
let map_t3 f (x,y,z) = f x, f y, f z
let map_option f = function
  | Some e -> Some (f e)
  | None -> None
let map_optionoption f = function
  | Some e -> f e
  | None -> None
let t2_option2option_t2 = function
  | (Some x, Some y) -> Some(x,y)
  | _ -> None
let rec l_option2option_l = function
  | [] -> Some []
  | None :: _l -> None
  | Some e :: l -> map_option (fun l -> e :: l) (l_option2option_l l)
let map_option_env f (e, env) = map_option f e, env

let t2_to_list (a,b) = [ a ; b ]
let t3_to_list (a,b,c) = [ a ; b ; c ]

let if_some bool val_ = if bool then Some val_ else None

let rec fold_left_option f val_ = function
  | [] -> Some val_
  | e::l ->
      match f val_ e with
      |	None -> None
      |	Some val_' -> fold_left_option f val_' l

let collect_some_withenv f env l = 
  let rec collect accu env = function
    | [] -> rev accu, env
    | e::l -> 
	let e', env' = f env e in
	let accu' = 
	  match e' with 
	  | Some e' -> e'::accu 
	  | None -> accu in
	collect accu' env' l
  in collect [] env l
	  
let for_all_option_withenv remap f env l =
  let rec for_all env accu = function
    | [] -> Some(remap (rev accu)), env
    | e::l ->
	(match f env e with
	| None, env' -> None, env'
	| Some e', env' -> for_all env' (e' :: accu) l)
  in
  for_all env [] l

let for_all2_option_withenv remap f env la lb =
  let rec for_all env accu = function
    | [], [] -> Some(remap (rev accu)), env
    | a::la, b::lb ->
	(match f env a b with
	| None, env' -> None, env'
	| Some ab, env' -> for_all env' (ab :: accu) (la, lb))
    | _ -> None, env
  in
  for_all env [] (la, lb)

let map_or_option f = function
  | Or_some e -> Or_some (f e)
  | Or_error err -> Or_error err

let map_index f l =
  let rec map_ n = function
    | [] -> []
    | e::l -> f e n :: map_ (n+1) l
  in map_ 0 l

let filter_index f l =
  let rec filter_ n = function
    | [] -> []
    | e::l -> 
	let l' = filter_ (n+1) l in
	if f e n then e :: l' else l'
  in filter_ 0 l

let iter_index f l =
  let rec iter_ n = function
    | [] -> ()
    | e::l -> f e n ; iter_ (n+1) l
  in iter_ 0 l

let map_fst f (x, y) = f x, y
let map_snd f (x, y) = x, f y

let map_withenv      f env e = do_withenv map f env e
let find_withenv     f env e = do_withenv find f env e
let filter_withenv   f env e = do_withenv filter f env e
let exists_withenv   f env e = do_withenv exists f env e
let map_t2_withenv   f env e = do_withenv map_t2 f env e
let for_all_withenv  f env e = do_withenv for_all f env e
let collect_withenv  f env e = do_withenv collect f env e
let partition_either_withenv f env e = do_withenv partition_either f env e

let map2_withenv     f env l1 l2 = do2_withenv map2 f env l1 l2
let for_all2_withenv f env l1 l2 = do2_withenv for_all2 f env l1 l2

let rec take n l =
  if n = 0 then []
  else match l with
  | [] -> raise Not_found
  | e::l -> e :: take (n-1) l
let last_n n l = rev (take n (rev l))
let last l = hd (last_n 1 l)

let rec skipfirst e = function
  | [] -> []
  | e'::l when e = e' -> skipfirst e l
  | l -> l

let rec removelast = function
  | [] -> failwith "removelast"
  | [_] -> []
  | e::l -> e :: removelast l

let rec split_last l = 
  let rec spl accu = function
  | [] -> failwith "split_last"
  | [e] -> rev accu, e
  | e::l -> spl (e :: accu) l
  in spl [] l

let iter_assoc_val f l = iter (fun (_,v) -> f v) l
let map_assoc_val f l = map (fun (k,v) -> k, f v) l

let assoc_or_fail e l =
  try assoc e l with Not_found -> failwith "assoc failed"

let assoc_by is_same e l = 
  find_some (fun (a,b) -> if is_same e a then Some b else None) l

let rec update_assoc_by is_same f e = function
  | [] -> raise Not_found
  | (a,b) :: l when is_same e a -> (a, f b) :: l
  | (a,b) :: l -> (a,b) :: update_assoc_by is_same f e l

let update_assoc f e = update_assoc_by (=) f e

let rec update_assoc_by_with_default default is_same f e = function
  | [] -> [ e, f default ]
  | (a,b) :: l when is_same e a -> (a, f b) :: l
  | (a,b) :: l -> (a,b) :: update_assoc_by_with_default default is_same f e l

let update_all_assoc_by is_same f e l =
  map (fun (a,b) -> a, if is_same e a then f b else b) l

let rec rassoc e = function
  | [] -> raise Not_found
  | (k,v) :: l -> if e = v then k else rassoc e l

let rec all_assoc e = function
  | [] -> []
  | (e',v) :: l when e=e' -> v :: all_assoc e l
  | _ :: l -> all_assoc e l

let rec all_assoc_by is_same e = function
  | [] -> []
  | (e',v) :: l when is_same e e' -> v :: all_assoc_by is_same e l
  | _ :: l -> all_assoc_by is_same e l

let prepare_want_all_assoc l =
  map (fun n -> n, uniq (all_assoc n l)) (uniq (map fst l))

let prepare_want_all_assoc_by is_same l =
  map (fun n -> n, uniq_ is_same (all_assoc_by is_same n l)) (uniq_ is_same (map fst l))

let prepare_want_all_assoc_by_ is_same_a is_same_b l =
  map (fun n -> n, uniq_ is_same_b (all_assoc_by is_same_a n l)) (uniq_ is_same_a (map fst l))

let rec count_uniq = function
  | [] -> []
  | e::l -> 
      let has, l' = partition ((=) e) l in
      (e, length has + 1) :: count_uniq l'

let rec repeat e = function
  | 0 -> []
  | n -> e :: repeat e (n-1)

let rec inits = function
  | [] -> [[]]
  | e::l -> [] :: map (fun l -> e::l) (inits l)
let rec tails = function
  | [] -> [[]]
  | (_::xs) as xxs -> xxs :: tails xs

let apply f x = f x;;

let rec map3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> []
  | (a1::l1, a2::l2, a3::l3) -> let r = f a1 a2 a3 in r :: map3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "map3"

let filter2 f l1 l2 =
  split (filter f (combine l1 l2))

let break_at f l =
  let rec b l1 = function
  | [] -> l1, []
  | e::l2 -> if f e then (l1, e :: l2) else b (l1 @ [e]) l2
  in b [] l
let break v l = break_at ((=) v) l

let drop_while f l = snd (break_at (fun e -> not (f e)) l)

(* break_at_indice 0 [1;2] gives [], [1;2]
   break_at_indice 1 [1;2] gives [1], [2]
 *)
let rec break_at_indice i l =
  if i = 0 then [], l else
   match l with
   | [] -> raise Not_found
   | e::l2 ->
     let a, b = break_at_indice (i-1) l2 in
     e::a, b

let rev_nth e l =
  let rec rev_nth' i = function
  | [] -> raise Not_found
  | e'::_ when e'=e -> i
  | _::l -> rev_nth' (i+1) l
  in  rev_nth' 0 l

let rec getset_nth l i f =
  match l, i with
  | e::l', 0 -> f e :: l'
  | [], _ -> failwith "getset_nth"
  | e::l', _ -> e :: getset_nth l' (i - 1) f

let set_nth l i v = getset_nth l i (fun _ -> v)
    
let adjustModDown m n = n - (n mod m)
let adjustModUp m n = adjustModDown m (n + m - 1)


let hashtbl_find f h =
  let r = ref None in
  Hashtbl.iter (fun v c -> if f v c then r := Some v) h ;
  match !r with
  | Some v -> v
  | None -> raise Not_found

let hashtbl_map f h = Hashtbl.iter (fun v c -> Hashtbl.replace h v (f v c)) h

let hashtbl_values  h = Hashtbl.fold (fun _ v l -> v :: l) h []
let hashtbl_keys    h = Hashtbl.fold (fun k _ l -> k :: l) h []
let hashtbl_to_list h = Hashtbl.fold (fun k v l -> (k,v) :: l) h []

let hashtbl_collect f h =
  rev (Hashtbl.fold (fun k v l -> rev_append (f k v) l) h [])

let hashtbl_exists f h =
  try
    Hashtbl.iter (fun v c -> if f v c then raise Found) h ;
    false
  with Found -> true

let memoize f =
  let hash = Hashtbl.create 16 in
  fun k ->
    try Hashtbl.find hash k
    with Not_found ->
      let v = f k in
      Hashtbl.add hash k v ; v

let array_shift a = Array.sub a 1 (Array.length a - 1)
let array_last_n n a = 
  let len = Array.length a in
  Array.sub a (len - n) n

let array_collect f a = Array.fold_left (fun l e -> f e @ l) [] a

let rec lvector_product = 
  let rec vector_product a b = match a with
  | [] -> []
  | e::l -> map (fun e' -> e :: e') b :: vector_product l b
  in function
  | [] -> []
  | [e] -> map (fun e -> [e]) e
  | e::l -> flatten (vector_product e (lvector_product l))

let vector_product2 a b =
  map (function
  | [a;b] -> a,b
  | _ -> failwith "vector_product2"
  ) (lvector_product [ a ; b ])

let rec transpose = function
  | [] :: _ -> []
  | ll -> 
      let l, ll' = split (map (function e::l -> e,l | _ -> raise Not_found) ll) in
      l :: transpose ll'

let rec range min max =
  if min >= max then [] else min :: range (min + 1) max

let sum l = List.fold_left (+) 0 l

let rec filter_some_with f = function
  | [] -> []
  | e :: l -> 
      match f e with
      |	None -> filter_some_with f l
      | Some e' -> e' :: filter_some_with f l

let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l

let rec difference l = function
  | [] -> l
  | e::l' -> difference (filter ((<>) e) l) l'

let rec difference_ eq l = function
  | [] -> l
  | e::l' -> 
      let l2 = filter (fun e' -> not (eq e e')) l in
      difference_ eq l2 l'

let intersection_by is_same l1 l2 = filter (fun e -> exists (is_same e) l2) l1

let intersection_and_differences eq l1 l2 =
  let rec both inter l2_only = function
    | [], l2 -> inter, [], rev l2_only @ l2
    | l1, [] -> inter, l1, rev l2_only
    | l1, e2 :: l2' ->
	match partition (eq e2) l1 with
	| [], _ -> both inter (e2 :: l2_only) (l1, l2')
	| _, l1' -> both (e2 :: inter) l2_only (l1', l2')
  in both [] [] (l1, l2)

let rec triangularize = function
  | [] -> []
  | e::l -> (e,l) :: triangularize l

let diagonalize l =
  map_index (fun a i ->
    a, filter_index (fun _ j -> i <> j) l
  ) l

let rec list_of_nonempty_sublists = function
  | [] -> []
  | e :: l -> 
      let l' = list_of_nonempty_sublists l in
      [e] :: l' @ map (fun l -> e :: l) l'

let rec graph_is_sorted_by eq = function
  | [] -> true
  | (_,deps) :: l -> 
      for_all (fun e -> try let _ = assoc_by eq e l in false with Not_found -> true) deps && graph_is_sorted_by eq l

let graph_closure_by eq graph =
  let err = ref None in
  try
    let graph_rev = collect (fun (i, l) -> map (fun e -> (e, i)) l) graph in
    let bothway = map (fun (i,l) -> i, (l, all_assoc_by eq i graph_rev)) graph in
    let closed = fold_left (fun graph j ->
        let next, prev = assoc_by eq j graph in
        let graph2 = fold_left (fun graph i ->
        if member_ eq i next then (err := Some(j,i); raise GraphSort_circular_deps) else
        update_assoc_by eq (fun (i_next,i_prev) -> i_next @ next, i_prev) i graph
      ) graph (filter (fun a -> not (eq a j)) prev) in
      let graph3 = fold_left (fun graph k ->
        if member_ eq k prev then (err := Some(j,k); raise GraphSort_circular_deps) else
        update_assoc_by eq (fun (k_next,k_prev) -> k_next, k_prev @ prev) k graph
      ) graph2 (filter (fun a -> not (eq a j)) next) in
      graph3
    ) bothway (map fst bothway) in
    Or_some (map (fun (e,(next,_)) -> e, uniq_ eq next) closed)
  with GraphSort_circular_deps ->
    Or_error (some !err)

let rec graph_sort_by eq l =
  let cmp (_, deps_a) (b, _) = if member_ eq b deps_a then 1 else -1 in 
  let rec sort_it = function
    | [] -> []
    | [e] -> [e]
    | e::l ->
	let l' = sort_it l in
	let gt, lt = break_at (fun ((_, deps) as e') -> deps = [] or cmp e e' = 1) l' in
	gt @ [e] @ lt
  in 
  map_or_option (fun l' ->
    let l_sorted = rev (sort_it l') in
    if not (graph_is_sorted_by eq l_sorted) then internal_error "graph_sort failed" else
    l_sorted
  ) (graph_closure_by eq l)

let int_sort l = sort (fun a b -> a - b) l

let str_begins_with prefix s =
  String.sub s 0 (min (String.length s) (String.length prefix)) = prefix

let rec strstr s subs =
  let len_s, len_subs = String.length s, String.length subs in
  let rec rec_ i =
    let i' = String.index_from s i subs.[0] in
    if i' + len_subs <= len_s then
      if String.sub s i' len_subs = subs then
	i'
      else
	rec_ (i' + 1)
    else
      raise Not_found
  in
  rec_ 0

let str_contains s subs =
  try
    let _ = strstr s subs in true
  with Not_found -> false

let str_ends_with s suffix =
  let len = min (String.length s) (String.length suffix) in
  String.sub s (String.length s - len) len = suffix

let chop = function
  | "" -> ""
  | s -> String.sub s 0 (String.length s - 1)

let chomps s =
  let i = ref (String.length s - 1) in
  while !i >= 0 && (s.[!i] = ' ' || s.[!i] = '\t') do decr i done ;
  String.sub s 0 (!i+1)

let rec times e = function
  | 0 -> []
  | n -> e :: times e (n-1)

let skip_n_char_ beg end_ s =
  let full_len = String.length s in
  if beg < full_len && full_len - beg - end_ > 0 
  then String.sub s beg (full_len - beg - end_)
  else ""
let skip_n_char n s = skip_n_char_ n 0 s

let rec non_index_from s beg c =
  if s.[beg] = c then non_index_from s (beg+1) c else beg
let non_index s c = non_index_from s 0 c

let rec non_rindex_from s beg c =
  if s.[beg] = c then non_rindex_from s (beg-1) c else beg
let non_rindex s c = non_rindex_from s (String.length s - 1) c

let rec explode_string = function
  | "" -> []
  | s -> (String.get s 0) :: explode_string (String.sub s 1 (String.length s - 1))

let count_matching_char s c =
  let rec count_matching_char_ nb i =
    try
      let i' = String.index_from s i c in
      count_matching_char_ (nb+1) (i'+1)
    with Not_found -> nb
  in
  count_matching_char_ 0 0

let is_uppercase c = Char.lowercase c <> c
let is_lowercase c = Char.uppercase c <> c

let char_is_alphanumerical c =
  let i = Char.code c in
  Char.code 'a' <= i && i <= Char.code 'z' ||
  Char.code 'A' <= i && i <= Char.code 'Z' ||
  Char.code '0' <= i && i <= Char.code '9'

let char_is_alphanumerical_ c =
  let i = Char.code c in
  Char.code 'a' <= i && i <= Char.code 'z' ||
  Char.code 'A' <= i && i <= Char.code 'Z' ||
  Char.code '0' <= i && i <= Char.code '9' || c = '_'

let char_is_alpha c =
  let i = Char.code c in
  Char.code 'a' <= i && i <= Char.code 'z' ||
  Char.code 'A' <= i && i <= Char.code 'Z'

let char_is_number c =
  let i = Char.code c in
  Char.code '0' <= i && i <= Char.code '9'

let count_chars_in_string s c =
  let rec rec_count_chars_in_string from =
    try
      let from' = String.index_from s from c in
      1 + rec_count_chars_in_string (from' + 1)
    with
      Not_found -> 0
  in rec_count_chars_in_string 0

let rec string_fold_left f val_ s =
  let val_ = ref val_ in
  for i = 0 to String.length s - 1 do
    val_ := f !val_ s.[i]
  done ;
  !val_

(*
let rec string_forall_with f i s =
  try
    f s.[i] && string_forall_with f (i+1) s
  with Invalid_argument _ -> true
*)
let string_forall_with f i s =
  let len = String.length s in
  let rec string_forall_with_ i =
    i >= len || f s.[i] && string_forall_with_ (i+1)
  in string_forall_with_ i

let starts_with_non_lowercase s = s <> "" && s.[0] <> '_' && not (is_lowercase s.[0])

let rec fold_lines f init chan =
  try 
    let line = input_line chan in 
    fold_lines f (f init line) chan
  with End_of_file -> init
let readlines chan = List.rev (fold_lines (fun l e -> e::l) [] chan)

let split_at c s =
  let rec split_at_ accu i =
    try
      let i' = String.index_from s i c in
      split_at_ (String.sub s i (i' - i) :: accu) (i'+1)
    with Not_found -> rev (skip_n_char i s :: accu)
  in
  split_at_ [] 0

let split_at2 c1 c2 s =
  let rec split_at2_ accu i i2 =
    try
      let i3 = String.index_from s i2 c1 in
      if s.[i3+1] = c2 then split_at2_ (String.sub s i (i3 - i) :: accu) (i3+2) (i3+2) else
      split_at2_ accu i i3
    with Not_found | Invalid_argument _ -> rev (skip_n_char i s :: accu)
  in
  split_at2_ [] 0 0

let words s =
  let rec words_ accu i s =
    try
      let i2 = non_index_from s i ' ' in
      try
	let i3 = String.index_from s i2 ' ' in
	words_ (String.sub s i2 (i3 - i2) :: accu) (i3+1) s
      with Not_found -> rev (skip_n_char i2 s :: accu)
    with Invalid_argument _ -> rev accu
  in
  collect (words_ [] 0) (split_at '\n' s)

let to_CamelCase s_ =
  let l = ref [] in
  let s = String.copy s_ in
  for i = 1 to String.length s - 1 do 
      if is_uppercase (String.unsafe_get s i) && is_lowercase (String.unsafe_get s (i-1)) then (
	String.set s i (Char.lowercase (String.get s i)) ;
        l := i :: !l
      )
  done ;
  if !l = [] then None else
  let offset, s' = fold_left (fun (offset, s') i -> 
    i, s' ^ String.sub s offset (i-offset) ^ "_"
  ) (0, "") (rev !l) in
  Some (s' ^ String.sub s offset (String.length s - offset))

let concat_symlink file link =
  if str_begins_with "..//" link then (* ..//foo => /foo *)
    skip_n_char 3 link
  else
    let file = if str_ends_with file "/" then chop file else file in (* s|/$|| *)
    let rec reduce file link =
      if str_begins_with "../" link then
	let file = String.sub file 0 (String.rindex file '/') in (* s|/[^/]+$|| *)
	reduce file (skip_n_char 3 link)
      else
	file ^ "/" ^ link
    in
    reduce file link

let expand_symlinks file =
  match split_at '/' file with
  | "" :: l ->
      let rec remove_dotdot accu nb = function
	| [] -> if nb = 0 then accu else failwith "remove_dotdot"
	| ".." :: l -> remove_dotdot accu (nb + 1) l
	| e :: l -> if nb > 0 then remove_dotdot accu (nb - 1) l else remove_dotdot (e :: accu) nb l
      in
      let l = remove_dotdot [] 0 (List.rev l) in
      List.fold_left (fun file piece ->
	fix_point (fun file -> 
	  try concat_symlink file ("../" ^ Unix.readlink file) 
	  with _ -> file
        ) (file ^ "/" ^ piece)) "" l
  | _ -> internal_error (Printf.sprintf "expand_symlinks: %s is relative\n" file)

let mtime f = (Unix.stat f).Unix.st_mtime

let rec updir dir nb =
  if nb = 0 then dir else
  match dir with
  | "." -> String.concat "/" (times ".." nb)
  | _ -> 
      if Filename.basename dir = ".." then
	dir ^ "/" ^ String.concat "/" (times ".." nb)
      else
	updir (Filename.dirname dir) (nb-1)

let (string_of_ref : 'a ref -> string) = fun r ->
  Printf.sprintf "0x%x" (Obj.magic r : int)

let print_endline_flush s = print_endline s ; flush stdout

let is_int n = n = floor n

(* total order *)
let rec compare_lists cmp l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | e1::l1, e2::l2 ->
      match cmp e1 e2 with
      |	0 -> compare_lists cmp l1 l2
      |	v -> v

let compare_best a b =
  match a, b with
  |  0,  0 -> 0
  |  1,  1 |  1, 0 | 0,  1 ->  1
  | -1, -1 | -1, 0 | 0, -1 -> -1
  |  1, -1 | -1, 1 -> raise Not_comparable
  | _ -> failwith "uh?"

(* partial order *)
let combine_comparison_list l =
  fold_left compare_best 0 l

let min_with_cmp less_than a b =
  if less_than a b then a
  else if less_than b a then b
  else raise Not_comparable

let max_with_cmp less_than a b =
  if less_than a b then b
  else if less_than b a then a
  else raise Not_comparable

let rec fold_left2_compare f e l1 l2 =
  match l1, l2 with
  | [], [] -> e
  | e1::l1, e2::l2 -> fold_left2_compare f (f e e1 e2) l1 l2
  | _ -> raise Not_comparable

let rec exists_compare cmp = function
  | [] -> raise Not_comparable
  | e :: l -> try cmp e with Not_comparable -> exists_compare cmp l

let forall_compare cmp = fold_left (fun n e -> compare_best n (cmp e)) 0
let forall2_compare cmp = fold_left2_compare (fun n e1 e2 -> compare_best n (cmp e1 e2)) 0

let exists2_compare left_dropping cmp l1 l2 =
  let rec forall_compare_ n = function
    | [], [] -> n
    | _, [] -> compare_best n left_dropping
    | [], _ -> compare_best n (-left_dropping)
    | e1::l1, e2::l2 ->
	match try Some (cmp e1 e2) with Not_comparable -> None with
	| Some n' -> forall_compare_ (compare_best n n') (l1, l2)
	| None ->
	    if n = left_dropping then
	      forall_compare_ left_dropping (l1, e2::l2)
	    else if n = -left_dropping then
	      forall_compare_ (-left_dropping) (e1::l1, l2)
	    else
	      (* need to try both *)
	      try forall_compare_ left_dropping (l1, e2::l2)
	      with Not_comparable -> forall_compare_ (-left_dropping) (e1::l1, l2)
  in forall_compare_ 0 (l1, l2)


let rec compare_sorted_sets is_same l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | e1::l1, e2::l2 -> if is_same e1 e2 then compare_sorted_sets is_same l1 l2 else raise Not_found

let scan_list_while_modifying f l =
  let rec scan_list_while_modifying_ prev = function
    | [] -> prev
    | e :: next -> 
	let prev', next' = some_or (f prev next e) (prev @ [e], next) in
	scan_list_while_modifying_ prev' next'
  in scan_list_while_modifying_ [] l

let bools2compare = function
  | true, true -> 0
  | true, false -> -1
  | false, true -> 1
  | _ -> raise Not_comparable

let lpush l e = l := e :: !l

(*
let is_greater2compare is_greater a b =
  match is_greater a b, is_greater b a with

  *)

module OrderedString =
  struct
    type t = string
    let compare = compare
  end;;

module StringSet = Set.Make(OrderedString);;

let stringSet_to_list = StringSet.elements
let stringSet_add set e = StringSet.add e set
let stringSet_difference = StringSet.diff
let list_to_StringSet l = fold_left stringSet_add StringSet.empty l

(* this character messes emacs caml mode *)
let char_quote = '"'
