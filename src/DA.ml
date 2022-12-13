type 'a t = { mutable len : int; mutable car : 'a array }

exception Empty

let init () = { len = 0; car = Array.make 16 (Obj.magic 0) }
let is_empty t = t.len = 0
let length t = t.len
let carrier t = t.car

let push v t =
  if t.len = Array.length t.car then begin
    let new_car = Array.make (2 * t.len) (Obj.magic 0) in
    for i = 0 to t.len - 1 do
      new_car.(i) <- t.car.(i)
    done;
    t.car <- new_car
  end;
  t.car.(t.len) <- v;
  t.len <- t.len + 1

let pop t =
  if t.len = 0 then raise Empty
  else begin
    t.len <- t.len - 1;
    let v = t.car.(t.len) in
    if
      Array.length t.car >= 2
      && t.len <= Array.length t.car / 4
    then begin
      let new_car =
        Array.make (Array.length t.car / 2) (Obj.magic 0)
      in
      for i = 0 to t.len - 1 do
        new_car.(i) <- t.car.(i)
      done;
      t.car <- new_car
    end;
    v
  end

let pop_opt t =
  if t.len = 0 then None
  else begin
    t.len <- t.len - 1;
    let v = t.car.(t.len) in
    if
      Array.length t.car >= 2
      && t.len <= Array.length t.car / 4
    then begin
      let new_car =
        Array.make (Array.length t.car / 2) (Obj.magic 0)
      in
      for i = 0 to t.len - 1 do
        new_car.(i) <- t.car.(i)
      done;
      t.car <- new_car
    end;
    Some v
  end

let top t =
  if t.len = 0 then raise Empty else t.car.(t.len - 1)

let top_opt t =
  if t.len = 0 then None else Some t.car.(t.len - 1)

let get t i =
  if i < t.len then t.car.(i)
  else raise (Invalid_argument "index out of bounds")

let iter f t =
  let carrier = t.car in
  for i = 0 to t.len - 1 do
    f carrier.(i)
  done

let iteri f t =
  let carrier = t.car in
  for i = 0 to t.len - 1 do
    f i carrier.(i)
  done

let fold f s t =
  let acc = ref s
  and carrier = t.car in
  for i = 0 to t.len - 1 do
    acc := f !acc carrier.(i)
  done;
  !acc

(* let of_stream s = *)
(*   let da = init () in *)
(*   Stream.iter (fun x -> push x da) s; *)
(*   da *)
(**)
let to_array t =
  let carrier = t.car in
  Array.init t.len (fun i -> carrier.(i))

let rec sieve_up cmp arr pos elt =
  if pos > 0 then
    let par = (pos - 1) / 2 in
    let par_elt = arr.(par) in
    if cmp par_elt elt > 0 then begin
      arr.(pos) <- par_elt;
      sieve_up cmp arr par elt
    end
    else arr.(pos) <- elt
  else arr.(pos) <- elt

let rec sieve_down cmp arr pos len elt =
  (* Printf.printf "pos:%d(%d) elt:%d\n" pos len elt; *)
  let curr = ref pos in
  let right = (2 * pos) + 2 in
  if right < len then begin
    if cmp arr.(right - 1) arr.(right) > 0 then begin
      if cmp elt arr.(right) > 0 then curr := right
    end
    else begin
      if cmp elt arr.(right - 1) > 0 then curr := right - 1
    end
  end
  else if right = len && cmp elt arr.(right - 1) > 0 then
    curr := right - 1;
  if !curr <> pos then begin
    arr.(pos) <- arr.(!curr);
    sieve_down cmp arr !curr len elt
  end
  else arr.(pos) <- elt

let heap_push cmp v t =
  let s = t.len in
  push v t;
  sieve_up cmp t.car s v

let heap_pop cmp t =
  if t.len = 0 then raise Empty;
  let res = t.car.(0) in
  let elt = pop t in
  sieve_down cmp t.car 0 t.len elt;
  res

let heap_pop_opt cmp t =
  if t.len = 0 then None
  else begin
    let res = t.car.(0) in
    let elt = pop t in
    t.car.(0) <- elt;
    sieve_down cmp t.car 0 t.len elt;
    Some res
  end
