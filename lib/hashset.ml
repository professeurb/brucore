type 'a t = {
  mutable len : int;
  mutable carrier : (int * 'a) array;
  mutable has_zero : bool;
  hash : 'a -> int;
}

let init ?(hash = Hashtbl.hash) () =
  {
    len = 0;
    carrier = Array.make 16 (Obj.magic 0);
    has_zero = false;
    hash;
  }

let length t = t.len

let position arr h e =
  let n = Array.length arr in
  let p = h mod n in
  let rec aux p =
    if Obj.magic arr.(p) = 0 then (false, p)
    else
      let ha, a = arr.(p) in
      if ha = h && a = e then (true, p)
      else aux ((p + 1) mod n)
  in
  if p >= 0 then aux p else aux (p + n)

let mem t e =
  if Obj.magic e = 0 then t.has_zero
  else
    let b, _ = position t.carrier (t.hash e) e in
    b

let ungarded_add t h e =
  match position t h e with
  | true, _ -> ()
  | false, p -> t.(p) <- (h, e)

let add t e =
  if Obj.magic e = 0 then t.has_zero <- true
  else begin
    let n = Array.length t.carrier in
    if 2 * t.len >= n then begin
      let new_carrier = Array.make (2 * n) (Obj.magic 0) in
      for i = 0 to n - 1 do
        if Obj.magic t.carrier.(i) <> 0 then
          let ha, a = t.carrier.(i) in
          ungarded_add new_carrier ha a
      done;
      t.carrier <- new_carrier
    end;
    let he = t.hash e in
    ungarded_add t.carrier he e;
    t.len <- 1 + t.len
  end

let to_list t =
  let n = Array.length t.carrier
  and l = ref [] in
  if t.has_zero then l := Obj.magic 0 :: !l;
  for i = 0 to n - 1 do
    if Obj.magic t.carrier.(i) <> 0 then
      let _, e = t.carrier.(i) in
      l := e :: !l
  done;
  !l

let iter f t =
  let n = Array.length t.carrier in
  if t.has_zero then f (Obj.magic 0);
  for i = 0 to n - 1 do
    if Obj.magic t.carrier.(i) <> 0 then
      let _, e = t.carrier.(i) in
      f e
  done

let fold f e t =
  let n = Array.length t.carrier in
  let r = ref e in
  if t.has_zero then r := f e (Obj.magic 0);
  for i = 0 to n - 1 do
    if Obj.magic t.carrier.(i) <> 0 then
      let _, e = t.carrier.(i) in
      r := f !r e
  done;
  !r
