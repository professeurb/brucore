type 'a t = { mutable len : int; mutable car : 'a array }

exception Empty

let init () = { len = 0; car = Array.make 16 (Obj.magic 0) }
let is_empty t = t.len = 0
let length t = t.len

let push t v =
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

let set t i v =
  if i < t.len then t.car.(i) <- v
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

let to_array t =
  let carrier = t.car in
  Array.init t.len (fun i -> carrier.(i))

module Unsafe = struct
  let carrier t = t.car
end
