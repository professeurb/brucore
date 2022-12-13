type ('elt, 'container) iterator =
  ('elt -> unit) -> 'container -> unit

type 'elt t = unit -> 'elt option

let generate (type elt) (i : (elt, 'container) iterator)
    (c : 'container) : elt t =
  let open Effect in
  let open Effect.Shallow in
  let module M = struct
    type _ Effect.t += Yield : elt -> unit Effect.t

    type ('a, 'b) status =
      | NotStarted
      | InProgress of ('a, 'b) continuation
      | Finished
  end in
  let open M in
  let yield v = perform (Yield v) in
  let curr_status = ref NotStarted in
  let rec helper () =
    match !curr_status with
    | Finished -> None
    | NotStarted ->
        curr_status :=
          InProgress (fiber (fun () -> i yield c));
        helper ()
    | InProgress k ->
        continue_with k ()
          {
            retc =
              (fun _ ->
                curr_status := Finished;
                helper ());
            exnc = (fun e -> raise e);
            effc =
              (fun (type b) (eff : b Effect.t) ->
                match eff with
                | Yield x ->
                    Some
                      (fun (k : (b, _) continuation) ->
                        curr_status := InProgress k;
                        Some x)
                | _ -> None);
          }
  in
  helper

let of_list l = generate List.iter l
let of_array a = generate Array.iter a
let of_fun f = generate (fun yield _ -> f yield) ()

let of_file file =
  let input = open_in file in
  let rec aux yield =
    let l =
      try Some (input_line input)
      with End_of_file ->
        close_in input;
        None
    in
    match l with
    | Some l ->
        yield l;
        aux yield
    | None -> ()
  in
  of_fun aux

let rec iter f g =
  match g () with
  | None -> ()
  | Some v ->
      f v;
      iter f g

let rec iteri ?(pos = 0) f g =
  match g () with
  | None -> ()
  | Some v ->
      f pos v;
      iteri ~pos:(pos + 1) f g

let map f g =
  let rec aux yield =
    match g () with
    | None -> ()
    | Some v ->
        yield (f v);
        aux yield
  in
  of_fun aux

let mapi ?(pos = 0) f g =
  let cnt = ref pos in
  let rec aux yield =
    match g () with
    | None -> ()
    | Some v ->
        yield (f !cnt v);
        incr cnt;
        aux yield
  in
  of_fun aux

let fold f s t =
  let acc = ref s in
  iter (fun v -> acc := f !acc v) t;
  !acc

let filter p t =
  let rec aux yield =
    match t () with
    | None -> ()
    | Some v ->
        if p v then yield v;
        aux yield
  in
  of_fun aux

let tap f g =
  let rec aux yield =
    match g () with
    | None -> ()
    | Some v ->
        f v;
        yield v;
        aux yield
  in
  of_fun aux

let genfold ?(init = false) f a t =
  let acc = ref a in
  of_fun (fun yield ->
      if init then yield a;
      iter
        (fun v ->
          let acc' = f !acc v in
          yield acc';
          acc := acc')
        t)
