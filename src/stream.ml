include Stdlib.Stream

let of_file file =
  let input = open_in file in
  let streamer _ =
    try Some (input_line input) with
    | End_of_file ->
        close_in input;
        None
    | e ->
        close_in input;
        raise e
  in
  from streamer

let scan_file file patt action =
  let input = Scanf.Scanning.open_in file in
  let streamer _ =
    try Some (Scanf.bscanf input patt action) with
    | End_of_file ->
        Scanf.Scanning.close_in input;
        None
    | e ->
        Scanf.Scanning.close_in input;
        raise e
  in
  from streamer

let to_list s =
  let l = ref [] in
  iter (fun e -> l := e :: !l) s;
  !l

let to_array s =
  let l = ref []
  and cnt = ref 0 in
  iter
    (fun e ->
      incr cnt;
      l := e :: !l)
    s;
  let a = Array.make !cnt (Obj.magic 0) in
  List.iter
    (fun e ->
      decr cnt;
      a.(!cnt) <- e)
    !l;
  a

let map f s =
  from (fun _ ->
      match peek s with
      | Some v ->
          junk s;
          Some (f v)
      | None -> None)

let mapi f s =
  let cnt = ref (-1) in
  from (fun _ ->
      match peek s with
      | Some v ->
          junk s;
          incr cnt;
          Some (f !cnt v)
      | None -> None)

let iteri f s =
  let cnt = ref 0 in
  iter
    (fun x ->
      f !cnt x;
      incr cnt)
    s
