open Stream

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
  Stream.from streamer
;;

let map f s =
  Stream.from (fun _ ->
      match Stream.peek s with
      | Some v ->
          Stream.junk s;
          Some (f v)
      | None -> None)
;;
