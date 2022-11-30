include module type of Stream

type 'a t = 'a Stream.t

val of_file : string -> string t

val scan_file :
  string ->
  ( 'a,
    Scanf.Scanning.scanbuf,
    'b,
    'c -> 'd,
    'a -> 'e,
    'e )
  format6 ->
  'c ->
  'd t

val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
