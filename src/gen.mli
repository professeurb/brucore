type 'a t = unit -> 'a option

val of_list : 'a list -> 'a t
val of_array : 'a array -> 'a t
val of_fun : (('a -> unit) -> unit) -> 'a t
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

val iter : ('a -> unit) -> 'a t -> unit
val iteri : ?pos:int -> (int -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : ?pos:int -> (int -> 'a -> 'b) -> 'a t -> 'b t

val scan :
  ( 'a,
    Scanf.Scanning.scanbuf,
    'b,
    'c -> 'd,
    'a -> 'e,
    'e )
  format6 ->
  'c ->
  string t ->
  'd t

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val filter : ('a -> bool) -> 'a t -> 'a t
val tap : ('a -> unit) -> 'a t -> 'a t

val genfold :
  ?init:bool -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
