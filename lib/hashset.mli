type 'a t

val init : ?hash:('a -> int) -> unit -> 'a t
val add : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool

(* val remove : 'a t -> 'a -> unit *)
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val to_list : 'a t -> 'a list
