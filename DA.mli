type 'a t

exception Empty

val init : unit -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int

val push : 'a -> 'a t -> unit
val pop : 'a t -> 'a
val pop_opt : 'a t -> 'a option
val top : 'a t -> 'a
val top_opt : 'a t -> 'a option

val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

