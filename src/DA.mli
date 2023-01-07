type 'a t

exception Empty

val init : unit -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val pop_opt : 'a t -> 'a option
val top : 'a t -> 'a
val top_opt : 'a t -> 'a option
val get : 'a t -> int -> 'a
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(* val of_stream : 'a Stream.t -> 'a t *)
val to_array : 'a t -> 'a array

(* Heap *)
(* val heap_push : ('a -> 'a -> int) -> 'a -> 'a t -> unit *)
(* val heap_pop : ('a -> 'a -> int) -> 'a t -> 'a *)
(* val heap_pop_opt : ('a -> 'a -> int) -> 'a t -> 'a option *)

(* Très dangereux à utiliser *)
(* On privilégiera les iter et fold *)
(* val carrier : 'a t -> 'a array *)
