include module type of struct include Stdlib.Stream end

type 'a t = 'a Stdlib.Stream.t

val of_file : string -> string Stream.t

val map : ('a -> 'b) -> 'a Stream.t -> 'b Stream.t
