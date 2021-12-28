include module type of struct include Stdlib.Stream end

type 'a t = 'a Stdlib.Stream.t

val of_file : string -> string t

val map : ('a -> 'b) -> 'a t -> 'b t
