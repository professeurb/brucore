include module type of struct include Stream end

val of_file : string -> string Stream.t

val map : ('a -> 'b) -> 'a Stream.t -> 'b Stream.t
