type index = int
type key = int
type value = int
type repr = string

type 'a packer

val make : unit -> 'a packer
val add_vector : 'a packer -> (key * 'a) list -> index
val pack : 'a packer -> ('a -> value) -> repr

val lookup : repr -> index -> key -> value option
