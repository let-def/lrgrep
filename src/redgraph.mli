open Fix.Indexing
open Grammar

val derive :
  root:'a ->
  step:('a -> Lr1.t -> 'a option) ->
  join:('a list -> 'b) ->
  'b Lr1.map

module State : sig
  include CARDINAL
  val of_lr1 : Lr1.t -> n index
end

type goto_closure = {
  sources: Lr1.set;
  targets: Lr1.set;
  lookahead: Terminal.set;
}

val state_lr1s : State.n index -> Lr1.set
val state_parent : State.n index -> State.n index option
val state_goto_closure : State.n index -> goto_closure list
val state_reachable : State.n index -> Lr1.set
