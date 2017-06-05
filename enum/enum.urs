(*

an obvious approach to store and retrieve simple enumerated values

*)
functor Make(M : sig
  con vars :: {Unit}
  val fl : folder vars
  val cols : $(mapU string vars)
end) : sig

type t
val sql_t : sql_injectable_prim t

val make : $(mapU t M.vars) (* all variants *)
val match : t' ::: Type -> t -> $(mapU t' M.vars) -> t'
val label : t -> string (* extract label *)

val eq_t : eq t
val ord_t : ord t
val show_t : show t
val read_t : read t

end
