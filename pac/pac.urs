type debit
type credit

val num_debit : num debit
val ord_debit : ord debit
val eq_debit : eq debit
val sql_debit : sql_injectable_prim debit
val sql_maxable_debit : sql_maxable debit

val debit_make : float -> option debit
val debit_make_error : float -> debit

val num_credit : num credit
val ord_credit : ord credit
val eq_credit : eq credit
val sql_credit : sql_injectable_prim credit
val sql_maxable_credit : sql_maxable credit

val credit_make : float -> option credit
val credit_make_error : float -> credit

type t

val make : debit -> credit -> t

(* TODO: val num : num pac *)

val zero : t
val sum : t -> t -> t
val eq : t -> t -> bool
val neg : t -> t
val reduce : t -> t

val pac_eq : eq t
val pac_ord : ord t
val pac_show : show t
val pac_read : read t

datatype pac_side = PSdebit | PScredit
val balance : pac_side -> t -> float
	       
