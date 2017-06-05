functor Make(M : sig
		 con vars :: {Unit}
		 val fl : folder vars
		 val cols : $(mapU string vars)
	     end) = struct

    type t = string
    val sql_t = _

    val make = M.cols

    fun match [t'] x actions =
	@foldR2 [fn _ => string] [fn _ => t'] [fn tr => unit -> t']
	       (fn [nm ::_] [x ::_] [r ::_] [[nm] ~ r] lab action acc r =>
		   if x = lab then action
		   else acc r
	       )
	       (fn _ =>  error <xml>Unable to match enumeration name: {[x]}</xml>)
	       M.fl
	       M.cols
	       actions
	       ()

    fun
    read_option x =
	@foldR [fn _ => string] [fn tr => unit -> option t]
	       (fn [nm ::_] [x ::_] [r ::_] [[nm] ~ r] lab acc r =>
		   if x = lab then Some lab
		   else acc r
	       )
	       (fn x => None)
	       M.fl
	       M.cols
	       ()
    fun
    read_error x = case read_option x of
		       Some x => x
		     | None => error <xml>Unable to read {[x]} into enumerated type</xml>
    
	
fun label x = x

val eq_t = _
val ord_t = _
val show_t = _
val read_t = mkRead read_error read_option

end
