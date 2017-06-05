(*
http://www.ellerman.org/Davids-Stuff/Maths/Omega-DEB.CV.pdf

T-term [d//c], where d and c are naturals
 *)

type debit = float
val num_debit = _
val ord_debit = _
val eq_debit = _
val sql_debit = _
val sql_maxable_debit = _

fun debit_make x = if x < 0.0 then None else Some x
fun debit_make_error x = if x < 0.0 then error <xml>Debit cannot be negative {[x]}</xml> else x

type credit = float
val num_credit = _
val ord_credit = _
val eq_credit = _
val sql_credit = _
val sql_maxable_credit = _

fun credit_make x = if x < 0.0 then None else Some x
fun credit_make_error x = if x < 0.0 then error <xml>Credit cannot be negative {[x]}</xml> else x											      

type t = debit * credit

fun
make w x = (w, x)
       
val
zero = (0.0, 0.0)

fun
sum (w, x) (y, z) = (w+y, x+z)

fun
eq (w,x) (y,z) = (x+y = w+z)

fun
neg (w,x) = (x,w)

fun
reduce (w,z) = let val r = min w z in (w - r, z - r) end

fun
pacShow (w,z) = "[" ^ show w ^ "//" ^ show z ^ "]"
fun
pacRead x =
let
    val xlen = strlen x
    val c0 = strsub x 0 = #"["
    val c1 = strsub x (xlen-1) = #"]"
in
    case (c0, c1) of
    | (True, True) =>
      (case (strsindex x "//") of
	   Some ix =>
	   let
	       val s0 = substring x 1 (ix-1)
	       val s1 = substring x (ix+2) (xlen-ix-2-1)
	       val dr = @read read_float (s0)
	       val cr = @read read_float (s1)
	   in
	       case (dr, cr) of
		   (Some dr, Some cr) => let
		       val dr = debit_make dr
		       val cr = credit_make cr
		   in
		       case (dr, cr) of
			   (Some dr, Some cr) => Some (make dr cr)
			 | (_, _) => None
		   end
		 | (_, _) => None
	   end
	 | None => None)
    | (_, _) => None
end

fun
pacReadError x = case pacRead x of Some x => x | None => error <xml>failed to read</xml>

(* val pac_num = ??? // really? can't be extended? *)
val pac_eq = mkEq eq
val pac_ord =
    mkOrd { Lt = fn (w,x) (y,z) => (x+y < w+z), Le = fn (w,x) (y,z) => (x+y <= w+z) }
val pac_show = mkShow pacShow
val pac_read = mkRead pacReadError pacRead

datatype pac_side = PSdebit | PScredit
fun balance s (w, x) = case s of PSdebit => w-x | PScredit => x-w
