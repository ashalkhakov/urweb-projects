fun main () = return <xml>
  <body>
    <p>1. {[Pac.zero = Pac.zero]} should be True</p>

    <p>2. {[show (Pac.make (Pac.debit_make_error 1.0) (Pac.credit_make_error 0.5)) = "[1//0.5]" (* false *)]}</p>

    <p>3a. {[@read @Pac.pac_read "[1.0//0.5]"]}</p>
    <p>3b. {[readError "[1.0//0.5]" = Pac.make (Pac.debit_make_error 1.0) (Pac.credit_make_error 0.5)]}</p>
    <p>3c. {[@read Pac.pac_read "[//]"]} should be empty</p>
    <p>3d. {[@read Pac.pac_read "[1.0//]"]} should be empty</p>
    <p>3e. {[@read Pac.pac_read "[]"]} should be empty</p>
    <p>3f. {[@read Pac.pac_read "[-1.0//0.5]"]} should be empty</p>
    <p>3g. {[@read Pac.pac_read "[1.0//-0.5]"]} should be empty</p>
    
    <p>4a. {[Pac.sum (Pac.make (Pac.debit_make_error 1.0) (Pac.credit_make_error 0.0)) (Pac.make (Pac.debit_make_error 0.0) (Pac.credit_make_error 1.0))]} should be [1//1]</p>
    <p>4b. {[Pac.sum (Pac.make (Pac.debit_make_error 1.0) (Pac.credit_make_error 0.0)) (Pac.make (Pac.debit_make_error 0.0) (Pac.credit_make_error 1.0)) = Pac.zero]} should be True</p>

    <p>5. {[let
	     val p0 = Pac.make (Pac.debit_make_error 1.0) (Pac.credit_make_error 0.0)
	     val p1 = Pac.neg p0
	     val p2 = Pac.sum p0 p1
	     val p3 = (p2 = Pac.zero)
	 in
	     p3
	 end]} should be True</p>
  </body>
</xml>
