structure Status = Enum.Make(struct
		   val cols = {Active = "ACTIVE", Inactive = "INACTIVE"}
	       end)

fun
main () = return <xml>
  <body>
    <p>1. {[Status.make.Active = Status.make.Inactive]} should be False</p>
    <p>2. {[Status.make.Inactive = Status.make.Inactive]} should be True</p>
    <p>3. {[Status.match Status.make.Active {Active = True, Inactive = False} = True]} should be True</p>
    <p>4. {[Status.make.Inactive]} should be INACTIVE</p>
    <p>5. {[@readError Status.read_t "INACTIVE" = Status.make.Inactive]} should be True</p>
    <p>5. {[@readError Status.read_t "ACTIVE" = Status.make.Active]} should be True</p>
  </body>
</xml>
