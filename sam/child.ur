(* based on UrLib's [set] https://github.com/vizziv/UrLib/blob/master/UrLib/record.urs
*)
fun assign
        [keep ::: {Type}]
        [drop ::: {Type}]
        [insert ::: {Type}]
        [keep ~ drop]
        [keep ~ insert]
        (xs : $(keep ++ drop)) (ys : $(insert))
    : $(keep ++ insert) =
    xs --- drop ++ ys

fun my_active (c : transaction xbody): xbody =
    <xml><active code={c}/></xml>
fun output (x : signal string) : xbody =
<xml><dyn signal={r <- x; return <xml>{[r]}</xml>}/></xml>

(*  https://github.com/jdubray/sam-samples/blob/master/vanilla-child-instance/index.html *)

structure Child = struct
    type model = { Value : int, SubmitToParent : int -> transaction unit }
    datatype data = Value of int

    fun model_init f = { Value = 50, SubmitToParent = f }
    fun model_present m data =
        return (
        case data of Value x => assign m { Value = x }
        )

    type actions = { SubmitToParent : int -> transaction unit }
               
    fun view_ready model actions =
          my_active (
          v <- source (Some (float model.Value));
          return <xml>
            <label>Weight</label><br/>
            <crange min=0.0 max=120.0 source={v}/>
            {output (i <- signal v; return (show i))}<br/>
            <button onclick={fn _ => x <- get v;
                                case x of
                                    None => return ()
                                  | Some x => actions.SubmitToParent (round x)}>
              Submit to parent
            </button>
            </xml>)

    fun view_init m actions = view_ready m actions
    fun view_display representation s = set s representation

    fun state_ready m = True
    fun state_representation model actions =
        if state_ready model then view_ready model actions
        else <xml>oops.. something went wrong, the system is in an invalid state</xml>

    fun present model s d =
        m <- get model;
        m1 <- model_present m d;
        set model m1;
        state_render model s

    and actions_submit_to_parent m s d =
        m0 <- get m;
        m0.SubmitToParent d

    and state_next_action m s =
        return ()

    and state_render model s =
        let
            val actions = { SubmitToParent = actions_submit_to_parent model s }
        in            
            m <- get model;
            let
                val representation = state_representation m actions
            in
                view_display representation s;
                state_next_action model s
            end
        end

    and init s actions =
       child <- source (model_init actions.SubmitToParent);
       state_render child s
end

structure Parent = struct
    type model = { ChildCreated : bool, Value : option int }

    datatype data = Child | Value of int

    fun model_init () =
        { ChildCreated = False, Value = None }

    fun model_present m d =
        return (
        case d of
            Child => { ChildCreated = True, Value = None }
          | Value x => { ChildCreated = False, Value = Some x }
        )

    type actions = { CreateChild : transaction unit, SubmitValue : int -> transaction unit }

    fun view_ready m actions = <xml>
      <p>Child has submitted the following value: {[case m.Value of None => "N/A" | Some x => show x]}</p>
      <button onclick={fn _ => actions.CreateChild}>Create SAM child</button>
    </xml>

    fun view_init m actions = view_ready m actions

    fun view_child m actions = <xml>
      <p>In the face of overwhelming odds, I'm left with only one option, I'm gonna have to science the shit out of this.</p>
      - Mark Watney, The Martian
    </xml>

    fun view_display repr s =
        set s repr

    fun state_ready m = not (m.ChildCreated)
    fun state_child m = m.ChildCreated
    fun state_representation m actions =
        if state_ready m then view_ready m actions
        else if state_child m then view_child m actions
        else <xml>oops... something went wrong, the system is in an invalid state</xml>

    fun present model s d =
        m <- get model;
        m1 <- model_present m d;
        set model m1;
        state_render model s

    and actions_create_child m s =
        present m s Child

    and actions_submit m s d =
        present m s (Value d)
        
    and state_next_action m s =
      m0 <- get m;
      if m0.ChildCreated then Child.init s { SubmitToParent = actions_submit m s }
      else return ()

    and state_render model s =
        let
            val actions = {
                CreateChild = actions_create_child model s,
                SubmitValue = actions_submit model s
            }
        in
            m <- get model;
            let
                val representation = state_representation m actions
            in
                view_display representation s;
                state_next_action model s
            end
        end
end
    
val init = <xml>
  <div>
    <p>Problem is (follow me closely here, the science is pretty complicated), if I cut a hole in the Hab, the air won't stay inside anymore.</p>
    <br/>
    - Andy Weir, The Martian
  </div>
</xml>

fun child_instance () = return <xml>
  <body>
    <active code={
       pmodel <- source (Parent.model_init ());
       s <- source init;
       Parent.state_render pmodel s;
       return <xml><dyn signal={signal s}/></xml>
    }/>    
    
  </body>
</xml>
