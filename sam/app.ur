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

(* https://bitbucket.org/snippets/jdubray/9dgKp/sam-sample *)

val counter_max = 10

type model = { Counter : int,  Started : bool, Launched : bool, Aborted : bool }
fun model_showfun (x:model):string =
                            "Counter: " ^ show x.Counter ^ ", Started " ^ show x.Started
                        ^ ", Launched " ^ show x.Launched ^ ", Aborted " ^ show x.Aborted
val model_show = mkShow (model_showfun)

fun model_init () : model =
  { Counter = counter_max,  Started = False, Launched = False, Aborted = False }

(* Derive the current state of the system *)
fun state_ready model =
    ((model.Counter = counter_max)
     && not model.Started
     && not model.Launched
     && not model.Aborted)

fun state_counting model =
    let
        val status = ((model.Counter <= counter_max)
                      && (model.Counter >= 0)
                      && model.Started
                      && not model.Launched
                      && not model.Aborted)
    in
        status
    end

fun state_launched model =
    ((model.Counter = 0)
     && model.Started
     && model.Launched
     && not model.Aborted)

fun state_aborted model =
    model.Counter <= counter_max
    && model.Counter >= 0
    && model.Started
    && not model.Launched
    && model.Aborted

datatype data =
         Aborted of bool
       | Launched of bool
       | Counter of int
       | Started of bool
val show_data =
    mkShow (fn x =>
               case x of Aborted x => "aborted "^show x
                       | Launched x => "launched " ^show x
                       | Counter x => "counter "^show x
                       | Started x => "started "^show x)

fun model_present (model : model) (data : data): transaction model =
  (*alert (show data);*)
  return (
  if state_counting model then (
    if model.Counter = 0 then
       assign model {Launched = case data of Launched b => b | _ => False}
    else let
       val model = assign model {Aborted = case data of Aborted b => b | _ => False}
     in
       (case data of Counter c => assign model {Counter = c} | _ => model)
     end
  ) else (
     if state_ready model then (assign model {Started = case data of Started b => b | _ => False})
     else model
  )
)

type actions = { Start : transaction unit, Abort : transaction unit }

(* State representation of the ready state *)
fun view_ready (model : model) (actions : actions) : xbody = <xml>
<p>Counter: {[model.Counter]}</p>
<button onclick={fn _ => actions.Start}>Start</button>
</xml>

(* Initial State *)
fun view_init (model : model) (actions : actions): xbody = view_ready model actions

(* State representation of the counting state *)
fun view_counting (model : model) (actions : actions): xbody = <xml>
  <p>Count down: {[model.Counter]}</p>
  <button onclick={fn _ => actions.Abort}>Abort</button>
</xml>

(* State representation of the aborted state *)
fun view_aborted (model : model) (actions : actions): xbody = <xml>
  <p>Aborted at Counter: {[model.Counter]}</p>
</xml>

(* State representation of the launched state *)
fun view_launched (model : model) (actions : actions): xbody = <xml>
<p>Launched</p>
</xml>

(* display the state representation *)
fun view_display (representation : xbody) (s : source xbody): transaction unit =
  set s representation

fun state_representation (model : model) (actions : actions) : xbody =
  if state_ready model then view_ready model actions
  else if state_counting model then view_counting model actions
  else if state_launched model then view_launched model actions
  else if state_aborted model then view_aborted model actions
  else <xml>oops.. something went wrong, the system is in an invalid state</xml>

(* actions *)

fun present (model : source model) (s : source xbody) (d : data) : transaction unit =
    m <- get model;
    m1 <- model_present m d;
    set model m1;
    state_render model s

and actions_start m s =
    present m s (Started True)

and actions_decrement m s d =
    sleep 1000;
    present m s (Counter (case d of Some n => (n-1) | None => 10))

and actions_launch m s =
   present m s (Launched True)

and actions_abort m s =
   present m s (Aborted True)

and state_next_action m s =
  m0 <- get m;
  if state_counting m0 then
    if m0.Counter > 0 then actions_decrement m s (Some m0.Counter)
    else if m0.Counter = 0 then actions_launch m s
    else return ()
  else return ()

and state_render (model : source model) (s : source xbody) : transaction unit =
  let
     val actions = {
        Start = actions_start model s,
        Abort = actions_abort model s
     }    
  in
     m <- get model;
     let
        val representation = state_representation m actions
     in
        view_display <xml>{representation}<br/>State: {[show m]}</xml> s;
        state_next_action model s
     end
  end

fun rocket () =
return <xml>
  <body>
    <active code={
       model <- source (model_init ());
       s <- source (<xml/>);
       state_render model s;
       return <xml><dyn signal={signal s}/></xml>
    }/>
  </body>
  </xml>

(* TODO: item list https://bitbucket.org/snippets/jdubray/zr9r6/item-list *)

fun main () = rocket ()
