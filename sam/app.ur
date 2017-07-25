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
       s <- source (<xml></xml>);
       state_render model s;
       return <xml><dyn signal={signal s}/></xml>
    }/>
  </body>
  </xml>

(* Todo MVC *)

(* item list https://bitbucket.org/snippets/jdubray/zr9r6/item-list *)

val counter_max = 10

type item = { Id : int, Nam : string, Description : string }
type newitem = { Nam : string, Description : string }
val newitem_show : show newitem =
    mkShow (fn i =>
               "(name '" ^ i.Nam ^ "', desc '" ^ i.Description ^ ")")
type model = {
     Items : list item
   , ItemId : int
   , LastEdited : option item
(*, LastDeleted : option int*)
}
val item_show : show item =
    mkShow (fn i =>
               "(id " ^ show i.Id ^ ", name '" ^ i.Nam ^ "', desc '" ^ i.Description ^ ")")

val model_show : show model =
    mkShow (fn x =>
               "Items: "^ show x.Items
               ^ ", ItemId: " ^ show x.ItemId
               ^ ", LastEdited: " ^ show x.LastEdited)

fun model_init () : model =
    { Items = { Id = 2, Nam = "Item 2", Description = "This is a description" }
                  :: { Id = 1, Nam = "Item 1", Description = "This is a description" }
                  :: []
    , ItemId = 3
    , LastEdited = None
    }

datatype data =
  DeletedItemId of int
| SetLastEdited of item
| EditItem of item
| NewItem of newitem
| Cancel
val show_data : show data =
    mkShow (fn d =>
               case d of
                   DeletedItemId i => "DeletedItemId("^ show i ^")"
                 | SetLastEdited x => "SetLastEdited("^ show x ^")"
                 | EditItem x => "EditItem("^ show x ^")"
                 | NewItem x => "NewItem("^ show x ^")"
                 | Cancel => "Cancel")
  
fun model_present (model : model) (data : data) : transaction model =
    (*alert ("present:" ^ show data);*)
    return (
case data of
  DeletedItemId id => assign model {Items = List.filter (fn x => (x.Id <> id)) model.Items}
| SetLastEdited itm => assign model {LastEdited = Some itm}
| EditItem itm =>
  assign model {Items = List.mp (fn x => if x.Id = itm.Id then itm else x) model.Items, LastEdited = None}
| NewItem itm => let
      val id = model.ItemId
  in
      assign model {Items = ({Id = id} ++ itm) :: model.Items, ItemId = id+1}
  end
| Cancel => model
    )

type actions = { Edit : item -> transaction unit
               , EditSave : item -> transaction unit
               , Create : newitem -> transaction unit
               , Delete : int -> transaction unit
               , Cancel : transaction unit }

fun my_active (c : transaction xbody): xbody =
    <xml><active code={c}/></xml>

fun mapX [a ::: Type] [ctx ::: {Unit}] (f : a -> xml ctx [] []) (ls : list a) : xml ctx [] [] =
  List.mapX f (List.rev ls)
    
fun view_ready (model : model) (actions : actions) : xbody =
    my_active (            

    nameValue <- source (case model.LastEdited of Some x => x.Nam | None => "");
    descriptionValue <- source (case model.LastEdited of Some x => x.Description | None => "");
    id <- return (case model.LastEdited of Some x => Some x.Id | None => None);

    return <xml>
      <!-- class="mdl-cell mdl-cell- -6-col" -->
      <div>
{mapX
     (fn e =>
                   <xml>
                     <h3 onclick={fn _ => (*alert ("edit" ^ show e);*) actions.Edit e}>{[e.Nam]}</h3>
                     <p>{[e.Description]}</p>
                     <button onclick={fn _ => actions.Delete e.Id}>Delete</button>
                   </xml>)
              model.Items}
      </div>
      <!-- class="mdl-cell mdl-cell- -6-col" -->
      <div>
        <ctextbox source={nameValue} placeholder="Name"/>
        <ctextbox source={descriptionValue} placeholder="Description"/>
        <button onclick={fn _ =>
                            name <- get nameValue;
                            description <- get descriptionValue;
                            case id of
                                Some id =>
                                actions.EditSave { Id = id, Nam = name, Description = description }
                              | None =>
                                actions.Create { Nam = name, Description = description }}>Save</button>
        <button onclick={fn _ => actions.Cancel}>Cancel</button>
      </div>
    </xml>
    )

fun view_init (model : model) (actions : actions) : xbody = view_ready model actions

fun view_display (representation : xbody) (s : source xbody): transaction unit =
  set s representation

(* Derive the current state of the system *)
fun state_ready (model : model) = True
      
(* Derive the state representation as a function of the systen
 * control state
 *)
fun state_representation (model : model) (actions : actions) : xbody =
  if state_ready model then view_ready model actions
  else <xml>oops.. something went wrong, the system is in an invalid state</xml>  
  
fun present (model : source model) (s : source xbody) (d : data) : transaction unit =
    m <- get model;
    m1 <- model_present m d;
    (*alert ("after: " ^ show m1);*)
    set model m1;
    state_render model s
  
and action_edit m s data =
    present m s (SetLastEdited data)

and action_create_save m s data =
    present m s (NewItem data)

and action_edit_save m s data =
    present m s (EditItem data)

and action_delete m s data =
    present m s (DeletedItemId data)

and action_cancel m s =
    present m s Cancel

and state_next_action m s = return ()

and state_render (model : source model) (s : source xbody) : transaction unit =
  let
      val actions = {
          Edit = fn itm => action_edit model s itm
        , Create = fn itm => action_create_save model s itm
        , EditSave = fn itm => action_edit_save model s itm
        , Delete = fn id => action_delete model s id
        , Cancel = action_cancel model s
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

fun todosam () =
return <xml>
  <body>
    <h1>Todo SAM</h1>
    <p>Press on entry heading to edit it.</p>
    <active code={
       model <- source (model_init ());
       s <- source (<xml/>);
       state_render model s;
       return <xml><dyn signal={signal s}/></xml>
    }/>
  </body>
  </xml>

fun main () =
  return <xml>
    <body>
      <a link={rocket ()}>Rocket</a>
      <a link={todosam()}>TODO SAM</a>
    </body>
  </xml>
