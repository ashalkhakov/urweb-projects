structure Child : sig
    type model = { Value : int, SubmitToParent : int -> transaction unit }
    datatype data = Value of int

    val model_init : (int -> transaction unit) -> model
    val model_present : model -> data -> transaction model

    type actions = { SubmitToParent : int -> transaction unit }

    val view_init : model -> actions -> xbody
    val view_ready : model -> actions -> xbody
    val view_display : xbody -> source xbody -> transaction unit

    val state_ready : model -> bool
    val state_representation : model -> actions -> xbody
    val state_next_action : source model -> source xbody -> transaction unit

    val state_render : source model -> source xbody -> transaction unit

    val init : source xbody(*where to render*) -> actions -> transaction unit
end

structure Parent : sig
    type model = { ChildCreated : bool, Value : option int }

    datatype data = Child | Value of int

    val model_init : unit -> model
    val model_present : model -> data -> transaction model

    type actions = { CreateChild : transaction unit, SubmitValue : int -> transaction unit }
               
    val view_init : model -> actions -> xbody
    val view_ready : model -> actions -> xbody
    val view_child : model -> actions -> xbody
    val view_display : xbody -> source xbody -> transaction unit

    val state_ready : model -> bool
    val state_representation : model -> actions -> xbody
    val state_next_action : source model -> source xbody -> transaction unit
    val state_render : source model -> source xbody -> transaction unit
end

val child_instance : {} -> transaction page
