fun output (x : signal string) : xbody =
<xml><dyn signal={r <- x; return <xml>{[r]}</xml>}/></xml>

(** A helper higher-order function, to map over a list with a function that is passed
  * a function for computing the list without the current element. *)
fun mapX [a] (f : (unit -> list a) -> a -> xbody) (ls : list a) : xbody =
    let
        fun mapX' ls acc =
            case ls of
                [] => <xml></xml>
              | x :: ls => <xml>{f (fn () => List.revAppend acc ls) x}{mapX' ls (x :: acc)}</xml>
    in
        mapX' ls []
    end

type telephone = { Typ : string, Tel : string }
type person = {
     Nam : string
   , BirthDate : option time
   , Gender : string
   , Telephones : list telephone
}
type telephoneS = {
     Typ : source string
   , Tel : source string
}
type personS = {
     Nam : source string
   , BirthDate : source (option time)
   , Gender : source string
   , Telephones : source (list telephoneS)
}

fun person_add () =
    let
        
        fun create () : transaction personS =
            name <- source "Javier";
            (* NOTE: since we don't have a specific type for a date,
             * will have to manage the view/model distinction manually *)
            birthdate <- source (Some (fromDatetime 1979 8 31 0 0 0));
            gender <- source "M";

            tel1nr <- source "666005000";
            tel1type <- source "mobile";
            tel2nr <- source "986888888";
            tel2type <- source "home";
            
            tels <- source (
                    { Typ = tel1type, Tel = tel1nr }
                        :: { Typ = tel2type, Tel = tel2nr }
                        :: []
                    );
            return { Nam = name, BirthDate = birthdate, Gender = gender, Telephones = tels }
        fun person_reset (perso : source personS) : transaction unit =
            p0 <- create ();
            set perso p0
        fun person_saveas (perso : source personS) : transaction unit =
            (* TODO: send the flattened row to the server to save it *)
            alert "Sorry, not implemented yet (requires RPC and a server)"
        fun person_unsourcify (perso : source personS) : transaction person =
            p <- get perso;
            
            name <- get p.Nam;
            birthdate <- get p.BirthDate;
            gender <- get p.Gender;

            tel1nr <- source "666005000";
            tel1type <- source "mobile";
            tel2nr <- source "986888888";
            tel2type <- source "home";

            tels <- get p.Telephones;
            tels <- List.mapM (fn x =>
                                  ty <- get x.Typ;
                                  tel <- get x.Tel;
                                  return { Typ = ty, Tel = tel }) tels;
            return
                { Nam = name
                , BirthDate = birthdate
                , Gender = gender
                , Telephones = tels
                }
            
        fun person_view (perso : source personS) : transaction unit =
            p1 <- person_unsourcify perso;
            let
                (* TODO: proper ISO format? why is it not in the JSON library? *)
                val json_time : Json.json time
                    = Json.mkJson
                          { ToJson
                            = fn x => @Json.toJson Json.json_string (show x)
                          , FromJson
                            = fn x =>
                                 let
                                     val (d, r) =
                                         @Json.fromJson' Json.json_string x
                                 in
                                     (@readError read_time d, r)
                                 end
                          }
                val telephone_json : Json.json telephone
                  = Json.json_record { Typ = "type", Tel = "tel" }
                val person_json : Json.json person
                  = Json.json_record {
                    Nam = "name"
                  , BirthDate = "birthdate"
                  , Gender = "gender"
                  , Telephones = "telephones" }
            in
                alert (Json.toJson p1)
            end
        fun person_new_phone (perso : source personS) : transaction unit =
            p <- get perso;
            tels <- get p.Telephones;

            typ <- source "mobile";
            tel <- source "";
            set p.Telephones ({ Typ = typ, Tel = tel } :: tels)

    in
    return <xml>
      <body>
        <active code={
    p0 <- create ();
    perso <- source p0;
                      
    bdate <- source "";
    return <xml>
      <dyn signal={
    person <- signal perso;
    bd <- signal p0.BirthDate;
                                     
    return <xml>
      <active code={
    set bdate (case bd of None => "" | Some bd => timef "%Y-%m-%d" bd);
    return <xml/>}/>                 
		  <fieldset>
                    <legend>Contact</legend>

		    <label>Name</label>
		    <ctextbox source={person.Nam}/>
                    <label>Gender</label>
		    <cselect source={person.Gender}>
		      <coption value="M">Male</coption>
		      <coption value="F">Female</coption>
		    </cselect>
		    <label>Birth date</label>
		    <cdate source={bdate}
                    onchange={v <- get bdate;
                              set person.BirthDate (@read read_time v)
                             }/>

		    <div>
		      <label>Telephones</label>
                      <dyn signal={
tels <- signal person.Telephones;

return <xml>
  <ul>
  {mapX (fn del it => <xml>
    <li>
    <label>Type</label>
    <cselect source={it.Typ}>
<coption value="home">Home</coption>
<coption value="mobile">Mobile</coption>
<coption value="company">Company</coption>
    </cselect>
                                                          
    <label>Number</label>
    <ctextbox source={it.Tel}/>
    <button onclick={fn _ => set person.Telephones (del ())}>X</button>
    </li>
  </xml>) tels}
  </ul>
</xml>
}/>
                      <button onclick={fn _ => person_new_phone perso}>New</button>
</div>
		    </fieldset>
		    <button onclick={fn _ => person_view perso}>View</button>
                    <button onclick={fn _ => person_saveas perso}>Save As</button>
		    <button onclick={fn _ =>
                                        person_reset perso;
                                        p0 <- get perso;
                                        bd <- get p0.BirthDate;
                                        set bdate (case bd of None => "" | Some bd => timef "%Y-%m-%d" bd)}>Reset</button>
                  </xml>
    }/>
     </xml>
    }/>
     </body>
      </xml>
    end

open Screens

fun
main () = return <xml>
  <head>
    <title>Meter monitoring app</title>
  </head>
  <body>
    <h1>Meter monitoring</h1>
    
    <h2>Meters</h2>

    <a link={meter_list ()}>Meters</a>

    <h2>Lookup tables</h2>
    <a link={person_add ()}>Person: add</a>
  </body>
</xml>
