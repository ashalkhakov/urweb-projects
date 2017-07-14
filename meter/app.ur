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

type meter =
     { ID : string
     , MName : string
     , MType : string
     , MeterNr : string
     , Version : int
     , Direction : string
     , Group : string
     , Description : string
     , StartQty : float
     , InstallTime : time
     , Photo : option (url)
     }
type reading =
     { ID : string
     , Qty : float
     , Consumption : float
     , MeterID : string
     , MeterVersion : int
     , ReadingNr : int
     , Time : time
     }

type meter_screen =
     { ID : source string
     , MName : source string
     , MType : source string
     , MeterNr : source string
     , Version : source int
     , Direction : source string
     , Group : source string
     , Description : source string
     , StartQty : source float
     , InstallTime : source time
     , Photo : source (option url)
     }     
     
type meter_info_screen = { Meter : meter, Readings : list reading }

fun
meter_edit (id : string) : transaction page =

let

    fun create () : transaction meter_screen =
        id0 <- source id;
        mname <- source "Meter sample";
        mtype <- source "electric";
        meternr <- source "15869";
        version <- source 1;
        direction <- source "asc";
        group <- source "Apartment 1";
        description <- source "";
        startqty <- source 93598.0;
        installtime <- source (fromDatetime 1979 8 31 0 0 0);
        photo <- source None;
        return { ID = id0, MName = mname, MType = mtype, MeterNr = meternr, Version = version, Direction = direction
               , Group = group, Description = description, StartQty = startqty, InstallTime = installtime, Photo = photo }

in    
    return <xml>
      <body>
        <active code={meter <- create ();
                      idate <- source "";
                      quantity <- source (None : option float);

                      return <xml>
                        <dyn signal={it <- signal meter.InstallTime;
                                     qt <- signal meter.StartQty;

                                     return <xml>
                                       <!-- -->
                                       <active code={set idate
                                                         (show it);
                                                     set quantity (Some qt);

                                                     return <xml>
                                                       <div>
                                                       <label>Meter name:</label>
                                                       <ctextbox source={meter.MName}/>
                                                       <label>Meter type:</label>
                                                       <cselect source={meter.MType}>
                                                         <coption value="electric">electric</coption>
                                                       </cselect>
                                                       <label>Start quantity:</label>
                                                       <cnumber source={quantity}
                                                       onchange={v <- get quantity;
                                                                 (case v of
                                                                     None => alert "Invalid quantity"
                                                                   | Some v => set meter.StartQty v);
                                                                  return ()
                                                                }/>
                                                       <label>No:</label>
                                                       <ctextbox source={meter.MeterNr}/>
                                                       <label>Install time</label>
                                                       <cdatetime_local source={idate}
                                                       onchange={v <- get idate;
                                                                 let
                                                                     val v = @read read_time v
                                                                 in
                                                                     case v of None => alert "Invalid install time"
                                                                             | Some v => set meter.InstallTime v
                                                                 end
                                                                }/>
                                                       </div>
                                                       <div>
                                                         <label>Count direction</label>
                                                         <cselect source={meter.Direction}>
                                                           <coption value="asc">ascending</coption>
                                                           <coption value="desc">descending</coption>
                                                         </cselect>
                                                         <label>Description</label>
                                                         <ctextarea source={meter.Description} rows=3 placeholder="Description..." />
                                                       </div>
                                                    </xml>}/> <!-- active -->
                                       
                                     </xml>}/> <!-- dyn -->
                                    </xml>}/> <!-- active -->
      </body>
    </xml>

end

(* TODO:
meter info should also show a line chart of consumption *)
                         
fun
meter_info (id : string) =
let
    fun
    create () : transaction (meter_info_screen) =
    return { Meter = { ID = id, MName = "Meter sample", MType = "electric", MeterNr = "15869", Version = 1, Direction = "ASC", Group = "Apartment 1", Description = "", StartQty = 93598.0, InstallTime = fromDatetime 1979 8 31 0 0 0, Photo = None }, Readings = if id = "1" then { ID = "", Qty = 93971.0, Consumption = 373.0, MeterID = id, MeterVersion = 1, ReadingNr = 1, Time = fromDatetime 2013 8 1 16 8 0 }
                                                                                                                                                                                                                                                                       :: { ID = "", Qty = 94443.0, Consumption = 472.0, MeterID = id, MeterVersion = 1, ReadingNr = 2, Time = fromDatetime 2013 9 2 16 10 0 }
                                                                                                                                                                                                                                                                       :: [] else [] }
in
    mi <- create ();
    return <xml>
      <a link={meter_edit id}>Edit</a>
      
      <h1>{[mi.Meter.MName]}</h1>
      <p>{[mi.Meter.MType]}</p>
      <p>No. {[mi.Meter.MeterNr]}</p>
      <p>Start quantity: {[mi.Meter.StartQty]}</p>
      <table>
        <thead>
          <tr>
            <td>Reading time</td>
            <td>Version</td>
            <td>Reading</td>
            <td>Consumption</td>
          </tr>
        </thead>
        <tbody>
          {List.mapX (fn it =>
                         <xml>
                           <tr>
                             <td>{[it.Time]}</td>
                             <td>{[it.MeterVersion]}</td>
                             <td>{[it.Qty]}</td>
                             <td>{[it.Consumption]}</td>
                           </tr>
                         </xml>) mi.Readings}
        </tbody>
      </table>
    </xml>
end
     
type meter_group_meter = { MeterName : string, ID : string }
type meter_group_screen = { GroupName : string, Meters : list meter_group_meter }

fun
meter_list () =
let
    fun
    create () : transaction (list meter_group_screen) =
    let
        val m0 = { MeterName = "Meter sample", ID = "1" }
        val m1 = { MeterName = "Meter sample 2", ID = "2" }
    in
        return ({ GroupName = "Apartment 1", Meters = m0 :: m1 :: [] } :: [])
    end
in
return <xml>
  <head>
    <title>Meter: list</title>
  </head>
  <body>
    <active code={meters <- create ();
                  sel_meter_id <- source None;
                  return <xml>
                    <div>{
                  List.mapX (fn gr =>
                                <xml>
                                  <div>
                                    <h2>{[gr.GroupName]}</h2>

                                    <ul>{List.mapX
                                             (fn m => <xml>
                                               <li>
                                                 <button onclick={fn _ => set sel_meter_id (Some m.ID)}>{[m.MeterName]}</button>
                                               </li>
                                             </xml>) gr.Meters}
                                    </ul>
                                  </div>
                    </xml>) meters}</div>
                    <div>
                      <dyn signal={meter_id <- signal sel_meter_id;
                                   case meter_id of
                                       None => return <xml></xml>
                                     | Some meter_id =>
                                       return <xml><active code={meter_info meter_id}/></xml>}/>
                    </div>
                  </xml>}/>
  </body>
  </xml>
end

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
