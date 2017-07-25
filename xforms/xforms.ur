fun output (x : signal string) : xbody =
<xml><dyn signal={r <- x; return <xml>{[r]}</xml>}/></xml>

fun ex_input () =
    let
        fun create () =
            givenName <- source "";
            surName <- source "";
            return {GivenName= givenName, SurName = surName}
    in
        return <xml>
          <body>
            <active code={data <- create ();
                          return <xml>
                            <label>Input First-Name:</label>
                            <!-- no hint support! -->
                            <ctextbox source={data.GivenName}/>
                                                             <br/>
                                                             <label>Input Last Name:</label>
                                                             <!-- no hint support! -->
                                                             <ctextbox source={data.SurName}/>
                                                             <br/>
                                                             Output first name: {output (signal data.GivenName)}
                                                             <br/>
                                                             Output last name: {output (signal data.SurName)}
                                                             </xml>
                         }/>
          </body>
        </xml>
    end

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
    
fun ex_incremental () =
    let

        fun create () =
            ocn <- source "";
            pn <- source "";
            rt <- source "";
            return {ObjectClassName = ocn, PropertyName = pn, RepresentationTerm = rt}

    in

        return <xml>
          <body>
            <active code={dummy <- create ();
                          dataElements <- source (dummy :: []);
                          return <xml>
                            <dyn signal={ds <- signal dataElements;
                                         case ds of
                                             [] => return <xml/> | _ => return <xml>
                                               {mapX (fn del it => <xml>
                                                 <fieldset>
                                                   <legend>Data Element Name</legend>
                                                   <label>DataElementName:</label>
                                                   {output (ocn <- signal it.ObjectClassName;
                                                            pn <- signal it.PropertyName;
                                                            rt <- signal it.RepresentationTerm;
                                                            return (ocn ^ pn ^ rt))}
                                                   <p>
                                                     <label>Object Name:</label>
                                                     <ctextbox source={it.ObjectClassName}/>
                                                   </p>
                                                   <p>
                                                     <label>Property Name:</label>
                                                     <ctextbox source={it.PropertyName}/>
                                                   </p>
                                                   <p>
                                                     <label>Representation Term:</label>
                                                     <cselect source={it.RepresentationTerm}>
                                                       <coption value="Amount">Amount</coption>
                                                       <coption value="Code">Code</coption>
                                                       <coption value="Count">Count</coption>
                                                       <coption value="ID">ID</coption>
                                                       <coption value="Indicator">Indicator</coption>
                                                       <coption value="Name">Name</coption>
                                                       <coption value="Percent">Percent</coption>
                                                       <coption value="Text">Text</coption>
                                                     </cselect>
                                                   </p>
                                                 </fieldset>
                                               </xml>) ds}
                                               </xml>
                                        }/>
                          </xml>}/>
          </body>
       </xml>

    end

fun ex_checkbox () =
let
    fun create () =
        b1 <- source True;
        b2 <- source False;
        return {Bool1 = b1, Bool2 = b2}
in
    return <xml>
      <body>
        <active code={data <- create ();
                      return <xml>
                        <h1>Checkbox</h1>
                        <label>Bool 1:</label>
                        <ccheckbox source={data.Bool1}/>
                        <label>Bool 2:</label>
                        <ccheckbox source={data.Bool2}/>
                        <label>Bool 1:</label>
                        {output (b <- signal data.Bool1; return (show b))}
                        <label>Bool 2:</label>
                        {output (b <- signal data.Bool2; return (show b))}
                      </xml>
        }/>
      </body>
    </xml>
end

fun ex_select_model () : transaction page =
    let
        fun code1 l v =
            {Label = l, Value = v}
        fun create () =
            mycode <- source "";
            mycodelist <- source (
                          code1 "Red" "red"
                              :: code1 "Orange" "orange"
                              :: code1 "Yellow" "yellow"
                              :: code1 "Green" "green"
                              :: code1 "Blue" "blue"
                              :: []);
            return {MyCode = mycode, MyCodeList = mycodelist}
    in
        return <xml>
          <body>
            <active code={model <- create ();
                          return <xml>
                            <p>Getting Selection List Data From the Model</p>
                            <dyn signal={codelist <- signal model.MyCodeList;
                                         return <xml>
                                           <cselect source={model.MyCode}>{List.mapX (fn it =>
                                                                                    <xml>
                                                                                      <coption value={it.Value}>{[it.Label]}</coption>
                                                                                    </xml>) codelist}</cselect>
                                           </xml>}/>
                            <br/>
                            Output: {output (signal model.MyCode)}
                          </xml>
                          }/>
            </body>
          </xml>
    end

fun ex_button () =
    return <xml>
      <body>
        <active code={return <xml>
          <!-- no hint support! -->
          <button onclick={fn _ => alert "Hello world!"}>Button</button>
          </xml>}/>
      </body>
    </xml>

(*

next:
- balance demo
- meter entry

http://www.impredicative.com/pipermail/ur/2011-October/000817.html

 *)

datatype calc_state = CSadd | CSsub | CSmul | CSdiv

(* TODO: handling of xf:switch/xf:case *)
                                          
fun ex_calculator () =

    let

        fun create () =
            scr <- source 0.0;
            scrbuf <- source 0.0;
            first <- source 0.0;
            second <- source 0.0;
            memory <- source 0.0;
            result <- source 0.0;
            state <- source CSadd;
            return { State = state, Screen = scr, ScreenBuffer = scrbuf, First = first, Second = second, Memory = memory, Result = result }
        fun clear calc =
            set calc.First 0.0;
            set calc.Second 0.0;
            set calc.Result 0.0;
            set calc.Screen 0.0;
            set calc.ScreenBuffer 0.0;
            set calc.State CSadd
        fun memclear calc =
            set calc.Memory 0.0
        fun digit calc d =
            sb <- get calc.ScreenBuffer;
            let
                val ns = sb * 10.0 + d
            in
                set calc.ScreenBuffer ns;
                set calc.Screen ns
            end
        fun divide calc =
            s <- get calc.Screen;
            set calc.First s;
            set calc.ScreenBuffer 0.0;
            set calc.State CSdiv
        fun memrecall calc =
            m <- get calc.Memory;
            set calc.ScreenBuffer m;
            set calc.Screen m;
            set calc.ScreenBuffer 0.0
        fun multiply calc =
            s <- get calc.Screen;
            set calc.First s;
            set calc.ScreenBuffer 0.0;
            set calc.State CSmul
        fun memsave calc =
            s <- get calc.Screen;
            set calc.Memory s;
            set calc.ScreenBuffer 0.0
        fun subtract calc =
            s <- get calc.Screen;
            set calc.First s;
            set calc.ScreenBuffer 0.0;
            set calc.State CSsub
        fun recip calc =
            s <- get calc.Screen;
            set calc.Screen (1.0 / s)
        fun memadd calc =
            m <- get calc.Memory;
            s <- get calc.Screen;
            set calc.Memory (m + s);
            set calc.ScreenBuffer 0.0
        fun invert calc =
            s <- get calc.Screen;
            set calc.Screen (-s)
        fun add calc =
            s <- get calc.Screen;
            set calc.First s;
            set calc.ScreenBuffer 0.0;
            set calc.State CSadd
        fun apply calc =
            cs <- get calc.State;
            s <- get calc.Screen;
            set calc.Second s;
            f <- get calc.First;
            (case cs of
                 CSadd => set calc.Result (f + s)
               | CSsub => set calc.Result (f - s)
               | CSmul => set calc.Result (f * s)
               | CSdiv => set calc.Result (f / s));
            r <- get calc.Result;
            set calc.Screen r;
            set calc.ScreenBuffer 0.0

        fun exbutton f lab =
            <xml>
              <button onclick={fn _ => f ()} style="width: 100%; height: 100%">{[lab]}</button>
            </xml>

    in
        return <xml>
          <body>
            <active code={
        calc <- create ();
        return <xml>
              <h3>Calculator</h3>

              <table border=1>
                <tr>
                  <td align="right" colspan=6>{output (r <- signal calc.Screen; return (show r))}</td>
                </tr>
                <tr>
                  <td>
                    <label>M:</label> {output (r <- signal calc.Memory; return (show r))}
                  </td>
                  <td colspan=3></td>
                  <td colspan=2>
                    {exbutton (fn _ => clear calc) "Clear"}
                  </td>
                </tr>
                <tr>
                  <td>
                    {exbutton (fn _ => memclear calc) "MC"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 7.0) "7"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 8.0) "8"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 9.0) "9"}
                  </td>
                  <td>
                    {exbutton (fn _ => divide calc) "/"}
                  </td>
                  <td></td>
                </tr>
                <tr>
                  <td>
                    {exbutton (fn _ => memrecall calc) "MR"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 4.0) "4"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 5.0) "5"}
                  </td>
            	  <td>
                    {exbutton (fn _ => digit calc 6.0) "6"}
                  </td>
            	  <td>
                    {exbutton (fn _ => multiply calc) "*"}
                  </td>
                </tr>
                <tr>
                  <td>
                    {exbutton (fn _ => memsave calc) "MS"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 1.0) "1"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 2.0) "2"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 3.0) "3"}
                  </td>
                  <td>
                    {exbutton (fn _ => subtract calc) "-"}
                  </td>
                  <td>
                    {exbutton (fn _ => recip calc) "1/x"}
                  </td>
                </tr>
                <tr>
                  <td>
                    {exbutton (fn _ => memadd calc) "M+"}
                  </td>
                  <td>
                    {exbutton (fn _ => digit calc 0.0) "0"}
                  </td>
                  <td colspan=2>
                    {exbutton (fn _ => invert calc) "+/-"}
                  </td>
                  <td>
                    {exbutton (fn _ => add calc) "+"}
                  </td>
                  <td>
                    {exbutton (fn _ => apply calc) "="}
                  </td>
                </tr>
              </table>
            </xml>}/>
            </body>
          </xml>

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

fun ex_contact () =
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

type book = { Year : int, Title : string }
type writer = { FirstName : string, LastName : string, Books : list book }

fun ex_writers_books books =
    <xml>
      <ul>{List.mapX (fn it => <xml><li>{[it.Year]} - {[it.Title]}</li></xml>) books}</ul>
    </xml>
              
fun ex_writers () =
    let

        fun create () : transaction (source (list writer)) =
            let
                val res =
                    { FirstName="Albert", LastName="Camus"
                    , Books =
                      { Year=1938, Title="Caligula"
                      } ::
                        { Year=1942, Title="L'étranger"
                        } ::
                        { Year=1947, Title="La peste"
                        } ::
                        { Year=1951, Title="L'homme révolté"
                        } :: []
                    } ::
                      { FirstName="Blaise", LastName="Cendrars"
                      , Books =
                        { Year=1925, Title="L'or"
                        } ::
                          { Year=1948, Title="Bourlinguer"
                          } :: []
                      } ::
                      { FirstName="Victor", LastName="Hugo"
                      , Books =
                        { Year=1831, Title="Notre-Dame de Paris"
                        } ::
                          { Year=1838, Title="Ruy Blas"
                          } ::
                          { Year=1856, Title="Les contemplations"
                          } ::
                          { Year=1862, Title="Les misérables"
                          } :: []
                      } ::
                      { FirstName="Edmond", LastName="Rostand"
                      , Books =
                        { Year=1897, Title="Cyrano de Bergerac"
                        } ::
                          { Year=1904, Title="Chantecler"
                          } :: []
                      } ::
                      { FirstName="Antoine", LastName="de Saint-Exupery"
                      , Books =
                        { Year=1931, Title="Vol de nuit"
                        } ::
                          { Year=1942, Title="Pilote de guerre"
                          } ::
                          { Year=1943, Title="Le petit prince"
                          } :: []
                      } :: []
            in
                source res
            end
    in
    return <xml>
      <body>
        <active code={writers <- create ();
                      return <xml>
	                <h3>Writers (Subforms)</h3>

                        <p>Note: subform functionality with delayed loading is not available</p>

                        <dyn signal={wr <- signal writers;
                                     return (List.mapX (fn it =>
                                                   <xml>
                                                     <active code={selected <- source False;
                                                                   return <xml>
			                                             <p>
			                                               {[it.FirstName]}&#160;{[it.LastName]}&#160;
                                                                       <dyn signal={
                                                                       sel <- signal selected;
                                                                       return <xml>
                                                                         <button onclick={fn _ =>
                                                                                             s <- get selected;
                                                                                             set selected (not s)}>{[if not sel then "Show" else "Hide"]} Books</button>
                                                                           {if sel then ex_writers_books it.Books else <xml/>}
                                                                       </xml>}/>
                                                                     </p>
                                                                   </xml>}/>
                                                   </xml>) wr)
                                    }/>
                      </xml>}/>
      </body>
    </xml>        
    end
    
fun
main () = return <xml>
  <head>
    <title>XForms examples translated to Ur/Web</title>
  </head>
  <body>
    <h1>XForms in Ur/Web</h1>
    <p>See the corresponding code at: <a href="http://www.agencexml.com/xsltforms.htm">XSLTForms demos</a></p>
  
    <p>Beginner examples</p>
  
    <a link={ex_input ()}>Input</a>
    <a link={ex_incremental ()}>Incremental</a>
    <a link={ex_checkbox ()}>Checkbox</a>
    <a link={ex_select_model ()}>Select model</a>
    <a link={ex_button ()}>Button</a>

    <p>Advanced examples</p>

    <a link={ex_calculator ()}>Calculator</a>
    <a link={ex_contact ()}>Contact</a>
    <a link={ex_writers ()}>Writers</a>
  </body>
</xml>
