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
  </body>
</xml>
