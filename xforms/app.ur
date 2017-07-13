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
                    <button onclick={fn _ => clear calc}>Clear</button>
                  </td>
                </tr>
                <tr>
                  <td>
                    <button onclick={fn _ => memclear calc}>MC</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 7.0}>7</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 8.0}>8</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 9.0}>9</button>
                  </td>
                  <td>
                    <button onclick={fn _ => divide calc}>/</button>
                  </td>
                  <td></td>
                </tr>
                <tr>
                  <td>
                    <button onclick={fn _ => memrecall calc}>MR</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 4.0}>4</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 5.0}>5</button>
                  </td>
            	  <td>
                    <button onclick={fn _ => digit calc 6.0}>6</button>
                  </td>
            	  <td>
                    <button onclick={fn _ => multiply calc}>*</button>
                  </td>
                </tr>
                <tr>
                  <td>
                    <button onclick={fn _ => memsave calc}>MS</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 1.0}>1</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 2.0}>2</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 3.0}>3</button>
                  </td>
                  <td>
                    <button onclick={fn _ => subtract calc}>-</button>
                  </td>
                  <td>
                    <button onclick={fn _ => recip calc}>1/x</button>
                  </td>
                </tr>
                <tr>
                  <td>
                    <button onclick={fn _ => memadd calc}>M+</button>
                  </td>
                  <td>
                    <button onclick={fn _ => digit calc 0.0}>0</button>
                  </td>
                  <td colspan=2>
                    <button onclick={fn _ => invert calc}>+/-</button>
                  </td>
                  <td>
                    <button onclick={fn _ => add calc}>+</button>
                  </td>
                  <td>
                    <button onclick={fn _ => apply calc}>=</button>
                  </td>
                </tr>
              </table>
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
  </body>
</xml>
