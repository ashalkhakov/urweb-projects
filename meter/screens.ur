
con meter_cols =
     [ ID = int
     , MName = string
     , MType = string
     , MeterNr = string
     , Version = int
     , Direction = string
     , Group = string
     , Description = string
     , StartQty = float
     , InstallTime = time
     , Photo = option (url)
     ]

type meter = $meter_cols
type reading =
     { ID : int
     , Qty : float
     , Consumption : float
     , MeterID : int
     , MeterVersion : int
     , ReadingNr : int
     , Time : time
     }

type meter_screen = $(map source meter_cols)
     
type meter_info_screen = { Meter : meter, Readings : list reading }

(* TODO:
meter info should also show a line chart of consumption *)

open Dataorg
open Data

val not_mocking = True

fun get_meter (id : int) : transaction meter =
    if not_mocking then
	    m <- Top.oneRow (SELECT
                           M.Id AS ID,
                           V.Nam AS MName,
                           MT.Nam AS MType,
                           V.MeterNo AS MeterNr,
                           M.VersionNr AS Version,
                           M.Direction AS Direction,
                           MG.Nam AS Group,
                           V.Description AS Description,
                           V.StartQty AS StartQty,
                           V.InstallTime AS InstallTime,
                           NULL AS Photo
                         FROM meters AS M
                           INNER JOIN meter_versions AS V ON M.Id = V.MeterId
                           AND M.VersionNr = V.MeterVersionNr
                           INNER JOIN meter_types AS MT ON M.MeterTypeId = MT.Id
                           INNER JOIN meter_groups AS MG ON M.MeterGroupId = MG.Id
                         WHERE M.Id = {[id]});
        return (m -- #Direction ++ {Direction = @show MeterDirection.show_t m.Direction})
    else
        return {
        ID = id,
        MName = "Meter sample",
        MType = "electric",
        MeterNr = "15869",
        Version = 1,
        Direction = "ASC",
        Group = "Apartment 1",
        Description = "",
        StartQty = 93598.0,
        InstallTime = fromDatetime 1979 8 31 0 0 0,
        Photo = None
        }

fun get_meter_info (id : int): transaction meter_info_screen =
    m <- get_meter id;
    (lst, _) <- (if not_mocking then
    query (SELECT
                         R.Id AS ID,
                         R.Qty AS Qty,
                         R.QtyDelta AS Consumption,
                         R.MeterId AS MeterID,
                         R.MeterVersionNr AS MeterVersion,
                         R.ReadingDateTime AS Time
                       FROM readings AS R
                       WHERE R.MeterId = {[id]}
                       ORDER BY R.ReadingDateTime DESC)
                 (fn r (xs, c) => return ((r ++ {ReadingNr = c}) :: xs, c+1))
                 ([],1)
    else
        return (
    (if id = 1 then { ID = 1, Qty = 93971.0, Consumption = 373.0, MeterID = id, MeterVersion = 1, ReadingNr = 1, Time = fromDatetime 2013 8 1 16 8 0 }
         :: { ID = 2, Qty = 94443.0, Consumption = 472.0, MeterID = id, MeterVersion = 1, ReadingNr = 2, Time = fromDatetime 2013 9 2 16 10 0 }
                                                                                  :: [] else []), 0));
    return { Meter = m
           , Readings = lst
           }
                         
fun get_meter_by_id (id: int) : transaction meter_screen =
    m <- get_meter id;
    (* given a record, put every field into a source *)
    Monad.mapR [fn x => x] [source] (fn [nm ::_] [t ::_] x => source x) m

(* FIXME: doesn't work if [s] is mutated elsewhere! *)
fun datetime_entry (s) =
    init <- get s;
    x <- source (timef "%Y-%m-%dT%H:%M" init);
    invalid <- source False;
    let
        fun change_handler () =
			v <- get x;
            (* YYYY-MM-DDTHH:MM *)
            let
                val y = read (substring v 0 4) : option int
                val mo = read (substring v 5 2) : option int
                val d = read (substring v 8 2) : option int
                val h = read (substring v 11 2) : option int
                val mi = read (substring v 14 2) : option int
            in
                case (y, mo, d, h, mi) of
                    (Some y, Some mo, Some d, Some h, Some mi) =>
		            set invalid False;
                    set s (fromDatetime y mo d h mi 0)
                  | (_, _, _, _, _) =>
                    alert "woops";
                    set invalid True
            end
        fun update () =
		    v <- get s;
		    set x (timef "%Y-%m-%dT%H:%M" v)
        val chunk = <xml>
	      <cdatetime_local source={x} onchange={change_handler ()}/>
	    </xml>
    in
        return (chunk, update)
    end

(* FIXME: doesn't work if [s] is mutated elsewhere! *)
fun quantity_entry (s) =
	qty <- get s;
    invalid <- source False;
	quantity <- source (Some qty);
    let
        fun change_handler () =
            v <- get quantity;
            case v of
                None =>
                alert "woops";
                set invalid True
              | Some v =>
                set invalid False;
                set s v
        fun update () =
		    v <- get s;
		    set quantity (Some v)
        val chunk = <xml>
          <cnumber onchange={change_handler ()} source={quantity}/>
        </xml>
    in 
	    return (chunk, update)
    end

fun snapshot [r ::: {Type}]
             (fl : folder r)
             (row : $(map (fn x => source x) r)) =
    @Monad.mapR transaction_monad
     [fn x => source x] [fn x => x]
     (fn [nm ::_] [t ::_] x => get x) fl row
    
fun reset_values [r ::: {Type}]
                 (fl : folder r)
                 (init : $r)
                 (src : $(map (fn x => source x) r)) =
    @Monad.appR2 transaction_monad
      [fn x => x] [source]
      (fn [nm ::_] [t ::_] v s => set s v)
      fl
      init src

style input_form
    
fun
meter_edit (id : string) : transaction page =
meter <- get_meter_by_id (readError id);
return <xml>
  <head>
    <title>Edit</title>
    <link rel="stylesheet" type="text/css" href="/meterstyle.css"/>
  </head>  
  <body>
    <active code={
meter_init <- snapshot meter;
(install_time, rf_install_time) <- datetime_entry meter.InstallTime;
(start_qty, rf_start_qty) <- quantity_entry meter.StartQty;
return <xml>
  <div class={input_form}>
  <div>
	<label >Meter name:</label>
	<ctextbox source={meter.MName}/>
  </div>
  <div>
	<label>Meter type:</label>
  	<cselect  source={meter.MType}>
    	<coption value="electric">electric</coption>
	</cselect>
  </div>
  <div>
    <label >Start quantity:</label>
    {start_qty}
</div>
<div>
                                <label >No:</label>
                                <ctextbox  source={meter.MeterNr}/>
  </div>
<div>  

                                <label >Install time</label>
                                {install_time}
                                </div>
  <div>
    <label>Count direction</label>
    <cselect source={meter.Direction}>
      <coption value="asc">ascending</coption>
      <coption value="desc">descending</coption>
      <coption value="bidi">bidirectional</coption>
    </cselect>
  </div>
  <div>
    <label>Description</label>
    <ctextarea source={meter.Description} rows=3 placeholder="Description..." />
  </div>
  <div>
    <button onclick={fn _ =>
                        reset_values meter_init meter;
                        rf_start_qty ();
                        rf_install_time ()}>Reset</button>
    <button>Save</button>
    </div>
  </div>  
</xml>
}/>
 </body>
  </xml>

fun
meter_info (id : int) =
let
in
    mi <- get_meter_info id;
    return <xml>
      <a link={meter_edit (show id)}>Edit</a>
      
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
     
type meter_group_meter = { MeterName : string, ID : int }
type meter_group_screen = { GroupName : string, Meters : list meter_group_meter }
     
fun
get_meter_list (siteid : int) : transaction (list meter_group_screen) =
if not_mocking then
let
    val q = (SELECT Mg.Id, Mg.Nam, M.Id, V.Nam
                        FROM meter_groups AS Mg
                          INNER JOIN meters AS M ON Mg.Id = M.MeterGroupId
                          INNER JOIN meter_versions AS V ON M.Id = V.MeterId
                          AND M.VersionNr = V.MeterVersionNr
                        WHERE Mg.SiteId = {[siteid]}
                          AND Mg.Active = TRUE)
in
	x <- Sql.listify [#Mg] q;
    return (List.mp (fn (x, r) => {
                        GroupName = x.Nam,
                        Meters = List.mp (fn x => { MeterName = x.V.Nam, ID = x.M.Id }) r
                    }) x)
end
else
    let
        val m0 = { MeterName = "Meter sample", ID = 1 }
        val m1 = { MeterName = "Meter sample 2", ID = 2 }
    in
        return ({ GroupName = "Apartment 1", Meters = m0 :: m1 :: [] } :: [])
    end
                          
fun
meter_list () =
let
in
    meters <- get_meter_list (1);
return <xml>
  <head>
    <title>Meter: list</title>
    <link rel="stylesheet" type="text/css" href="/meterstyle.css"/>
  </head>
  <body>
    <active code={
                  sel_meter_id <- source None;
                  page <- source <xml></xml>;
                  return <xml>
                    <div>{
                  List.mapX (fn gr =>
                                <xml>
                                  <div>
                                    <h2>{[gr.GroupName]}</h2>

                                    <ul>{List.mapX
                                             (fn m => <xml>
                                               <li>
                                                 <button onclick={fn _ =>
                                                                     set sel_meter_id (Some m.ID);
                                                                     v <- rpc (meter_info m.ID);
                                                                     set page v
													}>{[m.MeterName]}</button>
                                               </li>
                                             </xml>) gr.Meters}
                                    </ul>
                                  </div>
                    </xml>) meters}</div>
                    <div>
                      <dyn signal={signal page}/>
                    </div>
                  </xml>}/>
  </body>
  </xml>
end
