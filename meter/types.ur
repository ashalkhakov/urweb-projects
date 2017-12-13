con default a = unit -> transaction a

fun mkDefault [a ::: Type] (f : unit -> transaction a) : default a = f
fun getDefault [a ::: Type] (d : default a) : transaction a = d ()

datatype site_id = SiteId of int
datatype meter_type_id = MeterTypeId of int
datatype meter_group_id = MeterGroupId of int
datatype meter_id = MeterId of int
datatype reading_id = ReadingId of int

type site =
    { Site : site_id
    , Nam : string
    , Active : bool
    }                       

val default_site : default site = mkDefault (fn {} => return { Site = SiteId(-1), Nam = "", Active = True })

type meter_type =
    { MeterType : meter_type_id
    , Nam : string
    , UnitOfMeasure : string
    }

val default_meter_type =
    mkDefault (fn {} => return { MeterType = MeterTypeId(-1), Nam = "", UnitOfMeasure = "" })

type meter_group =
    { MeterGroup : meter_group_id
    , Site : site_id
    , Nam : string
    , Active : bool
    }

fun default_meter_group siteid =
    mkDefault (fn {} => return { MeterGroup = MeterGroupId(-1), Site = siteid, Nam = "", Active = True })

fun default_meter siteid metertypeid metergroupid =
    mkDefault (fn {} =>
                  return {
                  Meter = MeterId(-1)
                , MeterType = metertypeid
                , OwnerId = 0                      
                , MeterGroup = metergroupid                                                        
                , VersionNr = 1
                , Nam = ""
                , StartQty = 0.0
                , MeterNo = ""
                , InstallTime = minTime
                , Description = ""
              })
