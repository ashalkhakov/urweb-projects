class default

val mkDefault : a ::: Type -> (unit -> transaction a) -> default a
val getDefault : a ::: Type -> default a -> transaction a

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

val default_site : default site

type meter_type =
    { MeterType : meter_type_id
    , Nam : string
    , UnitOfMeasure : string
    }

val default_meter_type : default meter_type
    
type meter_group =
    { MeterGroup : meter_group_id
    , Site : site_id
    , Nam : string
    , Active : bool
    }

val default_meter_group : site_id -> default meter_group

type meter =
    { Meter : meter_id
    , MeterType : meter_type_id
    , OwnerId : int (* TODO: Reference *)
    , MeterGroup : meter_group_id
    , VersionNr : int
    , Nam : string
    , StartQty : float
    , MeterNo : string
    , InstallTime : time
    , Description : string
    }

val default_meter : site_id -> meter_type_id -> meter_group_id -> default meter

