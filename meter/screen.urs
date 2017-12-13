type meter =
     { ID : int
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
     { ID : int
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


val meter_edit : string -> transaction page
val meter_info : string -> transaction page

type meter_group_meter = { MeterName : string, ID : string }
type meter_group_screen = { GroupName : string, Meters : list meter_group_meter }

val meter_list : unit -> transaction page
