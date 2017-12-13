open Dataorg

(* various meter types
 * e.g. electricity, but also water, natgas, ...
 *)
sequence meter_type_s
table meter_types :
      { Id : int 
      , Nam : string
      , UnitOfMeasure : string (* what kind of units of measure we use *)
      } PRIMARY KEY Id,
      CONSTRAINT C1 CHECK Nam <> '',
      CONSTRAINT UC1 UNIQUE Nam

task initialize = fn () =>
                     x <- Sql.insertIfMissing meter_types { Id = 1, Nam = "Electricity", UnitOfMeasure = "kW*h" };
                     x <- (if x then nextval meter_type_s else return 0);
                     x <- Sql.insertIfMissing meter_types { Id = 2, Nam = "Water", UnitOfMeasure = "m^3" };
                     x <- (if x then nextval meter_type_s else return 0);
					 return ()

(* all meters can be grouped within a site *)
sequence meter_groups_s
table meter_groups :
          { Id : int
	      , Nam : string
	      , SiteId : int
	      , Active : bool
	      }
          PRIMARY KEY Id,
	      CONSTRAINT MeterGroupsUC1 UNIQUE (SiteId, Nam),
    	  CONSTRAINT FK1 FOREIGN KEY (SiteId) REFERENCES sites(Id)

structure MeterDirection = Enum.Make(struct
                                    val cols = {Asc = "asc", Desc = "desc", Bidi = "bidi"}
	                            end)
          
(* meters *)
sequence meters_s
table meters :
	      { Id : int
	      , MeterTypeId : int
	      , OwnerId : int
	      , MeterGroupId : int
          , Direction : MeterDirection.t
	      , VersionNr : int (* current version of meter, always increasing *)
          }
          PRIMARY KEY Id,
      CONSTRAINT MetersFK1 FOREIGN KEY (MeterTypeId) REFERENCES meter_types(Id),
	  CONSTRAINT MetersFK2 FOREIGN KEY (OwnerId) REFERENCES users(Id),
      CONSTRAINT MetersFK3 FOREIGN KEY (MeterGroupId) REFERENCES meter_groups(Id)

(* everything about the meter that is versionable *)
sequence meter_versions_s
table meter_versions :
	      { Id : int 
	      , MeterId : int
	      , MeterVersionNr : int
	      , Nam : string
	      , StartQty : float
	      , MeterNo : string
	      , InstallTime : time
	      , Description : string
	      , Photo : option blob
	      }
          PRIMARY KEY Id,
	  (* every version of a meter is stored exactly once *)
      CONSTRAINT MeterVersionsUC1 UNIQUE (MeterId, MeterVersionNr),
      CONSTRAINT MeterVersionFK1 FOREIGN KEY (MeterId) REFERENCES meters(Id)

sequence readings_s
table readings :
          { Id : int
	      , MeterId : int
	      , MeterVersionNr : int
	      , Qty : float (* qty of this reading *)
	      , QtyDelta : float (* qty - previous qty *)
	      , ReadingDateTime : time
	      , CreatedOn : time
	      , Comments : string
	      }
          PRIMARY KEY Id,
          CONSTRAINT ReadingsFK1 FOREIGN KEY (MeterId, MeterVersionNr) REFERENCES meter_versions (MeterId, MeterVersionNr),
          CONSTRAINT ReadingsFK2 FOREIGN KEY (MeterId) REFERENCES meters(Id)

task initialize =
  fn () =>
     x <- Sql.insertIfMissing meter_groups { Id = 1, Nam = "Apartment 1", SiteId = 1, Active = True };
     x <- (if x then nextval meter_groups_s else return 0);
     x <- Sql.insertIfMissing meters { Id = 1, MeterTypeId = 1, OwnerId = 1, MeterGroupId = 1, Direction = MeterDirection.make.Asc, VersionNr = 1 };
     x <- (if x then nextval meters_s else return 0);
     x <- Sql.insertIfMissing meter_versions {
          Id = 1, MeterId = 1,
          MeterVersionNr = 1,
          Nam = "Meter sample",
          StartQty = 93598.0,
          MeterNo = "15869",
          InstallTime = fromDatetime 1979 8 31 0 0 0,
          Description = "",
          Photo = None
          };
     x <- (if x then nextval meter_versions_s else return 0);
     x <- Sql.insertIfMissing readings { Id = 1, Qty = 93971.0, QtyDelta = 373.0, MeterId = 1, MeterVersionNr = 1, CreatedOn = fromDatetime 2017 12 7 0 0 0, ReadingDateTime = fromDatetime 2013 8 1 16 8 0, Comments = "" };
     x <- (if x then nextval readings_s else return 0);
     x <- Sql.insertIfMissing readings { Id = 2, Qty = 94443.0, QtyDelta = 472.0, MeterId = 1, MeterVersionNr = 1, CreatedOn = fromDatetime 2017 12 7 0 0 0, ReadingDateTime = fromDatetime 2013 9 2 16 10 0, Comments = "" };
     x <- (if x then nextval readings_s else return 0);
     return ()
