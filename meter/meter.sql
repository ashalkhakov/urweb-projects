CREATE TABLE uw_Dataorg_sites_s (id INTEGER PRIMARY KEY AUTOINCREMENT);
 
 CREATE TABLE uw_Dataorg_sites(uw_id integer NOT NULL, uw_nam text NOT NULL, 
                                uw_active integer NOT NULL,
  PRIMARY KEY (uw_id),
   CONSTRAINT uw_Dataorg_sites_SitesUC1 UNIQUE (uw_id)
  );
  
  CREATE TABLE uw_Dataorg_users_s (id INTEGER PRIMARY KEY AUTOINCREMENT);
   
   CREATE TABLE uw_Dataorg_users(uw_id integer NOT NULL, 
                                  uw_email text NOT NULL, uw_nam text, 
                                  uw_siteid integer NOT NULL,
    PRIMARY KEY (uw_id),
     CONSTRAINT uw_Dataorg_users_UsersUC1 UNIQUE (uw_email),
                                                            
      CONSTRAINT uw_Dataorg_users_UsersFK1
       FOREIGN KEY (uw_siteId) REFERENCES uw_Dataorg_sites (uw_id)
    );
    
    CREATE TABLE uw_Data_meter_type_s (id INTEGER PRIMARY KEY AUTOINCREMENT);
     
     CREATE TABLE uw_Data_meter_types(uw_id integer NOT NULL, 
                                       uw_nam text NOT NULL, 
                                       uw_unitofmeasure text NOT NULL,
      PRIMARY KEY (uw_id),
       CONSTRAINT uw_Data_meter_types_C1 CHECK (uw_nam <> ''),
                                                              
        CONSTRAINT uw_Data_meter_types_UC1 UNIQUE (uw_nam)
      );
      
      CREATE TABLE uw_Data_meter_groups_s (id INTEGER PRIMARY KEY AUTOINCREMENT);
       
       CREATE TABLE uw_Data_meter_groups(uw_id integer NOT NULL, 
                                          uw_nam text NOT NULL, 
                                          uw_siteid integer NOT NULL, 
                                          uw_active integer NOT NULL,
        PRIMARY KEY (uw_id),
         CONSTRAINT uw_Data_meter_groups_MeterGroupsUC1
          UNIQUE (uw_nam, uw_siteId),
                                     
          CONSTRAINT uw_Data_meter_groups_FK1
           FOREIGN KEY (uw_siteId) REFERENCES uw_Dataorg_sites (uw_id)
        );
        
        CREATE TABLE uw_Data_meters_s (id INTEGER PRIMARY KEY AUTOINCREMENT);
         
         CREATE TABLE uw_Data_meters(uw_id integer NOT NULL, 
                                      uw_metertypeid integer NOT NULL, 
                                      uw_ownerid integer NOT NULL, 
                                      uw_metergroupid integer NOT NULL, 
                                      uw_direction text NOT NULL, 
                                      uw_versionnr integer NOT NULL,
          PRIMARY KEY (uw_id),
           CONSTRAINT uw_Data_meters_MetersFK1
            FOREIGN KEY (uw_meterTypeId) REFERENCES uw_Data_meter_types (uw_id),
                                                                                
            CONSTRAINT uw_Data_meters_MetersFK2
             FOREIGN KEY (uw_ownerId) REFERENCES uw_Dataorg_users (uw_id),
                                                                          
            CONSTRAINT uw_Data_meters_MetersFK3
             FOREIGN KEY (uw_meterGroupId) REFERENCES uw_Data_meter_groups (uw_id)
          );
          
          CREATE TABLE uw_Data_meter_versions_s (id INTEGER PRIMARY KEY AUTOINCREMENT);
           
           CREATE TABLE uw_Data_meter_versions(uw_id integer NOT NULL, 
                                                uw_meterid integer NOT NULL, 
                                                uw_meterversionnr
                                                 integer NOT NULL, 
                                                uw_nam text NOT NULL, 
                                                uw_startqty real NOT NULL, 
                                                uw_meterno text NOT NULL, 
                                                uw_installtime text NOT NULL, 
                                                uw_description text NOT NULL, 
                                                uw_photo blob,
            PRIMARY KEY (uw_id),
             CONSTRAINT uw_Data_meter_versions_MeterVersionsUC1
              UNIQUE (uw_meterVersionNr, uw_meterId),
                                                     
              CONSTRAINT uw_Data_meter_versions_MeterVersionFK1
               FOREIGN KEY (uw_meterId) REFERENCES uw_Data_meters (uw_id)
            );
            
            CREATE TABLE uw_Data_readings_s (id INTEGER PRIMARY KEY AUTOINCREMENT);
             
             CREATE TABLE uw_Data_readings(uw_id integer NOT NULL, 
                                            uw_meterid integer NOT NULL, 
                                            uw_meterversionnr integer NOT NULL, 
                                            uw_qty real NOT NULL, 
                                            uw_qtydelta real NOT NULL, 
                                            uw_readingdatetime text NOT NULL, 
                                            uw_createdon text NOT NULL, 
                                            uw_comments text NOT NULL,
              PRIMARY KEY (uw_id),
               CONSTRAINT uw_Data_readings_ReadingsFK1
                FOREIGN KEY (uw_meterVersionNr, uw_meterId) REFERENCES uw_Data_meter_versions (uw_meterVersionNr, uw_meterId),
                                                                                                                              
                CONSTRAINT uw_Data_readings_ReadingsFK2
                 FOREIGN KEY (uw_meterId) REFERENCES uw_Data_meters (uw_id)
              );
              
              