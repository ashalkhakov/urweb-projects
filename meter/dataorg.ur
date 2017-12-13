sequence sites_s
table sites :
	{ Id : int
	, Nam : string
	, Active : bool
	}
    PRIMARY KEY Id,
    CONSTRAINT SitesUC1 UNIQUE (Id)

sequence users_s
table users :
    { Id : int 
    , Email : string
    , Nam : option string
	, SiteId : int
    }
    PRIMARY KEY Id,
    CONSTRAINT UsersUC1 UNIQUE (Email),
    CONSTRAINT UsersFK1 FOREIGN KEY (SiteId) REFERENCES sites(Id)
         
task initialize = fn () =>
                     x <- Sql.insertIfMissing sites { Id = 1, Nam = "0000", Active = True };
				     x <- (if x then nextval sites_s else return 0);
                     x <- Sql.insertIfMissing users { Id = 1, Email = "admin@example.org", Nam = Some "Admin", SiteId = 1 };
				     x <- (if x then nextval users_s else return 0);
					 return ()
