fun insert_list
[fields] [uniques]
(injs : $(map sql_injectable fields))
(fl : folder fields)
(tab : sql_table fields uniques)
(lfs : list $fields) =
List.foldlM
    (fn (fs : $fields) _ =>
	dml (insert
		 tab
		 (@Top.map2 [sql_injectable] [ident] [sql_exp [] [] []] (fn [t] => @sql_inject) fl injs fs)))
{} lfs

fun insert_list_def
[fields1] [fields2] [uniques] [fields1 ~ fields2]
(injs : $(map sql_injectable (fields1++fields2)))
(fl : folder (fields1++fields2))
(tab : sql_table (fields1++fields2) uniques)
(defs : $fields1)
(lfs : list $fields2) =
List.foldlM
    (fn (fs : $fields2) _ =>
	dml (insert
		 tab
		 (@Top.map2 [sql_injectable] [ident] [sql_exp [] [] []] (fn [t] => @sql_inject) fl injs (fs++defs))))
{} lfs

sequence exercise_kind_s
table exercise_kind : {
      ExerciseKindID : int,
      Description : string
} PRIMARY KEY ExerciseKindID,
      CONSTRAINT C1 CHECK Description <> ''

sequence exercise_type_s
table exercise_type : {
      ExerciseTypeID : int,
      Description : string,
      ExerciseKindID : int
} PRIMARY KEY ExerciseTypeID,
      CONSTRAINT FK1 FOREIGN KEY (ExerciseKindID) REFERENCES exercise_kind (ExerciseKindID),
      CONSTRAINT C1 CHECK Description <> ''

sequence exercise_s
table exercise : {
      ExerciseID : int,
      Title : string,
      Description : string,
      ExerciseTypeID : int
} PRIMARY KEY ExerciseID,
      CONSTRAINT FK1 FOREIGN KEY (ExerciseTypeID) REFERENCES exercise_type (ExerciseTypeID),
      CONSTRAINT C1 CHECK Title <> ''

sequence exercise_progression_s
table exercise_progression : {
      ExerciseProgressionID : int,
      ExerciseTypeID : int      
} PRIMARY KEY ExerciseProgressionID,
      CONSTRAINT FK1 FOREIGN KEY (ExerciseTypeID) REFERENCES exercise_type (ExerciseTypeID)

sequence exercise_progression_line_s
table exercise_progression_line : {
      ExerciseProgressionLineID : int,
      ExerciseProgressionID : int,
      LineNr : int,
      ExerciseID : int
} PRIMARY KEY ExerciseProgressionLineID,
      CONSTRAINT FK1 FOREIGN KEY (ExerciseProgressionID) REFERENCES exercise_progression (ExerciseProgressionID),
      CONSTRAINT FK2 FOREIGN KEY (ExerciseID) REFERENCES exercise (ExerciseID),
      CONSTRAINT UC1 UNIQUE (ExerciseProgressionID, LineNr),
      CONSTRAINT UC2 UNIQUE (ExerciseProgressionID, ExerciseID)

sequence workout_routine_s
table workout_routine : {
      WorkoutRoutineID : int
} PRIMARY KEY WorkoutRoutineID

sequence workout_routine_appointment_s
table workout_routine_appointment : {
      WorkoutRoutineAppointmentID : int,
      WorkoutRoutineID : int,
      WeekDay : int
} PRIMARY KEY WorkoutRoutineAppointmentID,
      CONSTRAINT FK1 FOREIGN KEY (WorkoutRoutineID) REFERENCES workout_routine (WorkoutRoutineID),
      CONSTRAINT CK1 CHECK (WeekDay >= 0 AND WeekDay <= 6)

sequence workout_routine_line_s
table workout_routine_line : {
      WorkoutRoutineLineID : int,
      WorkoutRoutineID : int,
      LineNr : int,
      ExerciseProgressionID : int,
      NumSets : int,
      RepsMin: int,
      RepsMax : int,
      RestMin : time,
      RestMax : time
} PRIMARY KEY WorkoutRoutineLineID,
      CONSTRAINT FK1 FOREIGN KEY (ExerciseProgressionID) REFERENCES exercise_progression (ExerciseProgressionID),
      CONSTRAINT FK2 FOREIGN KEY (WorkoutRoutineID) REFERENCES workout_routine (WorkoutRoutineID),
      CONSTRAINT UC1 UNIQUE (WorkoutRoutineID, LineNr),
      CONSTRAINT UC2 UNIQUE (WorkoutRoutineID, ExerciseProgressionID),
      CONSTRAINT C1 CHECK (RepsMin >= 0 AND RepsMax >= 0 AND RepsMin <= RepsMax),
      CONSTRAINT C2 CHECK (RestMin <= RestMax)      
      
task initialize =
  fn () =>
     count <- oneRowE1 (SELECT COUNT( * ) FROM exercise_kind);
     if count > 0 then return ()
     else
	 (* SQLITE does not support setval/nextval! *)
	 (*setval exercise_kind_s 3;*)
	 exkind_warmup_id <- nextval exercise_kind_s;
	 exkind_stretch_id <- nextval exercise_kind_s;
	 exkind_strength_id <- nextval exercise_kind_s;
	 insert_list exercise_kind
		     ({ExerciseKindID = exkind_warmup_id, Description = "Dynamic warm-up"}
			  :: {ExerciseKindID = exkind_stretch_id, Description = "Static stretch"}
			  :: {ExerciseKindID = exkind_strength_id, Description = "Strength"}
			  :: []);
	 squat_id <- nextval exercise_type_s;
	 pullup_id <- nextval exercise_type_s;
	 handstand_push_up_id <- nextval exercise_type_s;
	 leg_raises_id <- nextval exercise_type_s;
	 push_up_id <- nextval exercise_type_s;
	 dip_id <- nextval exercise_type_s;
	 horizontal_pulls_id <- nextval exercise_type_s;
	 plank_id <- nextval exercise_type_s;
	 
	 insert_list_def
	     exercise_type
	     {ExerciseKindID= exkind_strength_id}
	     ({ExerciseTypeID= squat_id, Description= "Squat"}
		  :: {ExerciseTypeID= pullup_id, Description= "Pull up"}
		  :: {ExerciseTypeID= handstand_push_up_id, Description= "Handstand push up"}
		  :: {ExerciseTypeID= leg_raises_id, Description= "Leg raises"}
		  :: {ExerciseTypeID= push_up_id, Description= "Push up"}
		  :: {ExerciseTypeID= dip_id, Description= "Dip"}
		  :: {ExerciseTypeID= horizontal_pulls_id, Description= "Horizontal pulls"}
		  :: {ExerciseTypeID= plank_id, Description= "Plank"}
		  :: []);

	 ex_squat1_id <- nextval exercise_s;
	 ex_squat2_id <- nextval exercise_s;
	 ex_squat3_id <- nextval exercise_s;
	 ex_squat4_id <- nextval exercise_s;
	 ex_squat5_id <- nextval exercise_s;
	 ex_squat6_id <- nextval exercise_s;
	 ex_squat7_id <- nextval exercise_s;
	 ex_squat8_id <- nextval exercise_s;
	 ex_squat9_id <- nextval exercise_s;
	 ex_squat10_id <- nextval exercise_s;
	 ex_squat11_id <- nextval exercise_s;
	 ex_squat12_id <- nextval exercise_s;
	 ex_squat13_id <- nextval exercise_s;
	 
	 insert_list_def exercise {ExerciseTypeID= squat_id}
		     ({ExerciseID= ex_squat1_id, Title="Assisted squats", Description = "Rest your hands on the back of a chair in front you. Lower yourself till your thighs are parallel with the floor, and come back up, using the chair as a support."}
			  :: {ExerciseID= ex_squat2_id, Title="Deep assisted squats", Description = "Same as above, but squat through the full range of motion."}
			  :: {ExerciseID= ex_squat3_id, Title="Squats", Description="Without any help, lower yourself till your thighs are parallel with the floor, and come back up"}
			  :: {ExerciseID= ex_squat4_id, Title="Deep squats", Description="Same as above, but squat through the full range of motion."}
			  :: {ExerciseID= ex_squat5_id, Title="Bulgarian split squats", Description="Rest your back foot on an elevated platform, with the sole pointing up. Keep your weight over your front leg as you lower yourself."}
			  :: {ExerciseID= ex_squat6_id, Title="Beginner shrimp squats (also known as airborne lunges)", Description="Lower yourself on one leg until the knee and toes of the leg that was bent under you touch the floor at the same time. This must be a slow controlled movement. Then come back up, trying to lift your knee and toes at the same time."}
			  :: {ExerciseID= ex_squat7_id, Title="Assisted one legged squats", Description="Performed with a bench or a chair next to you, extend one leg straight in front of you, and lower yourself all the way down, till your butt touches your heel. The heel must remain on the floor however. Help yourself back up by pushing on the chair."}
			  :: {ExerciseID= ex_squat8_id, Title="Balance assisted one legged squats", Description="Same as above, but using a door, door frame, or any vertical structure, mainly for support and balance."}
			  :: {ExerciseID= ex_squat9_id, Title="Weighted one legged squats", Description="This is somewhat counter-intuitive, but weighted one legged squats are actually easier than standard ones, as the object you hold in front of you acts as a counter-weight. You can use dumbbells if you have some, but any heavy object such as a dictionary will do. Start with a 5kg weight and move down in weight progressively in 1 kg increments until you can do unassisted pistols."}
			  :: {ExerciseID= ex_squat10_id, Title="One-legged squats (a.k.a. pistol squats)", Description="Same as above, but without any assitance."}
			  :: {ExerciseID= ex_squat11_id, Title="Renegade pistols", Description="Lower yourself on one leg; then switch legs at the bottom (bring the extended leg back next to the other one,into a full squat position), before coming back up. Video demonstration."}
			  :: {ExerciseID= ex_squat12_id, Title="Intermediate shrimps", Description="Same as shrimp squats, but your back foot must come off the floor before you come back up. This variation shifts the emphasis of the exercise to your quadriceps."}
			  :: {ExerciseID= ex_squat13_id, Title="Advanced shrimps", Description="Hold your back foot in your hand throughout the exercise. Your knee must touch the floor in a controlled manner."}
			  :: []);

	 prog_squat_id <- nextval exercise_progression_s;
	 
	 dml (INSERT INTO exercise_progression (ExerciseProgressionID, ExerciseTypeID) VALUES ({[prog_squat_id]}, {[squat_id]}));
	 
	 exp_squat1_id <- nextval exercise_progression_line_s;
	 exp_squat2_id <- nextval exercise_progression_line_s;
	 exp_squat3_id <- nextval exercise_progression_line_s;
	 exp_squat4_id <- nextval exercise_progression_line_s;
	 exp_squat5_id <- nextval exercise_progression_line_s;
	 exp_squat6_id <- nextval exercise_progression_line_s;
	 exp_squat7_id <- nextval exercise_progression_line_s;
	 exp_squat8_id <- nextval exercise_progression_line_s;
	 exp_squat9_id <- nextval exercise_progression_line_s;
	 exp_squat10_id <- nextval exercise_progression_line_s;
	 exp_squat11_id <- nextval exercise_progression_line_s;
	 exp_squat12_id <- nextval exercise_progression_line_s;
	 exp_squat13_id <- nextval exercise_progression_line_s;

	 insert_list_def exercise_progression_line
		     {ExerciseProgressionID=prog_squat_id}
		     ({ExerciseProgressionLineID= exp_squat1_id, LineNr=1, ExerciseID= ex_squat1_id}
			  ::{ExerciseProgressionLineID=exp_squat2_id, LineNr=2, ExerciseID= ex_squat2_id}
			  ::{ExerciseProgressionLineID=exp_squat3_id, LineNr=3, ExerciseID= ex_squat3_id}
			  ::{ExerciseProgressionLineID=exp_squat4_id, LineNr=4, ExerciseID= ex_squat4_id}
			  ::{ExerciseProgressionLineID=exp_squat5_id, LineNr=5, ExerciseID= ex_squat5_id}
			  ::{ExerciseProgressionLineID=exp_squat6_id, LineNr=6, ExerciseID= ex_squat6_id}
			  ::{ExerciseProgressionLineID=exp_squat7_id, LineNr=7, ExerciseID= ex_squat7_id}
			  ::{ExerciseProgressionLineID=exp_squat8_id, LineNr=8, ExerciseID= ex_squat8_id}
			  ::{ExerciseProgressionLineID=exp_squat9_id, LineNr=9, ExerciseID= ex_squat9_id}
			  ::{ExerciseProgressionLineID=exp_squat10_id, LineNr=10, ExerciseID= ex_squat10_id}
			  ::{ExerciseProgressionLineID=exp_squat11_id, LineNr=11, ExerciseID= ex_squat11_id}
			  ::{ExerciseProgressionLineID=exp_squat12_id, LineNr=12, ExerciseID= ex_squat12_id}
			  ::{ExerciseProgressionLineID=exp_squat13_id, LineNr=13, ExerciseID= ex_squat13_id}								  
			  ::[]);

	 (* workout routine repetitions *)
	 workout_routine_sw_id <- nextval workout_routine_s;
	 dml (INSERT INTO workout_routine (WorkoutRoutineID) VALUES ({[workout_routine_sw_id]}));

	 workout_routine_sw_appointment1_id <- nextval workout_routine_appointment_s;
	 workout_routine_sw_appointment2_id <- nextval workout_routine_appointment_s;
	 workout_routine_sw_appointment3_id <- nextval workout_routine_appointment_s;
	 
	 insert_list_def workout_routine_appointment {WorkoutRoutineID= workout_routine_sw_id}
			 ({WorkoutRoutineAppointmentID= workout_routine_sw_appointment1_id, WeekDay= 1}
			      :: {WorkoutRoutineAppointmentID= workout_routine_sw_appointment2_id, WeekDay= 3}
			      :: {WorkoutRoutineAppointmentID= workout_routine_sw_appointment3_id, WeekDay= 5}
			      :: []);

	 workout_routine_line1_id <- nextval workout_routine_line_s;
	 
	 insert_list_def workout_routine_line
			 { WorkoutRoutineID= workout_routine_sw_id
			 , NumSets=3, RepsMin=4, RepsMax=8
			 , RestMin=addSeconds minTime 60, RestMax=addSeconds minTime 120
			 }
			 ({WorkoutRoutineLineID= workout_routine_line1_id, LineNr=1, ExerciseProgressionID= prog_squat_id}
			      :: [])
     

(* UI *)
	 
structure ExerciseKindG =
Dbgrid.Make(struct
		val tab = exercise_kind
		con key = [ExerciseKindID = _]
		val raw = {ExerciseKindID = {New = nextval exercise_kind_s,
					     Inj = _},
			   Description = {New = return "new row, please change", Inj = _}
			  }
		val cols = {
		    Id = Dbgrid.Direct.readOnly [#ExerciseKindID] "ID" Dbgrid.Direct.int
		  , Description = Dbgrid.Direct.editable [#Description] "Description" Dbgrid.Direct.string
		}
		val aggregates = {}
		val pageLength = Some 10
	    end)

structure ExerciseTypeG =
Dbgrid.Make(struct
		structure F = Dbgrid.Direct.Foreign(struct
							con nm = #ExerciseKindID
							val tab = exercise_kind
							fun render r = r.Description
						    end)
		
		val tab = exercise_type
		con key = [ExerciseTypeID = _]
		val raw = {ExerciseTypeID = {New = nextval exercise_type_s,
					     Inj = _},
			   ExerciseKindID = {New = return 1, Inj = _},
			   Description = {New = return "new row, please change", Inj = _}
			  }
		val cols = {
		    Id = Dbgrid.Direct.readOnly [#ExerciseTypeID] "ID" Dbgrid.Direct.int
		  , KindId = Dbgrid.Direct.editable [#ExerciseKindID] "Kind ID" F.meta
		  , Description = Dbgrid.Direct.editable [#Description] "Description" Dbgrid.Direct.string
		}
		val aggregates = {}
		val pageLength = Some 10
	    end)

structure ExerciseG =
Dbgrid.Make(struct
		structure F = Dbgrid.Direct.Foreign(struct
							con nm = #ExerciseTypeID
							val tab = exercise_type
							fun render r = r.Description
						    end)

		val tab = exercise
		con key = [ExerciseID = _]
		val raw = {ExerciseID = {New = nextval exercise_s, Inj = _},
			   Title = {New = return "new title", Inj = _},
			   Description = {New = return "new description", Inj = _},
			   ExerciseTypeID = {New = return 1, Inj = _}
			  }
		val cols = {
		    Id = Dbgrid.Direct.readOnly [#ExerciseID] "ID" Dbgrid.Direct.int
		  , Title = Dbgrid.Direct.editable [#Title] "Title" Dbgrid.Direct.string
		  , Description = Dbgrid.Direct.editable [#Description] "Description" Dbgrid.Direct.string
		  , TypeId = Dbgrid.Direct.editable [#ExerciseTypeID] "Type ID" F.meta
		}
		val aggregates = {}
		val pageLength = Some 10
	    end)

(* main screen *)

fun
main () =
exerciseKindG <- ExerciseKindG.grid;
exerciseTypeG <- ExerciseTypeG.grid;
exerciseG <- ExerciseG.grid;
return <xml>
  <body onload={ExerciseKindG.sync exerciseKindG; ExerciseTypeG.sync exerciseTypeG; ExerciseG.sync exerciseG}>
    <h1>Exercise kinds</h1>
    {ExerciseKindG.render exerciseKindG}
    <h1>Exercise types</h1>
    {ExerciseTypeG.render exerciseTypeG}
    <h1>Exercises</h1>
    {ExerciseG.render exerciseG}
  </body>
</xml>
