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
table workout_routine_appointment {
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
     (* SQLITE does not support setval/nextval! *)
     (*setval exercise_kind_s 3;*)
     insert_list exercise_kind ({ExerciseKindID = 1, Description = "Dynamic warm-up"}
				    :: {ExerciseKindID = 2, Description = "Static stretch"}
				    :: {ExerciseKindID = 3, Description = "Strength"}
				    :: []);
     (*setval exercise_type_s 8;*)
     squat_id <- return 1;
     pullup_id <- return 2;
     insert_list_def exercise_type {ExerciseKindID=3} ({ExerciseTypeID= squat_id, Description= "Squat"}
				    :: {ExerciseTypeID= pullup_id, Description= "Pull up"}
				    :: {ExerciseTypeID= 3, Description= "Handstand push up"}
				    :: {ExerciseTypeID= 4, Description= "Leg raises"}
				    :: {ExerciseTypeID= 5, Description= "Push up"}
				    :: {ExerciseTypeID= 6, Description= "Dip"}
				    :: {ExerciseTypeID= 7, Description= "Horizontal pulls"}
				    :: {ExerciseTypeID= 8, Description= "Plank"}
				    :: []);
     insert_list_def exercise {ExerciseTypeID= squat_id}
		     ({ExerciseID= 1, Title="Assisted squats", Description = "Rest your hands on the back of a chair in front you. Lower yourself till your thighs are parallel with the floor, and come back up, using the chair as a support."}
			  :: {ExerciseID= 2, Title="Deep assisted squats", Description = "Same as above, but squat through the full range of motion."}
			  :: {ExerciseID= 3, Title="Squats", Description="Without any help, lower yourself till your thighs are parallel with the floor, and come back up"}
			  :: {ExerciseID= 4, Title="Deep squats", Description="Same as above, but squat through the full range of motion."}
			  :: {ExerciseID= 5, Title="Bulgarian split squats", Description="Rest your back foot on an elevated platform, with the sole pointing up. Keep your weight over your front leg as you lower yourself."}
			  :: {ExerciseID= 6, Title="Beginner shrimp squats (also known as airborne lunges)", Description="Lower yourself on one leg until the knee and toes of the leg that was bent under you touch the floor at the same time. This must be a slow controlled movement. Then come back up, trying to lift your knee and toes at the same time."}
			  :: {ExerciseID= 7, Title="Assisted one legged squats", Description="Performed with a bench or a chair next to you, extend one leg straight in front of you, and lower yourself all the way down, till your butt touches your heel. The heel must remain on the floor however. Help yourself back up by pushing on the chair."}
			  :: {ExerciseID= 8, Title="Balance assisted one legged squats", Description="Same as above, but using a door, door frame, or any vertical structure, mainly for support and balance."}
			  :: {ExerciseID= 9, Title="Weighted one legged squats", Description="This is somewhat counter-intuitive, but weighted one legged squats are actually easier than standard ones, as the object you hold in front of you acts as a counter-weight. You can use dumbbells if you have some, but any heavy object such as a dictionary will do. Start with a 5kg weight and move down in weight progressively in 1 kg increments until you can do unassisted pistols."}
			  :: {ExerciseID=10, Title="One-legged squats (a.k.a. pistol squats)", Description="Same as above, but without any assitance."}
			  :: {ExerciseID=11, Title="Renegade pistols", Description="Lower yourself on one leg; then switch legs at the bottom (bring the extended leg back next to the other one,into a full squat position), before coming back up. Video demonstration."}
			  :: {ExerciseID=12, Title="Intermediate shrimps", Description="Same as shrimp squats, but your back foot must come off the floor before you come back up. This variation shifts the emphasis of the exercise to your quadriceps."}
			  :: {ExerciseID=13, Title="Advanced shrimps", Description="Hold your back foot in your hand throughout the exercise. Your knee must touch the floor in a controlled manner."}
			  :: []);
     dml (INSERT INTO exercise_progression (ExerciseProgressionID, ExerciseTypeID) VALUES (1, {[squat_id]}));
     insert_list_def exercise_progression_line
		     {ExerciseProgressionID=1}
		     ({ExerciseProgressionLineID=1, LineNr=1, ExerciseID=1}
			  ::{ExerciseProgressionLineID=2, LineNr=2, ExerciseID=2}
			  ::{ExerciseProgressionLineID=3, LineNr=3, ExerciseID=3}
			  ::{ExerciseProgressionLineID=4, LineNr=4, ExerciseID=4}
			  ::{ExerciseProgressionLineID=5, LineNr=5, ExerciseID=5}
			  ::{ExerciseProgressionLineID=6, LineNr=6, ExerciseID=6}
			  ::{ExerciseProgressionLineID=7, LineNr=7, ExerciseID=7}
			  ::{ExerciseProgressionLineID=8, LineNr=8, ExerciseID=8}
			  ::{ExerciseProgressionLineID=9, LineNr=9, ExerciseID=9}
			  ::{ExerciseProgressionLineID=10, LineNr=10, ExerciseID=10}
			  ::{ExerciseProgressionLineID=11, LineNr=11, ExerciseID=11}
			  ::{ExerciseProgressionLineID=12, LineNr=12, ExerciseID=12}
			  ::{ExerciseProgressionLineID=13, LineNr=13, ExerciseID=13}								  
			  ::[])
     (* workout routine repetitions *)
      
fun
main () = return <xml><body>Hello</body></xml>
