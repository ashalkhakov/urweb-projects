val insert_list :
fields ::: {Type} -> uniques ::: {{Unit}}
-> $(map sql_injectable fields)
-> folder fields
-> sql_table fields uniques
-> list $fields
-> transaction unit

val insert_list_def :
    fields1 ::: {Type} -> fields2 ::: {Type} -> uniques ::: {{Unit}}
    -> [fields1 ~ fields2]
    => $(map sql_injectable (fields1++fields2))
    -> folder (fields1++fields2)
    -> sql_table (fields1++fields2) uniques
    -> $fields1
    -> list $fields2
    -> transaction unit   
																			    
val main : unit -> transaction page
