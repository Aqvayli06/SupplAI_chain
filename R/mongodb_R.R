# Connect to demo server

open_mogo_db_connection <- function(){
  mongo_con <- mongolite::mongo("mtcars", url =
                                  "mongodb+srv://readwrite:test@cluster0-84vdt.mongodb.net/test")
  
  mongo_con$insert(mtcars)
  
  # mongo_con$import()
  
  mydata <- mongo_con$find()
}

