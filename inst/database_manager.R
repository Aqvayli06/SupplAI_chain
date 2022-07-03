# define some credentials
credentials <- data.frame(
  user = c("farid", "omar"), # mandatory
  password = c("akka123", "12345"), # mandatory
  start = c("2020-12-15","2020-12-15"), # optinal (all others)
  expire = c(NA, "2021-12-31"),
  admin = c(TRUE, FALSE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

library("shiny")
library("shinymanager")
library("dplyr")
# you can use keyring package to set database key
library("keyring")

key_set("R-keyring-test-service", "donaldduck")
# keyring::key_set_with_value("R-keyring-test-service", "donaldduck",keyring = "farid")

db_name <- "./inst/db_warehouse_mgmt.sqlite"
# Init the database
create_db(
  credentials_data = credentials,
  sqlite_path = db_name, # will be created
  passphrase = key_get("R-keyring-test-service", "donaldduck")
  # passphrase = "passphrase_wihtout_keyring"
)

db_con <- open_warehouse_db(db_name = db_name)

initialize_warehouse_database(db_con)

odbc::dbDisconnect(db_con)

#

# res <- odbc::dbSendQuery(db_con , "SELECT * FROM product_table WHERE nom_article = 'Torche' ")
# odbc::dbFetch(res)
# odbc::dbClearResult(res)

