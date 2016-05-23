library(RSQLite)


aktualizuj.dane <- function(baza, dane){
  
  db <- dbConnect(SQLite(), dbname = baza)
  
  
  if (!"dane" %in% dbListTables(db)) {
    
    dbSendQuery(db, "CREATE TABLE dane (
                id smallint,
                parent_id smallint,
                created_at datetime,
                thread_id smallint,
                user_name nvarchar,
                source nvarchar,
                body ntext
    )")
  }
  
  
  
  
    
    
    dbSendPreparedQuery(db, "INSERT INTO dane VALUES (?,?,?,?,?,?,?)",
                        dane[, -c(5, 7)])
    

  
  
  
  
  suppressWarnings(dbDisconnect(db))
  invisible()
  
}