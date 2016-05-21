


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
  
  
  
  # szukamy po id, ktorych rekordow nie ma w tabeli
  # id.stare <- dbGetQuery(db, "select id from dane")$id
  # id.nowe  <- subset(dane$id, !dane$id %in% id.stare)
    
  
  
  
  # jesli sa nowe posty dodaj je do tabeli
  # if(length(id.nowe.dane) > 0){
  #   
  #   id.nowe.dane  <- which(dane$id == id.nowe.dane)
    
    
    dbSendPreparedQuery(db, "INSERT INTO dane VALUES (?,?,?,?,?,?,?)",
                        dane[, -c(5, 7)])
    
  # }
  # else { cat("\nNie ma nowych danych w tabeli dane!\n")}
  
  
  
  
  suppressWarnings(dbDisconnect(db))
  invisible()
  
}