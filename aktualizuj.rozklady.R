


aktualizuj.rozklady <- function(baza, ramka){

  
  db <- dbConnect(SQLite(), dbname = baza)


  if (!"rozklady" %in% dbListTables(db)) {
    dbSendQuery(db, "create table rozklady (
                id smallint,
                data varchar, 
                godzina varchar, 
                dzien tinyint,
                baza varchar,
                oryginal varchar, 
                tag varchar, 
                rozklad varchar
    )")
  }

  
   # szukamy po id, ktorych rekordow nie ma w tabeli
  # id.stare <- dbGetQuery(db, "select id from rozklady")$id
  # id.nowe  <- subset(ramka$id, !ramka$id %in% id.stare)
  # id.nowe  <- which(ramka$id == id.nowe)
  
  
  
   # jesli sa nowe posty dodaj je do tabeli
  # if(length(id.nowe) > 0){
    
    dbSendPreparedQuery(db, "insert into rozklady values (?,?,?,?,?,?,?,?)",
                        ramka)
    
  # }
  #    else { cat("\nNie ma nowych danych w tabeli rozklady!\n")}
  
  
  
  suppressWarnings(dbDisconnect(db))
  
  
  invisible()
  
  
}

