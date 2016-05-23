require(RSQLite)


# ramka - data.frame zrobiony z rozkladow
# baza - sciezka do bazy


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


  
    
    dbSendPreparedQuery(db, "insert into rozklady values (?,?,?,?,?,?,?,?)",
                        ramka)
    

  
  
  
  suppressWarnings(dbDisconnect(db))
  
  
  invisible()
  
  
}

