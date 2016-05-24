require(RSQLite)


#' aktualizuje tabele rozklady w bazie baza

#' ramka - data.frame zrobiony z rozkladow funkcja ramka()
#' baza - sciezka do bazy lub gdzie ma zostac utworzona nowa



aktualizuj.rozklady <- function(baza, ramka){

  
  db <- dbConnect(SQLite(), dbname = baza)


  if (!"rozklady" %in% dbListTables(db)) {
    
    dbSendQuery(db, "create table rozklady (
                id smallint,
                date date,
                time nvarchar, 
                weekday tinyint,
                baza nvarchar,
                oryginal nvarchar, 
                tag nvarchar, 
                rozklad nvarchar,
                page_name nvarchar
    )")
  }


  
    
    dbSendPreparedQuery(db, "insert into rozklady values (?,?,?,?,?,?,?,?,?)",
                        ramka)
    

  
  
  
  suppressWarnings(dbDisconnect(db))
  
  
  invisible()
  
  
}

