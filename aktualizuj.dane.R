library(RSQLite)




#' aktualizuje tabele dane
#' baza - sciezka do bazy lub gdzie ma zostac utworzona nowa
#' dane - dane pobrane z fejsa funkcja read.page()




aktualizuj.dane <- function(baza, dane){
  
  db <- dbConnect(SQLite(), dbname = baza)
  
  
  if (!"dane" %in% dbListTables(db)) {
    
    dbSendQuery(db, "CREATE TABLE dane (
                                          id nvarchar NOT NULL,
                                          from_id nvarchar,
                                          from_name nvarchar,
                                          body ntext,
                                          like_count tinyint,
                                          comments_count tinyint,
                                          shares_count tinyint,
                                          date date,
                                          time time,
                                          weekday tinyint,
                                          parent_id nvarchar,
                                          page_name nvarchar

                                      )")
  }
  
  
  
  
    
    
    dbSendPreparedQuery(db, "INSERT INTO dane VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
                        dane)
    

  
  
  
  suppressWarnings(dbDisconnect(db))
  invisible()
  
  
}



