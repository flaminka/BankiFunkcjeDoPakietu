require(RSQLite)

#' sprawdza czy trzeba aktualizowac tabele dane lub tabele rozklady
#' oraz jakich id zawartych w nowych danych nie ma w bazie (osobno dla
#' obu tabel)

#' baza - sciezka do bazy lub gdzie ma zostac utworzona nowa
#' dane - dane pobrane z fejsa funkcja read.page(), ramka danych

#' zwraca liste list

czy.aktualizowac <- function(dane, baza){
  
  
  
  czy.akt.dane     <- TRUE
  czy.akt.rozklady <- TRUE
  
  id.nowe.dane     <- id.nowe.rozklady <- dane$id

  
  
  
  db <- dbConnect(SQLite(), dbname = baza)
  
  
  
  # jesli aktualizowac tabele dane, to ktore wiersze
  
  if ("dane" %in% dbListTables(db)){
    
    id.stare <- dbGetQuery(db, "select id from dane")$id
    id.nowe.dane  <- setdiff(dane$id, id.stare)
    id.nowe.dane  <- which(dane$id %in% id.nowe.dane)
    
    czy.akt.dane <- ifelse(length(id.nowe.dane) > 0, TRUE, FALSE)
    
  } else {id.nowe.dane <- 1:nrow(dane)}
  
  
  
  
  
  # jesli aktualizowac tabele rozklady, to ktore wiersze
  
  if ("rozklady" %in% dbListTables(db)){
    
    id.stare <- dbGetQuery(db, "select id from rozklady")$id
    id.nowe.rozklady <- setdiff(dane$id, id.stare)
    id.nowe.rozklady  <-  which(dane$id %in% id.nowe.rozklady)
    
    czy.akt.rozklady <- ifelse(length(id.nowe.rozklady) > 0, TRUE, FALSE)
    
  } else {id.nowe.rozklady <- 1:nrow(dane) }
  
  
  suppressWarnings(dbDisconnect(db))
  
  
  # zwraca:
  # co.akt$dane - wiersze z dostarczonych dane, ktorych nie ma w tabeli dane (po ID)
  # co.akt$rozklady - jak wyzej dla tabeli dane
  # czy.akt$dane - TRUE jesli w dostarczonych danych sa ID ktorych nie ma w tabeli dane
  # czy.akt$rozklady - jak wyzej dla tabeli rozklady
  
  

  
  
  list(
         co.akt  = list(dane=id.nowe.dane, rozklady=id.nowe.rozklady),
         czy.akt = list(dane=czy.akt.dane, rozklady=czy.akt.rozklady)
       )
  
  
  
  
}



