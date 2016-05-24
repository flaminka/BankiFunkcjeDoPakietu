require(RSQLite)



#' dla danej strony sprawdza w bazie date ostatniego postu
#' 
#' page_name - nazwa strony
#' baza - sciezka do bazy lub gdzie ma powstac nowa



policz.since <- function(page_name, baza){
  
  
  
  since.dane <- NULL
  since.rozklady <- NULL
  

  
  query.dane <- paste("SELECT date FROM dane where page_name = '", 
                      page_name, "' limit 1", sep = "")
  query.rozklady <- paste("SELECT date FROM rozklady where page_name = '", 
                          page_name, "' limit 1", sep = "")

  
  
  

  
  db <- dbConnect(SQLite(), dbname = baza)
  
      
  
      if ("dane" %in% dbListTables(db)){
        
        
        if (page_name %in%   
            dbGetQuery(db, "select distinct(page_name) from dane")$page_name)
        
                 since.dane <- dbGetQuery(db, query.dane)$date
            

  
      }
  
  
    
  
  
  
  if ("rozklady" %in% dbListTables(db)){
    
    
    if (page_name %in%   
        dbGetQuery(db, "select distinct(page_name) from rozklady")$page_name)
      
      since.rozklady <- dbGetQuery(db, query.rozklady)$date
    
    
    
  }
  
  

  
  dbDisconnect(db)
  
  
  
  
  if (is.null(since.dane) | is.null(since.rozklady)) return(NULL)
  
  
  min(as.Date(since.dane), as.Date(since.rozklady))
  
  
  
  
  

}
  
  
  
  