


policz.rozklad <- function(dane, proby = 4, przerwa = 10){
  
  
  
  
  klaster <- makeCluster(5 * detectCores())
  
  
  for (j in 1:proby) {
    tryCatch({
      
      
      parSapplyLB(klaster, dane$body, function(napis){
        
        require(httr)
        
        set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
        
        URL <- "https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/nlp/single"
        
        nlp <- POST(URL,
                    body = list(message=list(body=napis), token="2$zgITnb02!lV"),
                    add_headers("Content-Type" = "application/json"), encode = "json")
        
        content(nlp, "parsed")
        
      }) -> rozklad
      

      
      break
    }, error = function(err) {
      cat(paste0("\n", j,". proba rozkladu nie powiodla sie\n"))
      if (j == proby) {
        
        stopCluster(klaster)
        
        stop(paste("Liczenie rozkladu nie powiodlo sie!"))
      }
      Sys.sleep(przerwa)
    })
    
  }
  
  suppressWarnings(stopCluster(klaster))
  
  
  rozklad

  
}



