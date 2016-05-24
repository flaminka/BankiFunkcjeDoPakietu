require(parallel)
require(httr)


#' Poprawia ortografie, proby i przerwa to zmienne ktore definiuja ilosc prob
#' polaczenia z serwerem i dlugosc przerwy pomiedzy kolejnymi probami (w sek)
#' 
#' dane - pobrane z fejsa funkcja read.page(), ramka danych
#' il.watkow - argument funkcji makeCluster()
#' il.prob - w przypadku niepowodzenia probuje jeszcze
#' przerwa - przerwa pomiedzy probami
#' 
#' zwraca rozklad - liste list



policz.rozklad <- function(dane, il.watkow, il.prob, przerwa){
  
  
  
  
  klaster <- makeCluster(il.watkow)
  
  
  for (j in 1:il.prob) {
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
      cat(paste0("\n", j,". Proba rozkladu nie powiodla sie. Próbuję ponownie.\n"))
      if (j == il.prob) {
        
        stopCluster(klaster)
        
        stop(paste("Liczenie rozkladu nie powiodlo sie!"))
      }
      Sys.sleep(przerwa)
    })
    
  }
  
  suppressWarnings(stopCluster(klaster))
  
  
  
  usun <- which(sapply(1:length(rozklad), function(i) 
    
                         all(class(rozklad[[i]]) == "list")
                    
                      ) != TRUE)
                
    
  
  
  
  if (length(usun) > 0) {rozklad[-usun]} else {rozklad}
  

  
}



