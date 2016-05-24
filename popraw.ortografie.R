
library(stringi)
library(parallel)
require(httr)


#' Poprawia ortografie, il.prob i przerwa to zmienne ktore definiuja ilosc prob
#' polaczenia z serwerem i dlugosc przerwy pomiedzy kolejnymi probami (w sek)
#' 
#' dane - pobrane z facebooka funkcja read.page(), ramka
#' il.watkow - argument funkcji makeCluster()
#' 
#' zwraca wektor napisow


popraw.ortografie <- function(dane, il.watkow, il.prob, przerwa){
  
  
  

  
  klaster <- makeCluster(il.watkow)
  

  
  for (j in 1:il.prob) {
    tryCatch({
 
      
        parSapply(klaster, dane$body, function(napis) {
          
          require(httr)
          set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
          
          URL <- "https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/spell/single"
          
          
          korekta <- httr::POST(URL, 
                                body = list(message=list(body=napis), token="2$zgITnb02!lV"),
                                add_headers("Content-Type" = "application/json"), encode = "json")
          
          korekta <- content(korekta, "parsed")
          korekta$output
          
        }) -> body
      
    
    break
    }, error = function(err) {
      cat(paste0("\n", j,". Próba poprawy ortografii nie powiodla sie. 
                 Próbuję ponownie.\n"))
      if (j == il.prob) {
        stopCluster(klaster)
        stop(paste("Poprawa ortografii nie powiodla sie!"))
      }
      Sys.sleep(przerwa)
    })

    
  }

  stopCluster(klaster)
  invisible()
  
  
  

   body <- as.character(body)


  
  if (Sys.info()[['sysname']] == "Windows" & stri_enc_get() == "windows-1250"){

     suppressWarnings(body <- stri_encode(body, from = "utf-8"))
  }
 

  

  body
  
}

