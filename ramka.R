require(pbapply)
require(stringi)


#' rozklad - policzony przez serwer, jak na zajeciach, funkcja policz.rozklad()
#' dane - pobrane z facebooka funkcja read.page(), ramka
#' 
#' zwraca ramke danych

ramka <- function(rozklad, dane){
  



  pblapply(1:length(rozklad), function(lista){
    
    
    
    # policz.rozklad() czasem zwraca liste w inny sposob
    
    if(!is.null(rozklad[[lista]]$elements)){
      
    sapply(rozklad[[lista]]$elements, function(slowo){
      
      

      
      c(ifelse(is.null(as.character(dane$id[lista])), "", as.character(dane$id[lista])),
        ifelse(is.null(as.character(dane$date[lista])), "", as.character(dane$date[lista])),
        ifelse(is.null(as.character(dane$time[lista])), "", as.character(dane$time[lista])), 
        ifelse(is.null(as.character(dane$weekday[lista])), "", as.character(dane$weekday[lista])), 
        ifelse(is.null(slowo$base), "", slowo$base),
        ifelse(is.null(slowo$orth), "", slowo$orth), 
        ifelse(is.null(slowo$cTag), "", slowo$cTag),
        ifelse(is.null(slowo$msd) , "", slowo$msd),
        as.character(dane$page_name[lista]))
      
      
    })
      
    }else{
      
      sapply(rozklad[[lista]], function(slowo){
        
        
        
        
        c(ifelse(is.null(as.character(dane$id[lista])), "", as.character(dane$id[lista])),
          ifelse(is.null(as.character(dane$date[lista])), "", as.character(dane$date[lista])),
          ifelse(is.null(as.character(dane$time[lista])), "", as.character(dane$time[lista])), 
          ifelse(is.null(as.character(dane$weekday[lista])), "", as.character(dane$weekday[lista])), 
          ifelse(is.null(slowo$base), "", slowo$base),
          ifelse(is.null(slowo$orth), "", slowo$orth), 
          ifelse(is.null(slowo$cTag), "", slowo$cTag),
          ifelse(is.null(slowo$msd) , "", slowo$msd),
          as.character(dane$page_name[lista]))
      
      
      
      
      
      
    })
    
  }}) -> ramka


  # zrobienie z 'ramki' data.frame'u
  ramka <- pblapply(ramka, function(macierz) t(as.data.frame(macierz)))


  # jesli sa jakies puste wiersze to usun
  puste <- which(sapply(ramka, function(macierz){ncol(macierz)!=9}))
  
 
  if(length(puste) > 0)
    ramka <- ramka[-puste]
  
  
  if(length(ramka) == 0) return(ramka)

  
  ramka <- do.call(rbind, ramka)
  
  ramka <- as.data.frame(ramka)
  
  colnames(ramka) <- c("id", "date", "time", "weekday", "baza", "oryginal", "tag", "rozklad", "page_name")
  
  ramka$id        <- as.character(ramka$id)

  ramka$weekday   <- as.numeric(as.character(ramka$weekday))
  
  ramka$baza      <- as.character(ramka$baza)
  
  ramka$oryginal  <- as.character(ramka$oryginal)
  
  ramka$page_name <- as.character(ramka$page_name)
  
  
 
  # potrzebne na windowsie:
  
  if (Sys.info()[['sysname']] == "Windows" & stri_enc_get() == "windows-1250"){

          ramka$baza <- stri_encode(as.character(ramka$baza,     from = "utf-8"))
      ramka$oryginal <- stri_encode(as.character(ramka$oryginal, from = "utf-8"))
   }
  
  

  ramka
  
}



