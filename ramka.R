require(pbapply)
require(stringi)


# rozklad - policzony przez serwer, jak na zajeciach
# dane - jak u Bicka

ramka <- function(rozklad, dane){
  

  
  

  pblapply(1:length(rozklad), function(lista){
    
    sapply(rozklad[[lista]], function(slowo){
      
      
      # tworzy trzy kolumny: date z dokladnoscia do dnia. sekundy, dnia tygodnia
      data <- strftime(dane$created_at[lista], c("%Y-%m-%d", "%H:%M:%S", "%w"))
      
      c(as.character(dane$id[lista])  , data, 
        ifelse(is.null(slowo$base), "", slowo$base),
        ifelse(is.null(slowo$orth), "", slowo$orth), 
        ifelse(is.null(slowo$cTag), "", slowo$cTag),
        ifelse(is.null(slowo$msd) , "", slowo$msd))
      
      
    })
    
  }) -> ramka

  


  # zrobienie z 'ramki' data.frame'u
  ramka <- pblapply(ramka, function(macierz) t(as.data.frame(macierz)))
  
  
  #jesli sa jakies puste wiersze to usun
  puste <- which(sapply(ramka, function(macierz){ncol(macierz)!=8}))
  
  
  if(length(puste) > 0)
    ramka <- ramka[-puste]
  
  
  ramka <- do.call(rbind, ramka)
  
  ramka <- as.data.frame(ramka)
  
  colnames(ramka) <- c("id", "data", "godzina", "dzien","baza", "oryginal", "tag", "rozklad")
  
  ramka$id    <- as.numeric(as.character(ramka$id))
  
  ramka$dzien <- as.numeric(as.character(ramka$dzien))
  
  
  # potrzebne na windowsie:
  
  if (Sys.info()[['sysname']] == "Windows" & stri_enc_get() == "windows-1250"){
    
          ramka$baza <- stri_encode(ramka$baza,     from = "utf-8")
      ramka$oryginal <- stri_encode(ramka$oryginal, from = "utf-8")
  }
  
  

  ramka
  
}



