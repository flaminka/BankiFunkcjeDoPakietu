require(stringi)

#' glowny skrypt, odpalenie aktualizuje baze sciagajac posty z facebooka od daty
#' ktora jako ostatnia widnieje w bazie dla danego banku

#' page_name - nazwa strony z fejsa
#' how_many - ile postow pobrac
#' baza - sciezka do bazy
#' il.watkow - argument makeCluster()
#' il.prob - jesli nie uda sie pobrac danych z fejsa, poprawic ortografii lub
#'        policzyc rozladu probuje domyslnie jeszcze 59 razy. Duzo, bo zdarza 
#'        sie ze tyle potrzeba
#' przerwa - ile czasu czeka pomiedzy probami
#' 
#' 
#' OGOLNIE SKRYPT DZIALA DOBRZE NA WINDOWSIE, NA LINUXIE NIE WIADOMO
#' 
#' 
#' skrypt wymaga, zeby w pakiecie byl zalaczony plik fb_oauth !!!



aktualizuj.baze <- function(page_name, how_many = 100, baza, il.watkow = 20, il.prob = 60, przerwa = 2){
  
  #                DANE W POSTACI TAKIEJ, JAK PRZYKLADOWA

  
  
  # stopifnot( is.character(baza), 
  #            length(baza) == 1,
  #            is.data.frame(dane),
  #            nrow(dane)  > 0, 
  #            ncol(dane) == 8, 
  #            colnames(dane) == c("id", "data", "godzina", 
  #                                "dzien","baza", "oryginal", "tag", "rozklad"))
  # 
  
  
  
  since <- policz.since(page_name = page_name, baza = baza)
  
  
   load("C:/Users/Michał/Desktop/fb_oauth")
  
  
  
  # czasami proba pobrania danych z fejsa przy duzej ilosci ustawionych postow sie
  # nie udaje od razu, wiec probujemy wiecej razy
  
  for (j in 1:il.prob) {
    tryCatch({
      
  dane <- read.page(page_name = page_name, how_many = how_many, token = fb_oauth, since = since, il.watkow = il.watkow)
  
  break
  
    }, error=function(err){
    
      cat(paste0("\n", j,". Próba pobrania danych z Facebooka nie powiodla sie. 
                 Próbuję ponownie.\n"))
      
      
      if(j == il.prob)
        stop(paste("Nie udalo sie pobrac danych!"))
      
      
    Sys.sleep(przerwa)
    
      })}
  

  
  
  
  # usuwanie pustych postow
  puste <- which(sapply(dane$body, function(x) is.na(x) | x == ""))

  
  if(length(puste) > 0)
    dane  <- dane[-puste, ] 
  
  
  
  
  
  #---------------SPRAWDZENIE CZY TRZEBA AKTUALIZOWAC BAZE---------------------
  
  
  aktualizacja     <- czy.aktualizowac(dane, "baza.db")
  
  czy.akt.dane     <- aktualizacja$czy.akt$dane       # czy trzeba aktualizowac tabele dane
  czy.akt.rozklady <- aktualizacja$czy.akt$rozklady   # czy trzeba aktualizowac tabele rozklady
  id.nowe.dane     <- aktualizacja$co.akt$dane        # ktorych wierszy z danych nie ma w tabeli dane
  id.nowe.rozklady <- aktualizacja$co.akt$rozklady    # ktorych wierszy z danych nie ma w tabeli rozklady
  
  wiersze   <- unique(c(id.nowe.dane, id.nowe.rozklady))
  dane      <- dane[wiersze, ]
  
  
      aktualizacja <- czy.aktualizowac(dane, "baza.db")
  id.nowe.dane     <- aktualizacja$co.akt$dane       
  id.nowe.rozklady <- aktualizacja$co.akt$rozklady
  
  
  
  if(!czy.akt.dane & czy.akt.rozklady) 
    cat("\nTabela dane jest aktualna! Aktualizuje tabele rozklady.")
  
  
  if(!czy.akt.rozklady & czy.akt.dane) 
    cat("\nTabela rozklady jest aktualna! Aktualizuje tabele dane.")
  
  
  
  
  
  if(!czy.akt.rozklady & !czy.akt.dane){
    
     return("Nie ma co aktualizowac! Obie tabele aktualne!")
    
  }
  

  #----------------------------------------------------------------------------


  # na windowsie trzeba czasem poprawic kodowanie, NIE WIEM CZY TO DZIALA
  
  if (Sys.info()[['sysname']] == "Windows" &
       stri_enc_get() == "windows-1250" &
      all(!stri_enc_detect(dane$body[1])[[1]]$Encoding %in% c("ISO-8859-1", "UTF-8"))){

        suppressWarnings(dane$body <- stri_encode(as.character(dane$body),  
                                                  from = "utf-8"))
      
  }

  
  suppressWarnings(dane$from_name <- stri_encode(as.character(dane$from_name), from = "utf-8"))


  
  
  #--------------------------POPRAWA ORTOFRAFII--------------------------------
  
  
  
  if(czy.akt.dane & length(id.nowe.dane) > 0 | 
     czy.akt.rozklady & length(id.nowe.rozklady) > 0){
    
    
    
        cat("\n", "Rozpoczynam poprawe ortografii")
    
          
        # do poprawy ortografii ida tylko te wiersze, ktorych nie bylo
        # w tabeli dane lub w tabeli rozklady
    
         
          dane$body <- popraw.ortografie(dane, il.watkow, il.prob, przerwa)
        
      
        
        cat("\n", "Koniec poprawy ortografii")
        
        
  } 
  

  
  
 
  #------------------------------ROZKLAD----------------------------------------
  
  
  
  
  if(czy.akt.rozklady & length(id.nowe.rozklady) > 0){
    
    
    
        cat("\n", "Rozpoczynam liczenie rozkladu")
        
        
          rozklad <- policz.rozklad(dane[id.nowe.rozklady, ], il.watkow, il.prob, przerwa)
        
        
        cat("\n", "Rozklad policzony")
        
        
        
        
        #------------------TWORZENIE RAMKI DO DRUGIEJ TABELI-------------------
        
        
        cat("\n", "Tworzenie ramki do tabeli rozklady")
        
        
          ramka <- ramka(rozklad, dane[id.nowe.rozklady, ])
          
        
        cat("\n", "Ramka utworzona")
        
        
        
        # usuwam rozklad, bo moze zajmowac duzo miejsca w pamieci
        rm(rozklad)
        


  } 
  
  
  

  
  
  #--------------------------AKTUALIZACJA BAZY---------------------------------
  
  
  
  #-------------------------PIERWSZA TABELA BAZY-------------------------------
  
  
      if(czy.akt.dane & length(id.nowe.dane) > 0){
        
            cat("\n", "Rozpoczynam aktualizacje tabeli dane")
            
            
            aktualizuj.dane(baza, dane[id.nowe.dane, ])
            
            
            cat("\n", "Tabela dane zaktualizowana")
            
      }
        
  
  
  
  
  #---------------------------DRUGA TABELA BAZY--------------------------------

  
  if(czy.akt.rozklady & length(ramka) > 0){
  
          cat("\n", "Rozpoczynam aktualizacje tabeli rozklady")
          
          
            aktualizuj.rozklady(baza, ramka)
            
          
          rm(ramka)
          
          
          cat("\n", "Tabela rozklady zaktualizowana")
  
  } else {cat("\n", "Tabela rozklady aktualna! Patrz ramka!")}
  
  

}



