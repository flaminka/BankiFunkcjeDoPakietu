




aktualizuj.baze <- function(dane, baza){
  
  #                DANE W POSTACI TAKIEJ, JAK PRZYKLADOWA

  
  # stopifnot( is.character(baza), 
  #            length(baza) == 1,
  #            is.data.frame(dane),
  #            nrow(dane)  > 0, 
  #            ncol(dane) == 8, 
  #            colnames(dane) == c("id", "data", "godzina", 
  #                                "dzien","baza", "oryginal", "tag", "rozklad"))
  # 
  
  
  
  
  

  # usuwanie pustych postow
  puste <- which(sapply(dane$body, function(x) x == ""))
  
  
  if(length(puste) > 0)
    dane  <- dane[-puste, ] 
  
  
  
  
  
  #---------------SPRAWDZENIE CZY TRZEBA AKTUALIZOWAC BAZE---------------------
  
  aktualizacja     <- czy.aktualizowac(dane, "baza.db")
  
  czy.akt.dane     <- aktualizacja$czy.akt$dane
  czy.akt.rozklady <- aktualizacja$czy.akt$rozklady
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


  
  # na windowsie trzeba poprawic kodowanie
  if (Sys.info()[['sysname']] == "Windows"){
           dane$body <- stri_encode(dane$body,      from = "utf-8")
      dane$user_name <- stri_encode(dane$user_name, from = "utf-8")
  }
  
  
  
  

  
  
  #--------------------------POPRAWA ORTOFRAFII--------------------------------
  
  
  
  if(czy.akt.dane & length(id.nowe.dane) > 0 | 
     czy.akt.rozklady & length(id.nowe.rozklady) > 0){
    
    
    
        cat("\n", "Rozpoczynam poprawe ortografii")
    
          
          wiersze   <- unique(c(id.nowe.dane, id.nowe.rozklady))
          dane$body <- popraw.ortografie(dane[wiersze, ])
        
      
        
        cat("\n", "Koniec poprawy ortografii")
        
        
  } 
  

  
  
 
  #------------------------------ROZKLAD----------------------------------------
  
  
  
  
  if(czy.akt.rozklady & length(id.nowe.rozklady) > 0){
    
    
    
        cat("\n", "Rozpoczynam liczenie rozkladu")
        
        
          rozklad <- policz.rozklad(dane[id.nowe.rozklady, ])
        
        
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
  
  
      if(czy.akt.dane){
        
            cat("\n", "Rozpoczynam aktualizacje tabeli dane")
            
            
            aktualizuj.dane(baza, dane[id.nowe.dane, ])
            
            
            cat("\n", "Tabela dane zaktualizowana")
            
      }
        
  
  
  
  
  #---------------------------DRUGA TABELA BAZY--------------------------------

  
  if(czy.akt.rozklady){
  
          cat("\n", "Rozpoczynam aktualizacje tabeli rozklady")
          
          
            aktualizuj.rozklady(baza, ramka)
            
          
          rm(ramka)
          
          
          cat("\n", "Tabela rozklady zaktualizowana")
  
  }
  
  
  #---------------------------KONIEC (DZWIEK)--------------------------------
  beep()
  
  
}



