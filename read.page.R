require(Rfacebook)
require(pbapply)
require(httr)



#' scraping z Facebooka, wczytuje posty z komentarzami ze strony page_name
#'        i zwraca ramke danych
#' page_name - nazwa strony
#' how_many - ile postow ze strony czytac
#' token - token lub plik z pakietu (plik fb_oauth), ktory trzeba wczytac load("fb_oauth")
#' 



read.page <- function(page_name, how_many, token, since, il.watkow){
    
    
   
  
    set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    
    page     <- getPage(page_name, token, n = how_many, feed = TRUE, since = since)
    posts_id <- page$id
    
    
    # wyciagamy posty z komentarzami
    raw_data <- get.post(posts_id, token, il.watkow)
    
    
    
    
    # tworzy liste data.frame'ow z danymi dla wszystkich postow
    data_inDF <- pblapply(raw_data, function(i) merge.comments(i, page_name = page_name))
    
    
    
    # laczy je w jeden data.frame:
    merged_data <- do.call(rbind, data_inDF)
    merged_data <- as.data.frame(merged_data)
    
    
    
}


