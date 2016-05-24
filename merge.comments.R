require(stringi)

#' laczy post z komentarzami w ramke
#' element - post z komentarzami lub sam post
#' page_name - nazwa strony

merge.comments <- function(element, page_name){
  
  
  # jesli nie bylo komentarzy pod postem
  if(nrow(element$comments) == 0){
    
    merged <- cbind(
                         element$post[, c(7,1,2,3,8,9,10)],
                         date = strftime(element$post$created_time, "%Y-%m-%d"),
                         time = stri_sub(element$post$created_time, from=12, to=-6),
                      weekday = strftime(element$post$created_time, "%w"),
                    parent_id = NA, 
                    page_name = page_name
                    
                    )
    
    
  }else{ # jesli byly komentarze pod postem
    
    merged <- rbind(
                    cbind(
                      
                               element$post[, c(7,1,2,3,8,9,10)],
                               date = strftime(element$post$created_time, "%Y-%m-%d"),
                               time = stri_sub(element$post$created_time, from=12, to=-6),
                            weekday = strftime(element$post$created_time, "%w"),
                          parent_id = NA, 
                          page_name = page_name
                          
                          ), 
                    
                    cbind(
                      
                          element$comments[, c(6,1,2,3, 5)], 
                          comments_count = "", 
                            shares_count = "",
                               date = strftime(element$comments$created_time, "%Y-%m-%d"),
                               time = stri_sub(element$post$created_time, from=12, to=-6),
                            weekday = strftime(element$comments$created_time, "%w"),
                          parent_id = element$post$id, 
                          page_name = page_name
                          
                          )
                    )
  }
  
  
  colnames(merged)[4]<-"body"
  
  
  merged
  
  
}


