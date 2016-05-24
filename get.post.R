require(Rfacebook)
require(parallel)
require(httr)

#' wydobywa posty wraz z komentarzami z tego co wypluje funkcja getPage() z pakietu
#' Rfacebook 
#' 
#' posts_id = getPage(...)$id
#' 
#' il.watkow - argument funkcji makeCluster()
#' 
#' zwraca liste list ramek

get.post <- function(posts_id, token, il.watkow){
  
  
  
  klaster <- makeCluster(il.watkow)
  
  clusterExport(klaster, c("posts_id", "token"), envir = environment())
  
  
  
  parLapply(klaster, posts_id, function(post){ 
    
      require(Rfacebook)
      require(httr)
      set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
      getPost(post, token, n = 1000)
    
  }) -> raw_data
  
  stopCluster(klaster)
  
  
  
  raw_data
}