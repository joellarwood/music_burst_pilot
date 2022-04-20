#' One Proportion Z test 
#' 
#' A function to calculate the results of a 
#' 

z_prop_test <- function(x,n,p){
  res <- prop.test(x = x,n = n,p = p, alternative = "greater")
  

  chi <- paste0("X","(", res$parameter, ") = ", round(res$statistic, 2))
  

  p <- if(res$p.value > .001){
    paste0("p = ", round(res$p.value, 3))
  } else {
    paste0("p < .001")
  }
  

  ci <- paste0("95% CI[", round(res$conf.int[1], 2), ",", round(res$conf.int[2], 2), "]")
  
  text <- paste(chi, p, ci, sep = ", ")
  
  return(text)
}


is_sig <- function(x, n,p){
  res <- prop.test(x = x,n = n,p = p, alternative = "greater")
  
  if (res$p.value < .05) return(1) else return(0)
}
  
