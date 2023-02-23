#' @title repeatability for each group
#' @description Will take the posterior distributions from a multi-response model and calculate the posterior distribution of repeatbility
#' @param G These are the SD estimates for one group at the between fish ID
#' @param cage These are the SD estimates for between cage
#' @param sigma These are the SD estimates for within individual / residual 
#' @param trait A character string with the name of the trait in the dataset
#' 
#'
rep_each <- function(G, cage, sigma, trait){
  
  if (log = TRUE) { 
    
  combine_trt <- exp(G[,grepl(trait, colnames(G))]^2)
  total <- combine_trt + cage[, grepl(trait, colnames(cage))]^2 +    sigma[,grepl(trait, colnames(sigma))]^2
  
  R_ea <- combine_trt / total
  
  return(R_ea)
  }
  
  else { rep_each <- function(G, cage, sigma, trait)
      
      combine_trt <- (G[,grepl(trait, colnames(G))])
      total <- combine_trt + cage[, grepl(trait, colnames(cage))] +    sigma[,grepl(trait, colnames(sigma))]
      
      R_ea <- combine_trt / total
      return(R_ea)
  }
}


