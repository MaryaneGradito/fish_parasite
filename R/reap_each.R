#' @title repeatability for each group
#' @description Will take the posterior distributions from a multi-response model and calculate the posterior distribution of repeatbility
#' @param G These are the SD estimates for one group at the between fish ID
#' @param cage These are the SD estimates for between cage
#' @param sigma These are the SD estimates for within individual / residual 
#' @param trait A character string with the name of the trait in the dataset
#' 
#'
rep_each <- function(G, cage, sigma, trait){
  
  one_gp <- (G[,grepl(trait, colnames(G))]^2)
  total <-  one_gp + cage[, grepl(trait, colnames(cage))]^2 + sigma[,grepl(trait, colnames(sigma))]^2
  
  R_ea <- one_gp / total
  
  return(R_ea)
  }
  


