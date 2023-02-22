#' @title repeatability
#' @description Will take the posterior distributions from a multi-response model and calculate the posterior distribution of repeatbility
#' @param C These are the SD estimates for control group at the between fish ID
#' @param E These are the SD estimates for experimental group at the between fish ID
#' @param cage These are the SD estimates for between cage
#' @param sigma These are the SD estimates for within individual / residual 
#' @param trait A character string with the name of the trait in the dataset

repeatability <- function(C, E, cage, sigma, trait){
  
  combine_trt <- rowMeans(cbind(C[,grepl(trait, colnames(C))]^2, E[,grepl(trait, colnames(E))]^2))
        total <- combine_trt + cage[, grepl(trait, colnames(cage))]^2 + sigma[,grepl(trait, colnames(sigma))]^2
  
  R <- combine_trt / total
  
  return(R)                                              
}
