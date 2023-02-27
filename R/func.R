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


#' @title repeatability for each group
#' @description Will take the posterior distributions from a multi-response model and calculate the posterior distribution of repeatbility
#' @param G These are the SD estimates for one group at the between fish ID
#' @param cage These are the SD estimates for between cage
#' @param sigma These are the SD estimates for within individual / residual 
#' @param trait A character string with the name of the trait in the dataset
#' 
#'
rep_each <- function(ID, cage, sigma, trait){
  
  one_gp <- (ID[,grepl(trait, colnames(ID))]^2)
  total <-  one_gp + cage[, grepl(trait, colnames(cage))]^2 + sigma[,grepl(trait, colnames(sigma))]^2
  
  R_ea <- one_gp / total
  
  return(R_ea)
}



 #' @title pMCMC Function
 #' @param x The vector for the posterior distribution. Note that this will test the null hypothesis that the parameter of interest is significantly different from 0. 
 #' @param null A numeric value decsribing what the null hypothesis should be
 #' @param twotail Whether to conduct a one-tailed hypothesis or a two-tailed hypotheses. Default = true indicating a two-tailed test will be done.
pmcmc <- function(x, null = 0, twotail = TRUE){
  if(twotail){
    2*(1 - max(table(x<=null) / length(x)))
  } else{
    (1 - max(table(x<=null) / length(x)))
  }
}

