#' @title repeatability
#' @description Will take the posterior distributions from a multi-response model and calculate the posterior distribution of repeatability
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


#' @title overall_repeatability
#' @description Will take the posterior distributions from a multi-response model and calculate the posterior distribution of repeatability
#' @param C_t These are the SD estimates for control group at the between fish ID
#' @param E_t These are the SD estimates for experimental group at the between fish ID
#' @param cage These are the SD estimates for between cage
#' @param logsigmaC These are the SD estimates for within individual / residual for group C
#' @param logsigmaE These are the SD estimates for within individual / residual for group E
#' @param trait A character string with the name of the trait in the data set
#' 
overall_repeatability <- function(C_t, E_t, cage, logsigmaC, logsigmaE, trait) {
    C_t  <- data.frame(C_t)
    E_t  <- data.frame(E_t)
    cage  <- data.frame(cage)
    logsigmaC <- data.frame(logsigmaC)
    logsigmaE <- data.frame(logsigmaE)

    # Extract 8000 from posterior of ID for C adn E
    Ct_v <- C_t[, grep(trait, colnames(C_t))][sample(1:(dim(C_t)[1]), size = (dim(C_t)[1])/ 2)]
    Et_v <- E_t[, grep(trait, colnames(E_t))][sample(1:dim(E_t)[1], size = dim(E_t)[1]/ 2)]
    
    # Extract 8000 from posterior for sigma C and E
   sigmaC <- (logsigmaC[,grep(trait, colnames(logsigmaC))])[sample(1:dim(logsigmaC)[1], size = dim(logsigmaC)[1]/ 2)]
   sigmaE <- (logsigmaE[,grep(trait, colnames(logsigmaE))])[sample(1:dim(logsigmaE)[1], size = dim(logsigmaE)[1]/ 2)]

    # Get cage variance
    cage <- cage[,grep(trait, colnames(cage))]
   
    # Pool C and E posteriors for e and ID
      sigma_e_p <- c(sigmaC, sigmaE)
    sigma_ID_p  <- c(Ct_v, Et_v)

    # Calculate R
    R  <- sigma_ID_p^2 / (sigma_ID_p^2 + cage^2 + sigma_e_p^2)

    return(R)
}



#' @title creation of a dataframe
#' @description Will create a dataframe using the posterior distribution from a model
#' @param blup posterior distribution of the deviation for each ID
#' @param b posterior distribution at the population level


df <- function(blup, b){
  
  fishid_slope <- apply(blup, 2, function(x) rowSums(b + x))
  fishid_slope_est <- apply(fishid_slope,2, function(x) mean(x))
  fishid_slope_se <- apply(fishid_slope,2, function(x) sd(x))
  fishid_slope <- cbind(fishid_slope_est,fishid_slope_se)
  
  return(fishid_slope)                                              
  }

#' @title Create dataframe of predictions from a model
#' @description Will create a dataframe using the posterior distribution from a model
#' @param blup posterior distribution of the deviation for each ID
#' @param b posterior distribution at the population level

predict <- function(post, trait, dat){
  
  #Grab posterior distribution for the trait
  post <- post[,grep(trait, colnames(post))]
  
  pred_ces <- apply(t(post), 2, function(x) as.matrix(dat) %*% x)

  sum_pred <- apply(pred_ces, 1, function(x) mean(x))
  sum_predL95 <- apply(pred_ces, 1, function(x) quantile(x, 0.025))
  sum_predU95 <- apply(pred_ces, 1, function(x) quantile(x, 0.975))

  trait_dat <- cbind(dat,sum_pred, sum_predL95, sum_predU95)
  
return(trait_dat)                                              
}
