#' Reconstruct the matching based on the estimated parameters
#' 
#' Calculate the hypothetical (our counterfactual) joint density of the observed features of the pairs in the new data
#' using previously estimated parameters. 
#' 
#' @param ff formula; an \code{\link{formula}} object, of the form \code{
#' ~ <model terms>}. For the details on the possible \code{<model terms>}, see
#' \code{\link{rpm-terms}}.
#' @param theta The estimated parameters.
#' @param mu_new The hypothetical observed matching matrix, where 1 represent a pairing, 0 otherwise. 
#' Each row is a woman, each column is a man. The order of the rows (columns)
#' needs to be the same as in \code{X} (\code{Z}). 
#' @param X_new Hypothetical feature matrix for women. Each row is a woman, each column is a feature. 
#' The number of column is assumed to be the same as in \code{Z_new}. 
#' @param Z_new Hypothetical feature matrix for men. Each row is a man, each column is a feature. 
#' The number of column is assumed to be the same as in \code{X_new}.
#' @param X_w_new The hypothetical sampling weight of each woman in the sample. The length of this vector is 
#' the same as the number of row of \code{X_new}.
#' @param Z_w_new The hypothetical sampling weight of each woman in the sample. The length of this vector is
#' the same as the number of row of \code{Z_new}.
#' @param pair_w_new The hypothetical sampling weight of each pair in the sample. The length of this vector is
#' the same as the number of pairs in \code{mu_new}.
#' @param control A list of control parameters for algorithm tuning.
#' @return This function returns a list consisting of the following elements: 
#' \item{pmfj_est}{The reconstructed joint density matrix based on the estimated parameters.}
#' \item{pmfj_obs}{The observed joint density matrix based on the new data \code{X} and \code{Z}.}
#' @seealso fitrpm_R_CP
#' @references Menzel, Konrad (2015).
#' \emph{Large Matching Markets as Two-Sided Demand Systems}
#' Econometrica, Vol. 83, No. 3 (May, 2015), 897-941.
#' @keywords models
#' @examples
#' 
create_counterfactual_distri = function(ff, theta, mu_new, X_new, Z_new, X_w_new, Z_w_new, pair_w_new, control){
  
  symmetric = control[["symmetric"]]
  sampling = control[["sampling_protocol"]]
  
  n=nrow(X_new)+nrow(Z_new)
  Xdata <- cbind(1, X_new)
  Zdata <- cbind(1, Z_new)
  colnames(Xdata)[1] <- "Int"
  colnames(Zdata)[1] <- "Int"
  
  model.terms <- rownames(attr(terms.formula(ff), "factors"))
  temp <- strsplit(model.terms, "[(]")
  model.terms.names <- unlist(lapply(temp, `[[`, 1))
  temp2 <- gsub(")", "", gsub("\"", "", unlist(temp)))
  temp2 <- gsub("[ \t\n\r\f\v]", "", temp2)
  model.terms.coef.names <- temp2[-which(temp2 %in% model.terms.names)]
  model.terms.coef.names <- strsplit(model.terms.coef.names, ",")
  
  # get the subset of the variables that are relevant according to the formula
  # Define the variables used in the model (and hence the unique classes of partners)
  # This is typically a subset of the available variables
  # model_vars <- c("Int", unlist(unique(lapply(model.terms.coef.names, '[[', 1))))
  model_vars <- c("Int", unique(unlist(model.terms.coef.names)))
  model_vars = model_vars[model_vars %in% colnames(Xdata)]
 
  Xu <- unique(Xdata[,model_vars])
  Xu <- Xu[do.call(order, as.data.frame(Xu)),]
  Zu <- unique(Zdata[,model_vars])
  Zu <- Zu[do.call(order, as.data.frame(Zu)),]
  
  num_Xu = nrow(Xu)
  num_Zu = nrow(Zu)
  
  # assume same # of explanatory variables for both men's and women's side
  NumGammaW <- num_Xu
  NumGammaM <- num_Zu
  NumGamma <- NumGammaW + NumGammaM
  NumBeta = length(theta) - NumGamma
  beta <- theta[1:NumBeta]
  GammaW <- theta[(NumBeta+1):(NumBeta+NumGammaW)]
  GammaM <- theta[(NumBeta+NumGammaW+1):length(theta)]
  
  # get the proportion of men and women
  gw = log(nrow(Xdata)/n*2) # to ensure exp(gw)+exp(gm) = 2
  gm = log(nrow(Zdata)/n*2)
  
  Xtype <- rep(NA,nrow(Xdata))
  for(i in 1:nrow(Xu)){
    Xtype[apply(Xdata[,model_vars], 1, function(x) identical(x, Xu[i,]))] <- i
  }
  # Ztype: group membership for men (one for each man in the pop)
  Ztype <- rep(NA,nrow(Zdata))
  for(i in 1:nrow(Zu)){
    Ztype[apply(Zdata[,model_vars], 1, function(x) identical(x, Zu[i,]))] <- i
  }
  
  Xtype_paired = Xtype[unlist(apply(mu_new, 2, function(x) which(x>0)))] 
  Ztype_paired = Ztype[unlist(apply(mu_new, 1, function(x) which(x>0)))]
  Xtype_paired = factor(Xtype_paired, 1:num_Xu) # account for missing types
  Ztype_paired = factor(Ztype_paired, 1:num_Zu) # account for missing types
  
  Xtype_single = wtd.table(factor(Xtype[!rowSums(mu_new)], 1:num_Xu), weights = X_w_new[-(1:length(Xtype_paired))]) # account for missing types
  Ztype_single = wtd.table(factor(Ztype[!colSums(mu_new)], 1:num_Zu), weights = Z_w_new[-(1:length(Ztype_paired))]) # account for missing types
  
  pmfW = wtd.table(Xtype, weights=X_w_new)
  pmfW = pmfW/sum(pmfW)
  pmfM = wtd.table(Ztype, weights=Z_w_new)
  pmfM = pmfM/sum(pmfM)
  
  num_Xu = nrow(Xu)
  num_Zu = nrow(Zu)
  
  if (sampling == "COUPLE") { 
    
    pmfj = matrix(0,nrow=num_Xu, ncol=num_Zu) # women (X) indexed by row, men (Z) indexed by column
    pmfj = unclass(wtd.table(Xtype_paired, Ztype_paired, weights = pair_w_new))
    pmfj = pmfj/sum(pmfj)
    
  } else if (sampling == "HOUSEHOLD") {
    
    pmfj = matrix(0,nrow=1+num_Xu, ncol=1+num_Zu) # women (X) indexed by row, men (Z) indexed by column
    pmfj[1:num_Xu,1:num_Zu] = unclass(wtd.table(Xtype_paired, Ztype_paired, weights = pair_w_new))
    
    if (length(Xtype_single) > 0) {
      pmfj[1:num_Xu,1+num_Zu] = Xtype_single
    }
    if (length(Ztype_single) > 0) {
      pmfj[1+num_Xu,1:num_Zu] = Ztype_single
    }
    
    pmfj = pmfj/sum(pmfj)
    
  } else { # assume "INDIV"
    
    pmfj = matrix(0,nrow=1+num_Xu, ncol=1+num_Zu) # women (X) indexed by row, men (Z) indexed by column
    pmfj[1:num_Xu,1:num_Zu] = unclass(wtd.table(Xtype_paired, Ztype_paired, weights = pair_w_new)) *2
   
    if (length(Xtype_single) > 0) {
      pmfj[1:num_Xu,1+num_Zu] = Xtype_single
    }
    if (length(Ztype_single) > 0) {
      pmfj[1+num_Xu,1:num_Zu] = Ztype_single
    }
    
    pmfj = pmfj / sum(pmfj)
    
  }
  

  loglikfun_fixed_pref <- function(theta, beta, Xd, Zd, NumGammaW, pmfW, pmfM, pmfj, gw, gm, n, symmetric, sampling){
    NumBeta <- 0
    GammaW <- theta[(NumBeta+1):(NumBeta+NumGammaW)]
    GammaM <- theta[(NumBeta+NumGammaW+1):length(theta)]
    -loglikelihood_CP(beta, GammaW, GammaM, Xd, Zd, pmfW, pmfM, pmfj, gw, gm, n, symmetric, sampling)
  }
  eqfun_fixed_pref <- function(theta, beta, Xd, Zd, NumGammaW, pmfW, pmfM, pmfj, gw, gm, n, symmetric, sampling){
    NumBeta <- 0
    GammaW <- theta[(NumBeta+1):(NumBeta+NumGammaW)]
    GammaM <- theta[(NumBeta+NumGammaW+1):length(theta)]
    equality_constraint_CP(beta, GammaW, GammaM, Xd, Zd, pmfW, pmfM, gw, gm, n, symmetric, sampling)
  }
  gloglikfun_fixed_pref <- function(theta, beta, Xd, Zd, NumGammaW, pmfW, pmfM, pmfj, gw, gm, n, symmetric, sampling){
    nl.grad(theta, loglikfun_fixed_pref,beta=beta,Xd=Xd,Zd=Zd,NumGammaW=NumGammaW,
            pmfW=pmfW,pmfM=pmfM,pmfj=pmfj,gw=gw, gm=gm, n=n, symmetric=symmetric, sampling=sampling)
  }
  jeqfun_fixed_pref <- function(theta, beta, Xd, Zd, NumGammaW, pmfW, pmfM, pmfj, gw, gm, n, symmetric, sampling){
    nl.jacobian(theta, eqfun_fixed_pref, beta=beta,Xd=Xd,Zd=Zd,NumGammaW=NumGammaW,
                pmfW=pmfW,pmfM=pmfM, pmfj=pmfj,gw=gw,gm=gm,n=n,symmetric=symmetric, sampling=sampling)
  }
  
  
  modelmat <- rpm.model.matrix(model.terms.names, model.terms.coef.names, Xu, Zu)
  
  X <- modelmat$X
  Z <- modelmat$Z
  
  # we have to re-estimate Gammas with our prefrence parameters (i.e. beta) because the marginal distributions has changed
  # at least for now, I'm only handling control[["algorithm"]]!="solnp"
  out <- nloptr(x0=theta[-c(1:NumBeta)], eval_f=loglikfun_fixed_pref, eval_grad_f=gloglikfun_fixed_pref,
                eval_g_eq=eqfun_fixed_pref, eval_jac_g_eq=jeqfun_fixed_pref,
                lb=rep(0,NumGamma), # ub=rep(Inf,NumGamma),
                beta=theta[1:NumBeta], Xd=X, Zd=Z, NumGammaW=NumGammaW,
                pmfW=pmfW, pmfM=pmfM, pmfj=pmfj, gw=gw, gm=gm, n=n, symmetric=symmetric, sampling=sampling,
                opts=control)
  
  theta_new = c(theta[1:NumBeta], out$solution)
  names(theta_new) <- names(theta)

  distr = check_CP_latent(ff, theta_new, mu_new, X_new, Z_new, X_w_new, Z_w_new, pair_w_new, sampling, symmetric)  
  
  return(list(pmfj_cf=distr$pmfj_est, pmfj_obs=distr$pmfj_obs))
  
}

