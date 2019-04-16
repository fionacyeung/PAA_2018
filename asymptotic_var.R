llik_paired = function(theta, NumBeta, NumGammaW, NumGammaM, j, k, pmfM, pmfW, Xd, Zd, gw, gm, n, symmetric) {
  
  beta = theta[1:NumBeta]
  GammaW = theta[NumBeta+(1:NumGammaW)]
  GammaM = theta[(NumBeta+NumGammaW)+(1:NumGammaM)]
  
  probs = logitProb_latent_opp_set(beta, GammaW, GammaM, Xd, Zd, gw, gm, n, symmetric)
  CPW = probs$CPW
  CPM = probs$CPM
  
  llik = log(CPW[j,k]*pmfM[k]*exp(gm) * CPM[k,j]*pmfW[j]*exp(gw) * n / (exp(gw)+exp(gm)))
  return(llik)
}

llik_sw = function(theta, NumBeta, NumGammaW, NumGammaM, j, pmfW, Xd, Zd, gw, gm, n, symmetric) {
  
  beta = theta[1:NumBeta]
  GammaW = theta[NumBeta+(1:NumGammaW)]
  GammaM = theta[(NumBeta+NumGammaW)+(1:NumGammaM)]
  
  probs = logitProb_latent_opp_set(beta, GammaW, GammaM, Xd, Zd, gw, gm, n, symmetric)
  CPW = probs$CPW
  CPM = probs$CPM
  
  llik = log(CPW[j,(1+NumGammaM)]*pmfW[j]*exp(gw) / (exp(gw)+exp(gm)))
  return(llik)
}

llik_sm = function(theta, NumBeta, NumGammaW, NumGammaM, k, pmfM, Xd, Zd, gw, gm, n, symmetric) {
  
  beta = theta[1:NumBeta]
  GammaW = theta[NumBeta+(1:NumGammaW)]
  GammaM = theta[(NumBeta+NumGammaW)+(1:NumGammaM)]
  
  probs = logitProb_latent_opp_set(beta, GammaW, GammaM, Xd, Zd, gw, gm, n, symmetric)
  CPW = probs$CPW
  CPM = probs$CPM
  
  llik = log(CPM[k,(1+NumGammaW)]*pmfM[k]*exp(gm) / (exp(gw)+exp(gm))) 
  return(llik)
}

# not centered
outer_prod_gradient_CP = function(theta, NumBeta, NumGammaW, NumGammaM, Xd, Zd, pmfW, pmfM, pmfj, gw, gm, n, symmetric){
  
  outer_grad = matrix(0, nrow=length(theta), ncol=length(theta))
  
  # couples
  for (j in 1:NumGammaW) {
    for (k in 1:NumGammaM) {
      
      score = nl.grad(theta, llik_paired, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, j=j, k=k, pmfM=pmfM, pmfW=pmfW, 
                      Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric)
      
      outer_grad = outer_grad + pmfj[j,k] * outer(score,score)
    }
  }
  
  # singles women (X each row)
  for (j in 1:NumGammaW) {
    
    score = nl.grad(theta, llik_sw, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, j=j, pmfW=pmfW,  
                    Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric) 
    
    outer_grad = outer_grad + pmfj[j,(1+NumGammaM)] * outer(score,score)
  }
  
  # single men (Z each col)
  for (k in 1:NumGammaM) {
    
    score = nl.grad(theta, llik_sm, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, k=k, pmfM=pmfM, 
                    Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric) 
    
    outer_grad = outer_grad + pmfj[(1+NumGammaW),k] * outer(score,score)
  }
  
  return (outer_grad)
  
}

# centered
outer_prod_gradient_CP_2 = function(theta, loglikfun, NumBeta, NumGammaW, NumGammaM, Xd, Zd, pmfW, pmfM, pmfj, gw, gm, n, symmetric, sampling){
  
  outer_grad = matrix(0, nrow=length(theta), ncol=length(theta))
  
  # couples
  for (j in 1:NumGammaW) {
    for (k in 1:NumGammaM) {
      score = nl.grad(theta, llik_paired, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, j=j, k=k, pmfM=pmfM, pmfW=pmfW, 
                      Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric) - 
        nl.grad(theta, loglikfun,Xd=Xd,Zd=Zd,NumGammaW=NumGammaW,
                pmfW=pmfW,pmfM=pmfM,pmfj=pmfj,gw=gw, gm=gm, n=n, symmetric=symmetric, sampling=sampling)
        
      outer_grad = outer_grad + pmfj[j,k] * outer(score,score)
    }
  }
  
  # singles women (X each row)
  for (j in 1:NumGammaW) {
    score = nl.grad(theta, llik_sw, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, j=j, pmfW=pmfW,  
                    Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric) - 
      nl.grad(theta, loglikfun,Xd=Xd,Zd=Zd,NumGammaW=NumGammaW,
              pmfW=pmfW,pmfM=pmfM,pmfj=pmfj,gw=gw, gm=gm, n=n, symmetric=symmetric, sampling=sampling)
    
    outer_grad = outer_grad + pmfj[j,(1+NumGammaM)] * outer(score,score)
  }
  
  # single men (Z each col)
  for (k in 1:NumGammaM) {
    score = nl.grad(theta, llik_sm, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, k=k, pmfM=pmfM, 
                    Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric) - 
      nl.grad(theta, loglikfun,Xd=Xd,Zd=Zd,NumGammaW=NumGammaW,
              pmfW=pmfW,pmfM=pmfM,pmfj=pmfj,gw=gw, gm=gm, n=n, symmetric=symmetric, sampling=sampling)
    
    outer_grad = outer_grad + pmfj[(1+NumGammaW),k] * outer(score,score)
  }
  
  return (outer_grad)
  
}



ave_hessian_CP = function(theta, NumBeta, NumGammaW, NumGammaM, Xd, Zd, pmfW, pmfM, pmfj, gw, gm, n, symmetric){
  
  ave_hess = matrix(0, nrow=length(theta), ncol=length(theta))
  
  # couples
  for (j in 1:NumGammaW) {
    for (k in 1:NumGammaM) {
      
      hess = hessian(llik_paired, theta, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, j=j, k=k, pmfM=pmfM, pmfW=pmfW, 
                     Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric)
      
      ave_hess = ave_hess + pmfj[j,k] * hess
    }
  }
  
  # singles women (X each row)
  for (j in 1:NumGammaW) {
    
    hess = hessian(llik_sw, theta, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, j=j, pmfW=pmfW,  
                   Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric)
    
    ave_hess = ave_hess + pmfj[j,(1+NumGammaM)] * hess
  }
  
  # single men (Z each col)
  for (k in 1:NumGammaM) {
    
    hess = hessian(llik_sm, theta, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM, k=k, pmfM=pmfM, 
                   Xd=Xd, Zd=Zd, gw=gw, gm=gm, n=n, symmetric=symmetric)
    
    ave_hess = ave_hess + pmfj[(1+NumGammaW),k] * hess
  }
  
  return (ave_hess)
  
}


asympt_var = function(theta, NumBeta, NumGammaW, NumGammaM, Xd, Zd, pmfW, pmfM, pmfj, gw, gm, n, symmetric, sampling, loglikfun) {
  
  # # not centered
  # outer_grad = outer_prod_gradient_CP(theta = theta, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM,
  #                                     Xd=Xd,Zd=Zd,pmfW=pmfW, pmfM=pmfM,pmfj=pmfj, gw=gw, gm=gm, n=n, symmetric=symmetric)
  
  # centered
  outer_grad_2 = outer_prod_gradient_CP_2(theta = theta, loglikfun=loglikfun, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM,
                                          Xd=Xd,Zd=Zd,pmfW=pmfW, pmfM=pmfM,pmfj=pmfj, gw=gw, gm=gm, n=n, symmetric=symmetric, sampling=sampling)
  
  hess_CP = ave_hessian_CP(theta = theta, NumBeta=NumBeta, NumGammaW=NumGammaW, NumGammaM=NumGammaM,
                           Xd=Xd,Zd=Zd,pmfW=pmfW, pmfM=pmfM,pmfj=pmfj, gw=gw, gm=gm, n=n, symmetric=symmetric)
  
  if (any(is.na(hess_CP)) || any(is.na(outer_grad_2))) {
    covar2 = rep(NA, NumBeta+NumGammaW+NumGammaM+2)
  } else {
    # covar = diag(ginv(hess_CP) %*% outer_grad %*% ginv(hess_CP)/n)
    covar2 = diag(ginv(hess_CP) %*% outer_grad_2 %*% ginv(hess_CP)/n)
  }
  
  # return(list(covar=covar, covar2=covar2, outer_grad=outer_grad, outer_grad_2=outer_grad_2, hess_CP=hess_CP))
  return(list(covar2=covar2, outer_grad_2=outer_grad_2, hess_CP=hess_CP))
}