eva <- function(S1_test,S2_test,A1_test,A2_test,y_test,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,RQLg_res,HDQg_res,cSS,LRs) 
{
  
  n_test = nrow(S1_test)
  X2_test = cbind(S1_test,A1_test,S2_test,rep(1,n_test))
  gg2 = SuperLearner(Y = A2_test, X = as.data.frame(X2_test), family = binomial(), SL.library = LRs, method = "method.NNloglik")
  gg1 = SuperLearner(Y = A1_test, X = as.data.frame(S1_test), family = binomial(), SL.library = LRs, method = "method.NNloglik")
  pd_A2 = as.vector(predict(gg2, as.data.frame(X2_test), onlySL = TRUE)$pred)
  pd_A1 = as.vector(predict(gg1, as.data.frame(S1_test), onlySL = TRUE)$pred)
  pi2 = ifelse(A2_test == 1, pd_A2, 1 - pd_A2)
  pi1 = ifelse(A1_test == 1, pd_A1, 1 - pd_A1)
  #pi2 = ifelse(A2_test == 1, mean(A2_test), 1 - mean(A2_test))
  #pi1 = ifelse(A1_test == 1, mean(A1_test), 1 - mean(A1_test))
  pi1pi2 = pi1*pi2
  
  #RQLg
  alpha_hat_RQLg = RQLg_res[[1]]; beta_hat_RQLg = RQLg_res[[2]]
  X1_RQLg = cbind(S1_test,1)
  A1_RQLg = as.vector(ifelse((X1_RQLg%*%beta_hat_RQLg) > (C1s[2]-C1s[1]), 1, 0)); 
  X2_RQLg = cbind(S1_test,A1_RQLg,S2_test,1)
  A2_RQLg = as.vector(ifelse((X2_RQLg%*%alpha_hat_RQLg) > (C2s[2]-C2s[1]), 1, 0));
  I_RQLg = ifelse(A1_RQLg == A1_test & A2_RQLg == A2_test, 1, 0)
  V1_RQLg = y_test*I_RQLg/pi1pi2
  V2_RQLg = I_RQLg/pi1pi2
  V_RQLg = mean(V1_RQLg)/mean(V2_RQLg) 
  
  d1 = ncol(S1_test); d2 = ncol(S2_test); alpha_hat1_RQLg = alpha_hat_RQLg[1:d1]; beta_hat1_RQLg = beta_hat_RQLg[1:d1]
  seta1 = which(abs(alpha_hat1_RQLg)>1e-6); seta1 = setdiff(seta1,cSS[1:2])
  setb1 = which(abs(beta_hat1_RQLg)>1e-6); setb1 = setdiff(setb1,cSS[1:2])
  ab_nz = union(seta1, setb1)
  J_ab1 = c()
  
  for(k1 in 1:(length(J1s))){
    JJ1 = c(L1,J1s[[k1]])
    if(all(is.element(ab_nz,JJ1))){
      J_ab1 = append(J_ab1,k1)
    }
  }
  
  alpha_hat2_RQLg = alpha_hat_RQLg[(d1+2):(d1+1+d2)]
  a2_nz = which(abs(alpha_hat2_RQLg)>1e-6)
  a2_nz = setdiff(a2_nz,cSS[3:4])
  J_a2 = c()
  
  for(k2 in 1:(length(J2s))){
    JJ2 = c(L2,J2s[[k2]])
    if(all(is.element(a2_nz,JJ2))){
      J_a2 = append(J_a2,k2)
    }
  }
  
  CJ2_RQLg = CJ2s[J_a2]; J2_RQLg = J_a2[which.min(CJ2_RQLg)];
  CJ1_RQLg = CJ1s[J_ab1]; J1_RQLg = J_ab1[which.min(CJ1_RQLg)];
  C_RQLg = mean(ifelse(A1_RQLg == 1, C1s[2], C1s[1])) + mean(ifelse(A2_RQLg == 1, C2s[2], C2s[1])) + min(CJ1_RQLg) + min(CJ2_RQLg)
  P_RQLg = V_RQLg - C_RQLg
  result_RQLg = list(V_RQLg,P_RQLg,A1_RQLg,A2_RQLg,J1_RQLg,J2_RQLg)
  
  #HDQg
  psi1_HDQg = HDQg_res[[1]]; psi2_HDQg = HDQg_res[[2]]; 
  H1_HDQg = cbind(1, S1_test)
  A1_HDQg = as.vector(ifelse((H1_HDQg%*%psi1_HDQg) > ((C1s[2]-C1s[1])/2), 1, -1)); 
  A1_HDQg_ = A1_HDQg; A1_HDQg_[A1_HDQg_==-1] = 0;
  S_HDQg = cbind(S1_test,A1_HDQg,S2_test); H2_HDQg = cbind(1,S_HDQg)
  A2_HDQg = as.vector(ifelse((H2_HDQg%*%psi2_HDQg) > ((C2s[2]-C2s[1])/2), 1, -1)); 
  A2_HDQg_ = A2_HDQg; A2_HDQg_[A2_HDQg_==-1] = 0;
  
  I_HDQg = ifelse(A1_HDQg_ == A1_test & A2_HDQg_ == A2_test, 1, 0)
  V1_HDQg = y_test*I_HDQg/pi1pi2
  V2_HDQg = I_HDQg/pi1pi2
  V_HDQg  = mean(V1_HDQg)/mean(V2_HDQg) 
  #V_HDQ  = mean(V1_HDQ)
  
  d1 = ncol(S1_test); psi21_HDQg = psi2_HDQg[2:(d1+1)]; psi11_HDQg = psi1_HDQg[2:(d1+1)]
  set21 = which(abs(psi21_HDQg)>1e-6); set21 = setdiff(set21,cSS[1:2])
  set11 = which(abs(psi11_HDQg)>1e-6); set11 = setdiff(set11,cSS[1:2])
  psi1g_nz = union(set21, set11)
  J_psi1g = c()
  for(k1 in 1:(length(J1s))){
    JJ1 = c(L1,J1s[[k1]])
    if(all(is.element(psi1g_nz, JJ1))){
      J_psi1g = append(J_psi1g,k1)
    }
  }
  psi22_HDQg = psi2_HDQg[(d1+3):length(psi2_HDQg)]
  #psi22g_nz = which(psi22_HDQg != 0)
  psi22g_nz = which(abs(psi22_HDQg)>1e-6); psi22g_nz = setdiff(psi22g_nz,cSS[3:4])
  J_psi22g = c()
  for(k2 in 1:(length(J2s))){
    JJ2 = c(L2,J2s[[k2]])
    if(all(is.element(psi22g_nz, JJ2))){
      J_psi22g = append(J_psi22g,k2)
    }
  }
  
  CJ2_HDQg = CJ2s[J_psi22g]; J2_HDQg = J_psi22g[which.min(CJ2_HDQg)];
  CJ1_HDQg = CJ1s[J_psi1g]; J1_HDQg = J_psi1g[which.min(CJ1_HDQg)];
  
  C_HDQg = mean(ifelse(A1_HDQg == 1, C1s[2], C1s[1])) + mean(ifelse(A2_HDQg == 1, C2s[2], C2s[1])) + min(CJ1_HDQg) + min(CJ2_HDQg)
  P_HDQg = V_HDQg - C_HDQg
  result_HDQg = list(V_HDQg,P_HDQg,A1_HDQg_,A2_HDQg_,J1_HDQg,J2_HDQg)
  
  return(list(result_RQLg,result_HDQg))
}

CV_GLasso = function(X, y, group_jj, cost_jj) {
  if (all(cost_jj <= 0)) {
    beta_hat = as.vector(lm(y ~ X - 1)$coef)
    return(beta_hat)
  }
  
  sorted_idx <- order(group_jj)
  X_sorted <- X[, sorted_idx]
  group_sorted <- group_jj[sorted_idx]
  
  lambda_max = 1; lambda_factor = 0.0001; nlambda = 100
  lambda_seq <- exp(seq(log(lambda_max), log(lambda_max * lambda_factor), length.out = nlambda))
  
  fit0 <- cv.sparsegl(X_sorted, y, group = group_sorted, asparse = 0, family = "gaussian", pf_group = cost_jj, standardize = FALSE, lambda = lambda_seq, intercept = FALSE)
  fit <- cv.sparsegl(X_sorted, y, group = group_sorted, asparse = 0, family = "gaussian", pf_group = cost_jj, standardize = FALSE, lambda = fit0$lambda, intercept = FALSE)
  
  lambda_seq <- fit$lambda
  mse_path   <- fit$cvm
  coef_mat   <- coef(fit, s = lambda_seq)
  
  n_lambdas <- length(lambda_seq)
  cost_path  <- numeric(n_lambdas)
  
  for (i in seq_len(n_lambdas)) {
    beta_sorted <- coef_mat[-1, i]
    beta_orig <- beta_sorted[order(sorted_idx)]
    
    costi <- 0
    for (g in seq_along(cost_jj)) {
      idx_g <- which(group_jj == g)
      if (sqrt(sum(beta_orig[idx_g]^2)) > 1e-6) {
        costi <- costi + cost_jj[g]
      }
    }
    cost_path[i] <- costi
  }
  
  total_path <- cost_path + mse_path
  best_idx   <- which.min(total_path)  
  
  best_beta_sorted <- coef_mat[-1 , best_idx]
  best_beta <- best_beta_sorted[order(sorted_idx)]
  return(as.vector(best_beta))
}

HDQ_glasso = function(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
{
  #stage 2
  A1[A1==0] = -1; A2[A2==0] = -1;
  n = nrow(S1); d1 = ncol(S1); d2 = ncol(S2)
  
  H2 = cbind(1, S1, A1, S2)
  p2 = ncol(H2)
  H2A2 = sweep(H2, 1, A2, "*")
  Z2 = cbind(H2, H2A2)
  
  groupZ2 = rep(1, (2*p2))
  c_g2 = c(0)
  gidx = 2
  for(i in seq_along(JJ1)){
    JJ1i = JJ1[[i]]
    CJJ1i = CJJ1[i]
    if(CJJ1i > 0){
      groupZ2[JJ1i+p2+1] = gidx 
      c_g2 = c(c_g2, CJJ1[i])
      gidx = gidx + 1 
    }
  }
  for(i in seq_along(JJ2)){
    JJ2i = JJ2[[i]]
    CJJ2i = CJJ2[i]
    if(CJJ2i > 0){
      groupZ2[JJ2i+p2+d1+2] = gidx
      c_g2 = c(c_g2, CJJ2[i])
      gidx = gidx + 1 
    }
  }
  beta_psi2 = CV_GLasso(Z2,y,groupZ2,c_g2)
  beta2 = beta_psi2[1:p2]
  psi2 = beta_psi2[(p2+1):(length(beta_psi2))] 
  
  #stage 1
  rho2n = n^(-2/5)
  abs_HAp = abs(H2A2%*%psi2); ind = abs_HAp>rho2n
  y1_hat = H2%*%beta2 + sweep(abs_HAp, MARGIN = 1, STATS = ind, FUN = "*") 
  H1 = cbind(1,S1)
  p1 = ncol(H1)
  H1A1 = sweep(H1, MARGIN = 1, STATS = A1, FUN = "*")
  Z1 = cbind(H1, H1A1)
  
  groupZ1 = rep(1, (2*p1))
  c_g1 = c(0)
  gidx = 2
  for(i in seq_along(JJ1)){
    JJ1i = JJ1[[i]]
    CJJ1i = CJJ1[i]
    if(CJJ1i > 0){
      groupZ1[JJ1i+p1+1] = gidx 
      c_g1 = c(c_g1, CJJ1[i])
      gidx = gidx + 1 
    }
  }
  beta_psi1 = CV_GLasso(Z1,y1_hat,groupZ1,c_g1)
  beta1 = beta_psi1[1:p1]
  psi1 = beta_psi1[(p1+1):(length(beta_psi1))]
  return(list(psi1,psi2))
}

RQL_glasso = function(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners)
{
  n = nrow(S1); d1 = ncol(S1); cf = sample(1:n,n/2)
  
  X2 = cbind(S1,A1,S2,1)
  g_hat1 = SuperLearner(Y = A2[cf], X = as.data.frame(X2[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  g_hat2 = SuperLearner(Y = A2[-cf], X = as.data.frame(X2[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  f_hat1 = SuperLearner(Y = y[cf], X = as.data.frame(X2[cf,]), family = binomial(), SL.library = Learners,method = "method.NNloglik")
  f_hat2 = SuperLearner(Y = y[-cf], X = as.data.frame(X2[-cf,]), family = binomial(), SL.library = Learners,method = "method.NNloglik")
  
  gg = A2
  gg[cf] = as.vector(predict(g_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  gg[-cf] = as.vector(predict(g_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
  ff = y
  ff[cf] = as.vector(predict(f_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  ff[-cf] = as.vector(predict(f_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
  impute_y = y-ff; impute_x = X2*(A2-gg);
  
  groupX2 = rep(1, ncol(X2))
  c_g2 = c(0)
  gidx = 2
  for(i in seq_along(JJ1)){
    JJ1i = JJ1[[i]]
    CJJ1i = CJJ1[i]
    if(CJJ1i > 0){
      groupX2[JJ1i] = gidx 
      c_g2 = c(c_g2, CJJ1[i])
      gidx = gidx + 1 
    }
  }
  for(i in seq_along(JJ2)){
    JJ2i = JJ2[[i]]
    CJJ2i = CJJ2[i]
    if(CJJ2i > 0){
      groupX2[JJ2i+d1+1] = gidx
      c_g2 = c(c_g2, CJJ2[i])
      gidx = gidx + 1 
    }
  }
  alpha_hat = CV_GLasso(impute_x,impute_y,groupX2,c_g2)
  
  g1 = as.vector(predict(g_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  g2 = as.vector(predict(g_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  f1 = as.vector(predict(f_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  f2 = as.vector(predict(f_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  impute_y1 = y[cf] - f1; impute_x1 = X2[cf,]*(A2[cf]-g1);
  impute_y2 = y[-cf] - f2; impute_x2 = X2[-cf,]*(A2[-cf]-g2);
  alpha_hat1 = CV_GLasso(impute_x1,impute_y1,groupX2,c_g2)
  alpha_hat2 = CV_GLasso(impute_x2,impute_y2,groupX2,c_g2)
  
  X1 = cbind(S1,1)
  X2_alpha = X2%*%alpha_hat; y2 = y + X2_alpha*((X2_alpha>0)-A2)
  X2_alpha1 = X2[cf,]%*%alpha_hat1; y21 = y[cf] + X2_alpha1*((X2_alpha1>0)-A2[cf]) 
  X2_alpha2 = X2[-cf,]%*%alpha_hat2; y22 = y[-cf] + X2_alpha2*((X2_alpha2>0)-A2[-cf])
  
  g21 = SuperLearner(Y = A1[cf], X = as.data.frame(X1[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  g22 = SuperLearner(Y = A1[-cf], X = as.data.frame(X1[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  f21 = SuperLearner(Y = y21, X = as.data.frame(X1[cf,]), SL.library = Learners)
  f22 = SuperLearner(Y = y22, X = as.data.frame(X1[-cf,]), SL.library = Learners)
  
  gg2 = A1
  gg2[cf] = as.vector(predict(g22, as.data.frame(X1[cf,]), onlySL = TRUE)$pred)
  gg2[-cf] = as.vector(predict(g21, as.data.frame(X1[-cf,]), onlySL = TRUE)$pred)
  
  ff2 = y2
  ff2[cf] = as.vector(predict(f22, as.data.frame(X1[cf,]), onlySL = TRUE)$pred)
  ff2[-cf] = as.vector(predict(f21, as.data.frame(X1[-cf,]), onlySL = TRUE)$pred)
  
  impute_1y2 = y2-ff2; impute_1x2 = X1*(A1-gg2);
  groupX1 = rep(1, ncol(X1))
  c_g1 = c(0)
  gidx = 2
  for(i in seq_along(JJ1)){
    JJ1i = JJ1[[i]]
    CJJ1i = CJJ1[i]
    if(CJJ1i > 0){
      groupX1[JJ1i] = gidx 
      c_g1 = c(c_g1, CJJ1[i])
      gidx = gidx + 1 
    }
  }
  beta_hat = CV_GLasso(impute_1x2,impute_1y2,groupX1,c_g1)
  return(list(alpha_hat,beta_hat))
}

