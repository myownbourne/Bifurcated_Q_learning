eva <- function(S1_test,S2_test,A1_test,A2_test,y_test,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,RQL_res,HDQ_res,BQL_res,LRs) 
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
  
  #BQL
  alphas_BQL = BQL_res[[1]]; betas_BQL = BQL_res[[2]]; gammas_BQL = BQL_res[[3]]; deltas_BQL = BQL_res[[4]];
  SL1_test = as.matrix(S1_test[,L1])
  J1_BQL = rep(0,n_test)
  A1_BQL = rep(0,n_test)
  J2_BQL = rep(0,n_test)
  A2_BQL = rep(0,n_test)
  
  for(i in 1:n_test){
    SL1i = SL1_test[i,]
    SL1_delta = rep(0,length(J1s))
    for(k1 in 1:length(J1s)){
      delta_BQL_k1 = deltas_BQL[[k1]]$delta_hat_k1
      SL1_delta[k1] = c(SL1i,1)%*%delta_BQL_k1
    }
    J1_BQLi = which.max(SL1_delta)
    J1_BQL[i] = J1_BQLi
    
    SJ1i = S1_test[i,J1s[[J1_BQLi]]]
    SJ1ib = c(SL1i,SJ1i,1)
    gamma_BQL_k1 = gammas_BQL[[J1_BQLi]]$gamma_hat_k1
    A1_BQL[i] = (SJ1ib%*%gamma_BQL_k1)>0
  }
  
  SL2 = as.matrix(S2_test[,L2])
  for(i in 1:n_test){
    J1_BQLi = J1_BQL[i]
    SL2ib = c(SL1_test[i,],S1_test[i,J1s[[J1_BQLi]]],SL2[i,])
    SL2b_beta = rep(0,length(J2s))
    for(k2 in 1:length(J2s)){
      beta_BQL_0k1k2 = betas_BQL[[J1_BQLi]][[k2]][[1]]$beta_hat_a1k1k2
      beta_BQL_1k1k2 = betas_BQL[[J1_BQLi]][[k2]][[2]]$beta_hat_a1k1k2
      if(A1_BQL[i]==0){
        SL2b_beta[k2] = c(SL2ib,1)%*%beta_BQL_0k1k2
      }else{
        SL2b_beta[k2] = c(SL2ib,1)%*%beta_BQL_1k1k2
      }
    }
    J2_BQLi = which.max(SL2b_beta)
    J2_BQL[i] = J2_BQLi
    
    SJ2i = S2_test[i,J2s[[J2_BQLi]]]
    SJ2ib = c(SL2ib,SJ2i,1)
    if(A1_BQL[i]==0){
      alpha_BQL_a1k1k2 = alphas_BQL[[J1_BQLi]][[J2_BQLi]][[1]]$alpha_hat_a1k1k2 
    }else{
      alpha_BQL_a1k1k2 = alphas_BQL[[J1_BQLi]][[J2_BQLi]][[2]]$alpha_hat_a1k1k2 
    }
    A2_BQL[i] = (SJ2ib%*%alpha_BQL_a1k1k2)>0
  }
  
  I_BQL = ifelse(A1_BQL == A1_test & A2_BQL == A2_test, 1, 0)
  y_test = as.vector(y_test)
  
  V1_BQL = y_test*I_BQL/pi1pi2
  V2_BQL = I_BQL/pi1pi2
  V_BQL = mean(V1_BQL)/mean(V2_BQL) 
  C_BQL = mean(ifelse(A1_BQL == 1, C1s[2], C1s[1])) + mean(ifelse(A2_BQL == 1, C2s[2], C2s[1])) + mean(CJ1s[J1_BQL]) + mean(CJ2s[J2_BQL])
  P_BQL = V_BQL - C_BQL
  result_BQL = list(V_BQL,P_BQL,J1_BQL,J2_BQL,A1_BQL,A2_BQL)
  
  #RQL
  alpha_hat_RQL = RQL_res[[1]]; beta_hat_RQL = RQL_res[[2]]
  X1_RQL = cbind(S1_test,rep(1,n_test))
  A1_RQL = as.vector(ifelse((X1_RQL%*%beta_hat_RQL) > (C1s[2]-C1s[1]), 1, 0)) 
  X2_RQL = cbind(S1_test,A1_RQL,S2_test,rep(1,n_test))
  A2_RQL = as.vector(ifelse((X2_RQL%*%alpha_hat_RQL) > (C2s[2]-C2s[1]), 1, 0))
  
  I_RQL = ifelse(A1_RQL == A1_test & A2_RQL == A2_test, 1, 0)
  
  V1_RQL = y_test*I_RQL/pi1pi2
  V2_RQL = I_RQL/pi1pi2
  V_RQL = mean(V1_RQL)/mean(V2_RQL) 
  #V_RQL  = mean(V1_RQL)
  C_RQL = mean(ifelse(A1_RQL == 1, C1s[2], C1s[1])) + mean(ifelse(A2_RQL == 1, C2s[2], C2s[1])) + CJ1s[length(CJ1s)] + CJ2s[length(CJ2s)] 
  P_RQL = V_RQL - C_RQL
  result_RQL = list(V_RQL,P_RQL,A1_RQL,A2_RQL)
  
  
  #HDQ
  psi1_HDQ = HDQ_res[[1]]; psi2_HDQ = HDQ_res[[2]]; 
  H1_HDQ = cbind(rep(1,n_test), S1_test)
  A1_HDQ = as.vector(ifelse((H1_HDQ%*%psi1_HDQ) > ((C1s[2]-C1s[1])/2), 1, -1)); 
  A1_HDQ_ = A1_HDQ; A1_HDQ_[A1_HDQ_==-1] = 0;
  S_HDQ = cbind(S1_test,A1_HDQ,S2_test); H2_HDQ = cbind(rep(1,n_test),S_HDQ)
  A2_HDQ = as.vector(ifelse((H2_HDQ%*%psi2_HDQ) > ((C2s[2]-C2s[1])/2), 1, -1)); 
  A2_HDQ_ = A2_HDQ; A2_HDQ_[A2_HDQ_==-1] = 0;
  
  I_HDQ = ifelse(A1_HDQ_ == A1_test & A2_HDQ_ == A2_test, 1, 0)
  V1_HDQ = y_test*I_HDQ/pi1pi2
  V2_HDQ = I_HDQ/pi1pi2
  V_HDQ  = mean(V1_HDQ)/mean(V2_HDQ) 
  #V_HDQ  = mean(V1_HDQ)
  
  d1 = ncol(S1_test); psi21_HDQ = psi2_HDQ[2:(d1+1)]; psi11_HDQ = psi1_HDQ[2:(d1+1)]
  psi1_nz = union(which(psi21_HDQ!=0), which(psi11_HDQ!=0))
  J_psi1 = c()
  for(k1 in 1:(length(J1s))){
    JJ1 = c(L1,J1s[[k1]])
    if(all(is.element(psi1_nz, JJ1))){
      J_psi1 = append(J_psi1,k1)
    }
  }
  psi22_HDQ = psi2_HDQ[(d1+3):length(psi2_HDQ)]
  psi22_nz = which(psi22_HDQ != 0)
  #psi22_nz = which(abs(psi22)>1e-4)
  J_psi22 = c()
  for(k2 in 1:(length(J2s))){
    JJ2 = c(L2,J2s[[k2]])
    if(all(is.element(psi22_nz, JJ2))){
      J_psi22 = append(J_psi22,k2)
    }
  }
  
  CJ2_HDQ = CJ2s[J_psi22]; J2_HDQ = J_psi22[which.min(CJ2_HDQ)];
  CJ1_HDQ = CJ1s[J_psi1]; J1_HDQ = J_psi1[which.min(CJ1_HDQ)];
  
  C_HDQ = mean(ifelse(A1_HDQ == 1, C1s[2], C1s[1])) + mean(ifelse(A2_HDQ == 1, C2s[2], C2s[1])) + min(CJ1_HDQ) + min(CJ2_HDQ)
  P_HDQ = V_HDQ - C_HDQ
  result_HDQ = list(V_HDQ,P_HDQ,A1_HDQ,A2_HDQ,J1_HDQ,J2_HDQ)
  
  return(list(result_BQL,result_RQL,result_HDQ))
}

BQL_4 <- function(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
{
  n = nrow(S1)
  cf = sample(1:n,n/2)
  
  alphas = list()
  betas = list()
  beta_tildes = list()
  gammas = list()
  gamma_tildes = list()
  deltas = list()
  y_J1s = list()
  
  #Step 1
  y1 = y;
  S2_b = cbind(S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2],S2[,J2s[[length(J2s)]]],rep(1,n))
  X2_b = cbind(A1,S2_b)
  g_hat1 = SuperLearner(Y = A2[cf], X = as.data.frame(X2_b[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  g_hat2 = SuperLearner(Y = A2[-cf], X = as.data.frame(X2_b[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  f_hat1 = SuperLearner(Y = y1[cf], X = as.data.frame(X2_b[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  f_hat2 = SuperLearner(Y = y1[-cf], X = as.data.frame(X2_b[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  
  gk = A2
  gk[cf] = as.vector(predict(g_hat2, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  gk[-cf] = as.vector(predict(g_hat1, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  
  fk = y1
  fk[cf] = as.vector(predict(f_hat2, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  fk[-cf] = as.vector(predict(f_hat1, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  
  impute_y = y1 - fk; impute_x = X2_b*(A2-gk);
  alpha_hat = as.vector(lm(impute_y ~ impute_x-1)$coefficients)
  alpha_hat[length(alpha_hat)] = alpha_hat[length(alpha_hat)] - (C2s[2]-C2s[1])
  
  gk1 = as.vector(predict(g_hat1, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  gk2 = as.vector(predict(g_hat2, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  fk1 = as.vector(predict(f_hat1, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  fk2 = as.vector(predict(f_hat2, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  impute_y1 = y1[cf] - fk1; impute_x1 = X2_b[cf,]*(A2[cf]-gk1);
  impute_y2 = y1[-cf] - fk2; impute_x2 = X2_b[-cf,]*(A2[-cf]-gk2);
  alpha_hat1 = as.vector(lm(impute_y1 ~ impute_x1-1)$coefficients)
  alpha_hat2 = as.vector(lm(impute_y2 ~ impute_x2-1)$coefficients)
  alpha_hat1[length(alpha_hat1)] = alpha_hat1[length(alpha_hat1)] - (C2s[2]-C2s[1])
  alpha_hat2[length(alpha_hat2)] = alpha_hat2[length(alpha_hat2)] - (C2s[2]-C2s[1])
  
  for(k1 in 1:length(J1s)){
    alphas_k1 = list()
    for(k2 in 1:length(J2s)){
      alphas_k1k2 = list()
      for(a1 in 1:2){
        ta = a1 - 1; 
        aS2_b = cbind(ta*rep(1,n),S2_b);
        SJ2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[k2]]],rep(1,n));
        
        impute_yJ2 = aS2_b%*%alpha_hat;
        impute_yJ21 = aS2_b[cf,]%*%alpha_hat1;
        impute_yJ22 = aS2_b[-cf,]%*%alpha_hat2;
        impute_xJ21 = SJ2_b[cf,];
        impute_xJ22 = SJ2_b[-cf,];
        
        alpha_hat_a1k1k2 = as.vector(lm(impute_yJ2 ~ SJ2_b-1)$coefficients)
        alpha_hat_a1k1k21 = as.vector(lm(impute_yJ21 ~ impute_xJ21-1)$coefficients)
        alpha_hat_a1k1k22 = as.vector(lm(impute_yJ22 ~ impute_xJ22-1)$coefficients)
        alphas_k1k2[[a1]] = list(alpha_hat_a1k1k2 = alpha_hat_a1k1k2, alpha_hat_a1k1k21 = alpha_hat_a1k1k21, alpha_hat_a1k1k22 = alpha_hat_a1k1k22)
      }
      alphas_k1[[k2]] = alphas_k1k2;
    }
    alphas[[k1]] = alphas_k1;
  }
  
  #Step 2
  for(k1 in 1:length(J1s)){
    betas_k1 = list()
    beta_tildes_k1 = list()
    for(k2 in 1:length(J2s)){
      betas_k1k2 = list()
      if(k2==length(J2s)){
        XL2f_b = cbind(A1,S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2],rep(1,n));
        beta_tilde_k1f = rep(0,ncol(XL2f_b))
        beta_tildes_k1[[k2]] = list(beta_tilde_k1k2 = beta_tilde_k1f, beta_tilde_k1k21 = beta_tilde_k1f, beta_tilde_k1k22 = beta_tilde_k1f)
        for(a1 in 1:2){
          SL2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],rep(1,n));
          beta_hat_a1k1f = rep(0,ncol(SL2_b))
          betas_k1k2[[a1]] = list(beta_hat_a1k1k2 = beta_hat_a1k1f, beta_hat_a1k1k21 = beta_hat_a1k1f, beta_hat_a1k1k22 = beta_hat_a1k1f)
        }
        betas_k1[[k2]] = betas_k1k2;
      }else{
        XL2f_b = cbind(A1,S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2],rep(1,n));
        X2_balpha = X2_b%*%alpha_hat; X2_balpha1 = X2_b[cf,]%*%alpha_hat1; X2_balpha2 = X2_b[-cf,]%*%alpha_hat2;
        
        SJ2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[k2]]],rep(1,n))
        alpha_hat_0k1k2 = alphas[[k1]][[k2]][[1]]$alpha_hat_a1k1k2
        alpha_hat_0k1k21 = alphas[[k1]][[k2]][[1]]$alpha_hat_a1k1k21 
        alpha_hat_0k1k22 = alphas[[k1]][[k2]][[1]]$alpha_hat_a1k1k22 
        alpha_hat_1k1k2 = alphas[[k1]][[k2]][[2]]$alpha_hat_a1k1k2
        alpha_hat_1k1k21 = alphas[[k1]][[k2]][[2]]$alpha_hat_a1k1k21 
        alpha_hat_1k1k22 = alphas[[k1]][[k2]][[2]]$alpha_hat_a1k1k22 
        SJ2_b_alpha = (A1 == 0) * (SJ2_b %*% alpha_hat_0k1k2) + (A1 == 1) * (SJ2_b %*% alpha_hat_1k1k2)
        SJ2_b_alpha1 = (A1[cf] == 0) * (SJ2_b[cf,] %*% alpha_hat_0k1k21) + (A1[cf] == 1) * (SJ2_b[cf,] %*% alpha_hat_1k1k21)
        SJ2_b_alpha2 = (A1[-cf] == 0) * (SJ2_b[-cf,] %*% alpha_hat_0k1k22) + (A1[-cf] == 1) * (SJ2_b[-cf,] %*% alpha_hat_1k1k22)
        
        
        SJ2f_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[length(J2s)]]],rep(1,n))
        alpha_hat_0k1f = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k2
        alpha_hat_0k1f1 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k21 
        alpha_hat_0k1f2 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k22 
        alpha_hat_1k1f = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k2
        alpha_hat_1k1f1 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k21 
        alpha_hat_1k1f2 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k22 
        SJ2f_b_alpha = (A1 == 0) * (SJ2f_b %*% alpha_hat_0k1f) + (A1 == 1) * (SJ2f_b %*% alpha_hat_1k1f)
        SJ2f_b_alpha1 = (A1[cf] == 0) * (SJ2f_b[cf,] %*% alpha_hat_0k1f1) + (A1[cf] == 1) * (SJ2f_b[cf,] %*% alpha_hat_1k1f1)
        SJ2f_b_alpha2 = (A1[-cf] == 0) * (SJ2f_b[-cf,] %*% alpha_hat_0k1f2) + (A1[-cf] == 1) * (SJ2f_b[-cf,] %*% alpha_hat_1k1f2)
        
        impute_y = (X2_balpha*(SJ2_b_alpha>0) - CJ2s[k2]) - (X2_balpha*(SJ2f_b_alpha>0) - CJ2s[length(CJ2s)])
        beta_tilde_k1k2 = as.vector(lm(impute_y ~ XL2f_b-1)$coefficients)
        
        impute_y1 = (X2_balpha1*(SJ2_b_alpha1>0) - CJ2s[k2]) - (X2_balpha1*(SJ2f_b_alpha1>0) - CJ2s[length(CJ2s)])
        beta_tilde_k1k21 = as.vector(lm(impute_y1 ~ XL2f_b[cf,]-1)$coefficients)
        
        impute_y2 = (X2_balpha2*(SJ2_b_alpha2>0) - CJ2s[k2]) - (X2_balpha2*(SJ2f_b_alpha2>0) - CJ2s[length(CJ2s)])
        beta_tilde_k1k22 = as.vector(lm(impute_y2 ~ XL2f_b[-cf,]-1)$coefficients)
        beta_tildes_k1[[k2]] = list(beta_tilde_k1k2 = beta_tilde_k1k2, beta_tilde_k1k21 = beta_tilde_k1k21, beta_tilde_k1k22 = beta_tilde_k1k22)
        
        for(a1 in 1:2){
          ta = a1 - 1; 
          aSL2f_b = cbind(ta*rep(1,n),S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2],rep(1,n));
          SL2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],rep(1,n));
          
          impute_yJ1 = aSL2f_b%*%beta_tilde_k1k2;
          impute_yJ11 = aSL2f_b[cf,]%*%beta_tilde_k1k21;
          impute_yJ12 = aSL2f_b[-cf,]%*%beta_tilde_k1k22;
          
          beta_hat_a1k1k2 = as.vector(lm(impute_yJ1 ~ SL2_b-1)$coefficients)
          beta_hat_a1k1k21 = as.vector(lm(impute_yJ11 ~ SL2_b[cf,]-1)$coefficients)
          beta_hat_a1k1k22 = as.vector(lm(impute_yJ12 ~ SL2_b[-cf,]-1)$coefficients)
          
          betas_k1k2[[a1]] = list(beta_hat_a1k1k2 = beta_hat_a1k1k2, beta_hat_a1k1k21 = beta_hat_a1k1k21, beta_hat_a1k1k22 = beta_hat_a1k1k22)
        }
        betas_k1[[k2]] = betas_k1k2;
      }
    }
    betas[[k1]] = betas_k1;
    beta_tildes[[k1]] = beta_tildes_k1;
  }
  
  #Step 3
  X2_b_alpha = X2_b%*%alpha_hat; X2_b_alpha1 = X2_b[cf,]%*%alpha_hat1; X2_b_alpha2 = X2_b[-cf,]%*%alpha_hat2; 
  SJ1f_b = cbind(S1[,L1],S1[,J1s[[length(J1s)]]],rep(1,n));
  g_hat31 = SuperLearner(Y = A1[cf], X = as.data.frame(SJ1f_b[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  g_hat32 = SuperLearner(Y = A1[-cf], X = as.data.frame(SJ1f_b[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  for(k1 in 1:length(J1s)){
    SJ1_b = cbind(S1[,L1],S1[,J1s[[k1]]],rep(1,n));
    SJ2f_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[length(J2s)]]],rep(1,n));
    alpha_hat_0k1f = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k2
    alpha_hat_0k1f1 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k21 
    alpha_hat_0k1f2 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k22
    
    alpha_hat_1k1f = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k2
    alpha_hat_1k1f1 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k21 
    alpha_hat_1k1f2 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k22
    
    SJ2f_b_alpha = (A1 == 0) * (SJ2f_b %*% alpha_hat_0k1f) + (A1 == 1) * (SJ2f_b %*% alpha_hat_1k1f)
    SJ2f_b_alpha1 = (A1[cf] == 0) * (SJ2f_b[cf,] %*% alpha_hat_0k1f1) + (A1[cf] == 1) * (SJ2f_b[cf,] %*% alpha_hat_1k1f1)
    SJ2f_b_alpha2 = (A1[-cf] == 0) * (SJ2f_b[-cf,] %*% alpha_hat_0k1f2) + (A1[-cf] == 1) * (SJ2f_b[-cf,] %*% alpha_hat_1k1f2)
    
    SL2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],rep(1,n))
    XL2f_b = cbind(A1,S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2],rep(1,n))
    
    list_Sbeta = list(); list_Sbeta1 = list(); list_Sbeta2 = list();
    list_Xfbeta = list(); list_Xfbeta1 = list(); list_Xfbeta2 = list();
    
    for(k2 in 1:(length(J2s))){
      beta_hat_0k1k2 = betas[[k1]][[k2]][[1]]$beta_hat_a1k1k2
      beta_hat_0k1k21 = betas[[k1]][[k2]][[1]]$beta_hat_a1k1k21
      beta_hat_0k1k22 = betas[[k1]][[k2]][[1]]$beta_hat_a1k1k22
      
      beta_hat_1k1k2 = betas[[k1]][[k2]][[2]]$beta_hat_a1k1k2
      beta_hat_1k1k21 = betas[[k1]][[k2]][[2]]$beta_hat_a1k1k21
      beta_hat_1k1k22 = betas[[k1]][[k2]][[2]]$beta_hat_a1k1k22
      
      beta_tilde_k1k2 = beta_tildes[[k1]][[k2]]$beta_tilde_k1k2
      beta_tilde_k1k21 = beta_tildes[[k1]][[k2]]$beta_tilde_k1k21
      beta_tilde_k1k22 = beta_tildes[[k1]][[k2]]$beta_tilde_k1k22
      
      list_Sbeta[[k2]] = (A1 == 0) * (SL2_b%*%beta_hat_0k1k2) + (A1 == 1) * (SL2_b%*%beta_hat_1k1k2)
      list_Sbeta1[[k2]] = (A1[cf] == 0) * (SL2_b[cf,]%*%beta_hat_0k1k21) + (A1[cf] == 1) * (SL2_b[cf,]%*%beta_hat_1k1k21)
      list_Sbeta2[[k2]] = (A1[-cf] == 0) * (SL2_b[-cf,]%*%beta_hat_0k1k22) + (A1[-cf] == 1) * (SL2_b[-cf,]%*%beta_hat_1k1k22)
      
      list_Xfbeta[[k2]] = XL2f_b%*%beta_tilde_k1k2
      list_Xfbeta1[[k2]] = XL2f_b[cf,]%*%beta_tilde_k1k21
      list_Xfbeta2[[k2]] = XL2f_b[-cf,]%*%beta_tilde_k1k22
    }
    
    Sbeta_mat = do.call(cbind, list_Sbeta)
    Sbeta_mat1 = do.call(cbind, list_Sbeta1)
    Sbeta_mat2 = do.call(cbind, list_Sbeta2)
    Xfbeta_mat = do.call(cbind, list_Xfbeta)
    Xfbeta_mat1 = do.call(cbind, list_Xfbeta1)
    Xfbeta_mat2 = do.call(cbind, list_Xfbeta2)
    k2_idx  = apply(Sbeta_mat, 1, which.max)
    f1_values = sapply(1:length(k2_idx), function(i) Xfbeta_mat[i, k2_idx[i]])
    k2_idx1  = apply(Sbeta_mat1, 1, which.max)
    f1_values1 = sapply(1:length(k2_idx1), function(i) Xfbeta_mat1[i, k2_idx1[i]])
    k2_idx2  = apply(Sbeta_mat2, 1, which.max)
    f1_values2 = sapply(1:length(k2_idx2), function(i) Xfbeta_mat2[i, k2_idx2[i]])
    
    yy = y - ifelse(A2 == 1, C2s[2], C2s[1]) - CJ2s[length(CJ2s)]
    y3 = yy + X2_b_alpha*((SJ2f_b_alpha>0)-A2) + f1_values
    y31 = yy[cf] + X2_b_alpha1*((SJ2f_b_alpha1>0)-A2[cf]) + f1_values1
    y32 = yy[-cf] + X2_b_alpha2*((SJ2f_b_alpha2>0)-A2[-cf]) + f1_values2
    
    y_J1s[[k1]] = y3;
    
    f_hat31 = SuperLearner(Y = y31, X = as.data.frame(SJ1f_b[cf,]), SL.library = Learners)
    f_hat32 = SuperLearner(Y = y32, X = as.data.frame(SJ1f_b[-cf,]), SL.library = Learners)
    
    g3_tilde = A1
    g3_tilde[cf] = as.vector(predict(g_hat32, as.data.frame(SJ1f_b[cf,]), onlySL = TRUE)$pred)
    g3_tilde[-cf] = as.vector(predict(g_hat31, as.data.frame(SJ1f_b[-cf,]), onlySL = TRUE)$pred)
    
    f3_tilde = y3
    f3_tilde[cf] = as.vector(predict(f_hat32, as.data.frame(SJ1f_b[cf,]), onlySL = TRUE)$pred)
    f3_tilde[-cf] = as.vector(predict(f_hat31, as.data.frame(SJ1f_b[-cf,]), onlySL = TRUE)$pred)
    
    impute_y3_tilde = y3-f3_tilde; impute_x3_tilde = SJ1f_b*(A1-g3_tilde);
    gamma_tilde_k1 = as.vector(lm(impute_y3_tilde ~ impute_x3_tilde-1)$coefficients)
    gamma_tilde_k1[length(gamma_tilde_k1)] = gamma_tilde_k1[length(gamma_tilde_k1)] - (C1s[2]-C1s[1])
    gamma_tildes[[k1]] = list(gamma_tilde_k1 = gamma_tilde_k1)
    
    impute_y3 = SJ1f_b%*%gamma_tilde_k1;
    gamma_hat_k1 = as.vector(lm(impute_y3 ~ SJ1_b-1)$coefficients)
    gammas[[k1]] = list(gamma_hat_k1 = gamma_hat_k1)
  }
  
  
  #Step 4
  S1b = cbind(S1[,L1],S1[,J1s[[length(J1s)]]],rep(1,n))
  SL1 = cbind(S1[,L1],rep(1,n))
  
  if(length(J1s)!=1){
    for(k1 in 1:(length(J1s)-1)){
      SJ1_b = cbind(S1[,L1],S1[,J1s[[k1]]],rep(1,n))
      
      gamma_hat_f = gammas[[length(J1s)]]$gamma_hat_k1
      gamma_hat_k1 = gammas[[k1]]$gamma_hat_k1
      gamma_tilde_k1 = gamma_tildes[[k1]]$gamma_tilde_k1
      gamma_tilde_f = gamma_tildes[[length(J1s)]]$gamma_tilde_k1
      
      SJ1f_gamma_tilde = S1b%*%gamma_tilde_f
      SJ1_gamma_tilde = S1b%*%gamma_tilde_k1
      SJ1f_gamma = S1b%*%gamma_hat_f
      SJ1_gamma = SJ1_b%*%gamma_hat_k1
      
      y_J1 = y_J1s[[k1]] - ifelse(A1 == 1, C1s[2], C1s[1]) 
      y_J1f = y_J1s[[length(J1s)]] - ifelse(A1 == 1, C1s[2], C1s[1]) 
      
      impute_yJ1 = y_J1 + SJ1_gamma_tilde*((SJ1_gamma>0) - A1) - CJ1s[k1]
      impute_yJ1f = y_J1f + SJ1f_gamma_tilde*((SJ1f_gamma>0) - A1) - CJ1s[length(CJ1s)]
      
      impute_y = impute_yJ1 - impute_yJ1f
      delta_hat_k1 = as.vector(lm(impute_y ~ SL1-1)$coefficients)
      deltas[[k1]] = list(delta_hat_k1 = delta_hat_k1)
    }
  }
  
  delta_f = rep(0,ncol(SL1))
  deltas[[length(J1s)]] = list(delta_hat_k1 = delta_f)
  
  return(list(alphas,betas,gammas,deltas))
}

HDQ_new = function(S1,S2,A1,A2,y) 
{
  A1[A1==0] = -1; A2[A2==0] = -1;
  n = nrow(S2)
  H2 = cbind(rep(1,n),S1,A1,S2)
  p2 = ncol(H2)
  H2A2 = sweep(H2, MARGIN = 1, STATS = A2, FUN = "*")
  Z2 = cbind(H2, H2A2)
  F2 = cv.ncvreg(Z2, y, penalty="SCAD")
  beta_psi2_ = as.numeric(coef(F2));
  beta_psi2 = beta_psi2_[-1]; beta20 = beta_psi2_[1]
  beta2 = beta_psi2[1:p2]; psi2 = beta_psi2[(p2+1):(2*p2)]
  
  rho2n = n^(-2/5)
  abs_HAp = abs(H2A2%*%psi2); ind = abs_HAp>rho2n
  y1_hat = beta20 + H2%*%beta2 + sweep(abs_HAp, MARGIN = 1, STATS = ind, FUN = "*") 
  H1 = cbind(rep(1,n), S1)
  p1 = ncol(H1)
  H1A1 = sweep(H1, MARGIN = 1, STATS = A1, FUN = "*")
  Z1 = cbind(H1, H1A1)
  F1 = cv.ncvreg(Z1, y1_hat, penalty="SCAD")
  beta_psi1_ = as.numeric(coef(F1));
  beta_psi1 = beta_psi1_[-1]; beta10 = beta_psi1_[1]
  beta1 = beta_psi1[1:p1]; psi1 = beta_psi1[(p1+1):(2*p1)]
  return(list(psi1,psi2))
}

RQL_new <- function(S1,S2,A1,A2,y,Learners) 
{
  n = nrow(S1)
  cf = sample(1:n,n/2)
  
  X2 = cbind(S1,A1,S2,rep(1,n))
  g_hat1 = SuperLearner(Y = A2[cf], X = as.data.frame(X2[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  g_hat2 = SuperLearner(Y = A2[-cf], X = as.data.frame(X2[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  f_hat1 = SuperLearner(Y = y[cf], X = as.data.frame(X2[cf,]), family = binomial(), SL.library = Learners,method = "method.NNloglik")
  f_hat2 = SuperLearner(Y = y[-cf], X = as.data.frame(X2[-cf,]),family = binomial(), SL.library = Learners,method = "method.NNloglik")
  
  gg = A2
  gg[cf] = as.vector(predict(g_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  gg[-cf] = as.vector(predict(g_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
  ff = y
  ff[cf] = as.vector(predict(f_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  ff[-cf] = as.vector(predict(f_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
  impute_y = y-ff; impute_x = X2*(A2-gg);
  alpha_hat = as.vector(lm(impute_y ~ impute_x-1)$coefficients)
  
  g1 = as.vector(predict(g_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  g2 = as.vector(predict(g_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  f1 = as.vector(predict(f_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  f2 = as.vector(predict(f_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  impute_y1 = y[cf] - f1; impute_x1 = X2[cf,]*(A2[cf]-g1);
  impute_y2 = y[-cf] - f2; impute_x2 = X2[-cf,]*(A2[-cf]-g2);
  alpha_hat1 = as.vector(lm(impute_y1 ~ impute_x1-1)$coefficients)
  alpha_hat2 = as.vector(lm(impute_y2 ~ impute_x2-1)$coefficients)
  
  X1 = cbind(S1,rep(1,n))
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
  
  impute_y2 = y2-ff2; impute_x2 = X1*(A1-gg2);
  beta_hat = as.vector(lm(impute_y2 ~ impute_x2-1)$coefficients)
  
  return(list(alpha_hat,beta_hat))
}
