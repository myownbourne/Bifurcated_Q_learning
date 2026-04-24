CV_Lasso = function(X,y)
{
  if (length(unique(y)) <= 5) {
    beta_hat = rep(0, (ncol(X)+1))
    #beta_hat[1] = y[1]
    beta_hat[1] = as.numeric(names(which.max(table(y))))
  }else{
    fit_cv = cv.glmnet(x = X, y = y, alpha = 1)
    beta_hat = as.vector(coef(fit_cv, s = "lambda.min"))}
  return(beta_hat)
}

BQL_4 <- function(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s) 
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
  S2_b = cbind(S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2],S2[,J2s[[length(J2s)]]])
  X2_b = cbind(A1,S2_b)
  #g_hat1 = SuperLearner(Y = A2[cf], X = as.data.frame(X2_b[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #g_hat2 = SuperLearner(Y = A2[-cf], X = as.data.frame(X2_b[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #f_hat1 = SuperLearner(Y = y1[cf], X = as.data.frame(X2_b[cf,]), SL.library = Learners)
  #f_hat2 = SuperLearner(Y = y1[-cf], X = as.data.frame(X2_b[-cf,]), SL.library = Learners)
  g_hat1 = cv.glmnet(x = X2_b[cf,], y = A2[cf], alpha = 1, family = "binomial")
  g_hat2 = cv.glmnet(x = X2_b[-cf,], y = A2[-cf], alpha = 1, family = "binomial")
  f_hat1 = cv.glmnet(x = X2_b[cf,], y = y1[cf], alpha = 1)
  f_hat2 = cv.glmnet(x = X2_b[-cf,], y = y1[-cf], alpha = 1)
  
  gk = A2
  gk[cf] = as.vector(predict(g_hat2, newx = X2_b[cf,], s = "lambda.min", type = "response"))
  gk[-cf] = as.vector(predict(g_hat1, newx = X2_b[-cf,], s = "lambda.min", type = "response"))
  #gk[cf] = as.vector(predict(g_hat2, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  #gk[-cf] = as.vector(predict(g_hat1, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  
  fk = y1
  fk[cf] = as.vector(predict(f_hat2, newx = X2_b[cf,], s = "lambda.min"))
  fk[-cf] = as.vector(predict(f_hat1, newx = X2_b[-cf,], s = "lambda.min"))
  #fk[cf] = as.vector(predict(f_hat2, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  #fk[-cf] = as.vector(predict(f_hat1, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  
  impute_y = y1 - fk; impute_x = X2_b*(A2-gk);
  alpha_hat = CV_Lasso(impute_x, impute_y)
  #alpha_hat = as.vector(lm(impute_y ~ impute_x)$coefficients) #the first element is intercept
  #fit_cv = cv.glmnet(x = impute_x, y = impute_y, alpha = 1); alpha_hat = as.vector(coef(fit_cv, s = "lambda.min"))}
  alpha_hat[1] = alpha_hat[1] - (C2s[2]-C2s[1])
  
  gk1 = as.vector(predict(g_hat1, newx = X2_b[cf,], s = "lambda.min", type = "response"))
  gk2 = as.vector(predict(g_hat2, newx = X2_b[-cf,], s = "lambda.min", type = "response"))
  fk1 = as.vector(predict(f_hat1, newx = X2_b[cf,], s = "lambda.min"))
  fk2 = as.vector(predict(f_hat2, newx = X2_b[-cf,], s = "lambda.min"))
  #gk1 = as.vector(predict(g_hat1, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  #gk2 = as.vector(predict(g_hat2, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  #fk1 = as.vector(predict(f_hat1, as.data.frame(X2_b[cf,]), onlySL = TRUE)$pred)
  #fk2 = as.vector(predict(f_hat2, as.data.frame(X2_b[-cf,]), onlySL = TRUE)$pred)
  impute_y1 = y1[cf] - fk1; impute_x1 = X2_b[cf,]*(A2[cf]-gk1);
  impute_y2 = y1[-cf] - fk2; impute_x2 = X2_b[-cf,]*(A2[-cf]-gk2);
  alpha_hat1 = CV_Lasso(impute_x1, impute_y1)
  alpha_hat2 = CV_Lasso(impute_x2, impute_y2)
  #alpha_hat1 = as.vector(lm(impute_y1 ~ impute_x1)$coefficients) #the first element is intercept
  #alpha_hat2 = as.vector(lm(impute_y2 ~ impute_x2)$coefficients) #the first element is intercept
  #fit_cv1 = cv.glmnet(x = impute_x1, y = impute_y1, alpha = 1, standardize.response = FALSE); alpha_hat1 = as.vector(coef(fit_cv1, s = "lambda.min"))
  #fit_cv2 = cv.glmnet(x = impute_x2, y = impute_y2, alpha = 1, standardize.response = FALSE); alpha_hat2 = as.vector(coef(fit_cv2, s = "lambda.min"))
  alpha_hat1[1] = alpha_hat1[1] - (C2s[2]-C2s[1])
  alpha_hat2[1] = alpha_hat2[1] - (C2s[2]-C2s[1])
  
  for(k1 in 1:length(J1s)){
    alphas_k1 = list()
    for(k2 in 1:length(J2s)){
      alphas_k1k2 = list()
      for(a1 in 1:2){
        ta = a1 - 1; 
        aS2_b = cbind(ta*rep(1,n),S2_b);
        SJ2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[k2]]]);
        
        impute_yJ2 = cbind(1,aS2_b)%*%alpha_hat;
        impute_yJ21 = cbind(1,aS2_b[cf,])%*%alpha_hat1;
        impute_yJ22 = cbind(1,aS2_b[-cf,])%*%alpha_hat2;
        impute_xJ21 = SJ2_b[cf,];
        impute_xJ22 = SJ2_b[-cf,];
        
        alpha_hat_a1k1k2 = CV_Lasso(SJ2_b, impute_yJ2)
        alpha_hat_a1k1k21 = CV_Lasso(impute_xJ21, impute_yJ21)
        alpha_hat_a1k1k22 = CV_Lasso(impute_xJ22, impute_yJ22)
        
        #fit_cv = cv.glmnet(x = SJ2_b, y = impute_yJ2, alpha = 1, standardize.response = FALSE)
        #alpha_hat_a1k1k2 = as.vector(coef(fit_cv, s = "lambda.min"))
        #fit_cv1 = cv.glmnet(x = impute_xJ21, y = impute_yJ21, alpha = 1, standardize.response = FALSE)
        #alpha_hat_a1k1k21 = as.vector(coef(fit_cv1, s = "lambda.min"))
        #fit_cv2 = cv.glmnet(x = impute_xJ22, y = impute_yJ22, alpha = 1, standardize.response = FALSE) 
        #alpha_hat_a1k1k22 = as.vector(coef(fit_cv2, s = "lambda.min"))
        
        #alpha_hat_a1k1k2 = as.vector(lm(impute_yJ2 ~ SJ2_b)$coefficients) #the first elemet is intercept
        #alpha_hat_a1k1k21 = as.vector(lm(impute_yJ21 ~ impute_xJ21)$coefficients) #the first elemet is intercept
        #alpha_hat_a1k1k22 = as.vector(lm(impute_yJ22 ~ impute_xJ22)$coefficients) #the first elemet is intercept
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
        XL2f_b = cbind(A1,S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2]);
        beta_tilde_k1f = rep(0,(1+ncol(XL2f_b)))
        beta_tildes_k1[[k2]] = list(beta_tilde_k1k2 = beta_tilde_k1f, beta_tilde_k1k21 = beta_tilde_k1f, beta_tilde_k1k22 = beta_tilde_k1f)
        for(a1 in 1:2){
          SL2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2]);
          beta_hat_a1k1f = rep(0,(1+ncol(SL2_b)))
          betas_k1k2[[a1]] = list(beta_hat_a1k1k2 = beta_hat_a1k1f, beta_hat_a1k1k21 = beta_hat_a1k1f, beta_hat_a1k1k22 = beta_hat_a1k1f)
        }
        betas_k1[[k2]] = betas_k1k2;
      }else{
        XL2f_b = cbind(A1,S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2]);
        X2_balpha = cbind(1,X2_b)%*%alpha_hat; X2_balpha1 = cbind(1,X2_b[cf,])%*%alpha_hat1; X2_balpha2 = cbind(1,X2_b[-cf,])%*%alpha_hat2;
        
        SJ2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[k2]]])
        alpha_hat_0k1k2 = alphas[[k1]][[k2]][[1]]$alpha_hat_a1k1k2
        alpha_hat_0k1k21 = alphas[[k1]][[k2]][[1]]$alpha_hat_a1k1k21 
        alpha_hat_0k1k22 = alphas[[k1]][[k2]][[1]]$alpha_hat_a1k1k22 
        alpha_hat_1k1k2 = alphas[[k1]][[k2]][[2]]$alpha_hat_a1k1k2
        alpha_hat_1k1k21 = alphas[[k1]][[k2]][[2]]$alpha_hat_a1k1k21 
        alpha_hat_1k1k22 = alphas[[k1]][[k2]][[2]]$alpha_hat_a1k1k22 
        SJ2_b_alpha = (A1 == 0) * (cbind(1,SJ2_b) %*% alpha_hat_0k1k2) + (A1 == 1) * (cbind(1,SJ2_b) %*% alpha_hat_1k1k2)
        SJ2_b_alpha1 = (A1[cf] == 0) * (cbind(1,SJ2_b[cf,]) %*% alpha_hat_0k1k21) + (A1[cf] == 1) * (cbind(1,SJ2_b[cf,]) %*% alpha_hat_1k1k21)
        SJ2_b_alpha2 = (A1[-cf] == 0) * (cbind(1,SJ2_b[-cf,]) %*% alpha_hat_0k1k22) + (A1[-cf] == 1) * (cbind(1,SJ2_b[-cf,]) %*% alpha_hat_1k1k22)
        
        
        SJ2f_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[length(J2s)]]])
        alpha_hat_0k1f = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k2
        alpha_hat_0k1f1 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k21 
        alpha_hat_0k1f2 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k22 
        alpha_hat_1k1f = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k2
        alpha_hat_1k1f1 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k21 
        alpha_hat_1k1f2 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k22 
        SJ2f_b_alpha = (A1 == 0) * (cbind(1,SJ2f_b) %*% alpha_hat_0k1f) + (A1 == 1) * (cbind(1,SJ2f_b) %*% alpha_hat_1k1f)
        SJ2f_b_alpha1 = (A1[cf] == 0) * (cbind(1,SJ2f_b[cf,]) %*% alpha_hat_0k1f1) + (A1[cf] == 1) * (cbind(1,SJ2f_b[cf,]) %*% alpha_hat_1k1f1)
        SJ2f_b_alpha2 = (A1[-cf] == 0) * (cbind(1,SJ2f_b[-cf,]) %*% alpha_hat_0k1f2) + (A1[-cf] == 1) * (cbind(1,SJ2f_b[-cf,]) %*% alpha_hat_1k1f2)
        
        impute_y = (X2_balpha*(SJ2_b_alpha>0) - CJ2s[k2]) - (X2_balpha*(SJ2f_b_alpha>0) - CJ2s[length(CJ2s)])
        beta_tilde_k1k2 = CV_Lasso(XL2f_b, impute_y)
        #fit_cv = cv.glmnet(x = XL2f_b, y = impute_y, alpha = 1, standardize.response = FALSE); beta_tilde_k1k2 = as.vector(coef(fit_cv, s = "lambda.min"))
        #beta_tilde_k1k2 = as.vector(lm(impute_y ~ XL2f_b)$coefficients) #the first element is intercept
        
        impute_y1 = (X2_balpha1*(SJ2_b_alpha1>0) - CJ2s[k2]) - (X2_balpha1*(SJ2f_b_alpha1>0) - CJ2s[length(CJ2s)])
        beta_tilde_k1k21 = CV_Lasso(XL2f_b[cf,], impute_y1)
        #fit_cv1 = cv.glmnet(x = XL2f_b[cf,], y = impute_y1, alpha = 1, standardize.response = FALSE); beta_tilde_k1k21 = as.vector(coef(fit_cv1, s = "lambda.min"))
        #beta_tilde_k1k21 = as.vector(lm(impute_y1 ~ XL2f_b[cf,])$coefficients) #the first element is intercept
        
        impute_y2 = (X2_balpha2*(SJ2_b_alpha2>0) - CJ2s[k2]) - (X2_balpha2*(SJ2f_b_alpha2>0) - CJ2s[length(CJ2s)])
        beta_tilde_k1k22 = CV_Lasso(XL2f_b[-cf,], impute_y2)
        #fit_cv2 = cv.glmnet(x = XL2f_b[-cf,], y = impute_y2, alpha = 1, standardize.response = FALSE); beta_tilde_k1k22 = as.vector(coef(fit_cv2, s = "lambda.min"))
        #beta_tilde_k1k22 = as.vector(lm(impute_y2 ~ XL2f_b[-cf,])$coefficients) #the first element is intercept
        beta_tildes_k1[[k2]] = list(beta_tilde_k1k2 = beta_tilde_k1k2, beta_tilde_k1k21 = beta_tilde_k1k21, beta_tilde_k1k22 = beta_tilde_k1k22)
        
        for(a1 in 1:2){
          ta = a1 - 1; 
          aSL2f_b = cbind(ta*rep(1,n),S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2]);
          SL2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2]);
          
          impute_yJ1 = cbind(1,aSL2f_b)%*%beta_tilde_k1k2;
          impute_yJ11 = cbind(1,aSL2f_b[cf,])%*%beta_tilde_k1k21;
          impute_yJ12 = cbind(1,aSL2f_b[-cf,])%*%beta_tilde_k1k22;
          
          beta_hat_a1k1k2 = CV_Lasso(SL2_b, impute_yJ1)
          beta_hat_a1k1k21 = CV_Lasso(SL2_b[cf,], impute_yJ11)
          beta_hat_a1k1k22 = CV_Lasso(SL2_b[-cf,], impute_yJ12)
          #fit_cv = cv.glmnet(x = SL2_b, y = impute_yJ1, alpha = 1, standardize.response = FALSE); beta_hat_a1k1k2 = as.vector(coef(fit_cv, s = "lambda.min"))
          #fit_cv1 = cv.glmnet(x = SL2_b[cf,], y = impute_yJ11, alpha = 1, standardize.response = FALSE); beta_hat_a1k1k21 = as.vector(coef(fit_cv1, s = "lambda.min"))
          #fit_cv2 = cv.glmnet(x = SL2_b[-cf,], y = impute_yJ12, alpha = 1, standardize.response = FALSE); beta_hat_a1k1k22 = as.vector(coef(fit_cv2, s = "lambda.min"))
          #beta_hat_a1k1k2 = as.vector(lm(impute_yJ1 ~ SL2_b)$coefficients) #the first element is intercept
          #beta_hat_a1k1k21 = as.vector(lm(impute_yJ11 ~ SL2_b[cf,])$coefficients) #the first element is intercept
          #beta_hat_a1k1k22 = as.vector(lm(impute_yJ12 ~ SL2_b[-cf,])$coefficients) #the first element is intercept
          
          betas_k1k2[[a1]] = list(beta_hat_a1k1k2 = beta_hat_a1k1k2, beta_hat_a1k1k21 = beta_hat_a1k1k21, beta_hat_a1k1k22 = beta_hat_a1k1k22)
        }
        betas_k1[[k2]] = betas_k1k2;
      }
    }
    betas[[k1]] = betas_k1;
    beta_tildes[[k1]] = beta_tildes_k1;
  }
  
  #Step 3
  X2_b_alpha = cbind(1,X2_b)%*%alpha_hat; X2_b_alpha1 = cbind(1,X2_b[cf,])%*%alpha_hat1; X2_b_alpha2 = cbind(1,X2_b[-cf,])%*%alpha_hat2; 
  SJ1f_b = cbind(S1[,L1],S1[,J1s[[length(J1s)]]]);
  g_hat31 = cv.glmnet(x = SJ1f_b[cf,], y = A1[cf], alpha = 1, family = "binomial")
  g_hat32 = cv.glmnet(x = SJ1f_b[-cf,], y = A1[-cf], alpha = 1, family = "binomial")
  #g_hat31 = SuperLearner(Y = A1[cf], X = as.data.frame(SJ1f_b[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #g_hat32 = SuperLearner(Y = A1[-cf], X = as.data.frame(SJ1f_b[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  for(k1 in 1:length(J1s)){
    SJ1_b = cbind(S1[,L1],S1[,J1s[[k1]]]);
    SJ2f_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2],S2[,J2s[[length(J2s)]]]);
    alpha_hat_0k1f = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k2
    alpha_hat_0k1f1 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k21 
    alpha_hat_0k1f2 = alphas[[k1]][[length(J2s)]][[1]]$alpha_hat_a1k1k22
    
    alpha_hat_1k1f = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k2
    alpha_hat_1k1f1 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k21 
    alpha_hat_1k1f2 = alphas[[k1]][[length(J2s)]][[2]]$alpha_hat_a1k1k22
    
    SJ2f_b_alpha = (A1 == 0) * (cbind(1,SJ2f_b) %*% alpha_hat_0k1f) + (A1 == 1) * (cbind(1,SJ2f_b) %*% alpha_hat_1k1f)
    SJ2f_b_alpha1 = (A1[cf] == 0) * (cbind(1,SJ2f_b[cf,]) %*% alpha_hat_0k1f1) + (A1[cf] == 1) * (cbind(1,SJ2f_b[cf,]) %*% alpha_hat_1k1f1)
    SJ2f_b_alpha2 = (A1[-cf] == 0) * (cbind(1,SJ2f_b[-cf,]) %*% alpha_hat_0k1f2) + (A1[-cf] == 1) * (cbind(1,SJ2f_b[-cf,]) %*% alpha_hat_1k1f2)
    
    SL2_b = cbind(S1[,L1],S1[,J1s[[k1]]],S2[,L2])
    XL2f_b = cbind(A1,S1[,L1],S1[,J1s[[length(J1s)]]],S2[,L2])
    
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
      
      list_Sbeta[[k2]] = (A1 == 0) * (cbind(1,SL2_b)%*%beta_hat_0k1k2) + (A1 == 1) * (cbind(1,SL2_b)%*%beta_hat_1k1k2)
      list_Sbeta1[[k2]] = (A1[cf] == 0) * (cbind(1,SL2_b[cf,])%*%beta_hat_0k1k21) + (A1[cf] == 1) * (cbind(1,SL2_b[cf,])%*%beta_hat_1k1k21)
      list_Sbeta2[[k2]] = (A1[-cf] == 0) * (cbind(1,SL2_b[-cf,])%*%beta_hat_0k1k22) + (A1[-cf] == 1) * (cbind(1,SL2_b[-cf,])%*%beta_hat_1k1k22)
      
      list_Xfbeta[[k2]] = cbind(1,XL2f_b)%*%beta_tilde_k1k2
      list_Xfbeta1[[k2]] = cbind(1,XL2f_b[cf,])%*%beta_tilde_k1k21
      list_Xfbeta2[[k2]] = cbind(1,XL2f_b[-cf,])%*%beta_tilde_k1k22
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
    
    f_hat31 = cv.glmnet(x = SJ1f_b[cf,], y = y31, alpha = 1)
    f_hat32 = cv.glmnet(x = SJ1f_b[-cf,], y = y32, alpha = 1)
    #f_hat31 = SuperLearner(Y = y31, X = as.data.frame(SJ1f_b[cf,]), SL.library = Learners)
    #f_hat32 = SuperLearner(Y = y32, X = as.data.frame(SJ1f_b[-cf,]), SL.library = Learners)
    
    g3_tilde = A1
    g3_tilde[cf] = as.vector(predict(g_hat32, newx = SJ1f_b[cf,], s = "lambda.min", type = "response"))
    g3_tilde[-cf] = as.vector(predict(g_hat31, newx = SJ1f_b[-cf,], s = "lambda.min", type = "response"))
    #g3_tilde[cf] = as.vector(predict(g_hat32, as.data.frame(SJ1f_b[cf,]), onlySL = TRUE)$pred)
    #g3_tilde[-cf] = as.vector(predict(g_hat31, as.data.frame(SJ1f_b[-cf,]), onlySL = TRUE)$pred)
    
    f3_tilde = y3
    f3_tilde[cf] = as.vector(predict(f_hat32, newx = SJ1f_b[cf,], s = "lambda.min"))
    f3_tilde[-cf] = as.vector(predict(f_hat31, newx = SJ1f_b[-cf,], s = "lambda.min"))
    #f3_tilde[cf] = as.vector(predict(f_hat32, as.data.frame(SJ1f_b[cf,]), onlySL = TRUE)$pred)
    #f3_tilde[-cf] = as.vector(predict(f_hat31, as.data.frame(SJ1f_b[-cf,]), onlySL = TRUE)$pred)
    
    impute_y3_tilde = y3-f3_tilde; impute_x3_tilde = SJ1f_b*(A1-g3_tilde);
    gamma_tilde_k1 = CV_Lasso(impute_x3_tilde, impute_y3_tilde)
    #fit_cv = cv.glmnet(x = impute_x3_tilde, y = impute_y3_tilde, alpha = 1, standardize.response = FALSE); gamma_tilde_k1 = as.vector(coef(fit_cv, s = "lambda.min"))
    #gamma_tilde_k1 = as.vector(lm(impute_y3_tilde ~ impute_x3_tilde)$coefficients) # the first element is intercept
    gamma_tilde_k1[1] = gamma_tilde_k1[1] - (C1s[2]-C1s[1])
    gamma_tildes[[k1]] = list(gamma_tilde_k1 = gamma_tilde_k1)
    
    impute_y3 = cbind(1,SJ1f_b)%*%gamma_tilde_k1;
    gamma_hat_k1 = CV_Lasso(SJ1_b, impute_y3)
    #fit_cv = cv.glmnet(x = SJ1_b, y = impute_y3, alpha = 1, standardize.response = FALSE); gamma_hat_k1 = as.vector(coef(fit_cv, s = "lambda.min"))
    #gamma_hat_k1 = as.vector(lm(impute_y3 ~ SJ1_b)$coefficients) #the first element is intercept
    gammas[[k1]] = list(gamma_hat_k1 = gamma_hat_k1)
  }
  
  
  #Step 4
  S1b = cbind(S1[,L1],S1[,J1s[[length(J1s)]]])
  SL1 = cbind(S1[,L1])
  
  if(length(J1s)!=1){
    for(k1 in 1:(length(J1s)-1)){
      SJ1_b = cbind(S1[,L1],S1[,J1s[[k1]]])
      
      gamma_hat_f = gammas[[length(J1s)]]$gamma_hat_k1
      gamma_hat_k1 = gammas[[k1]]$gamma_hat_k1
      gamma_tilde_k1 = gamma_tildes[[k1]]$gamma_tilde_k1
      gamma_tilde_f = gamma_tildes[[length(J1s)]]$gamma_tilde_k1
      
      SJ1f_gamma_tilde = cbind(1,S1b)%*%gamma_tilde_f
      SJ1_gamma_tilde = cbind(1,S1b)%*%gamma_tilde_k1
      SJ1f_gamma = cbind(1,S1b)%*%gamma_hat_f
      SJ1_gamma = cbind(1,SJ1_b)%*%gamma_hat_k1
      
      y_J1 = y_J1s[[k1]] - ifelse(A1 == 1, C1s[2], C1s[1]) 
      y_J1f = y_J1s[[length(J1s)]] - ifelse(A1 == 1, C1s[2], C1s[1]) 
      
      impute_yJ1 = y_J1 + SJ1_gamma_tilde*((SJ1_gamma>0) - A1) - CJ1s[k1]
      impute_yJ1f = y_J1f + SJ1f_gamma_tilde*((SJ1f_gamma>0) - A1) - CJ1s[length(CJ1s)]
      
      impute_y = impute_yJ1 - impute_yJ1f
      delta_hat_k1 = CV_Lasso(SL1, impute_y)
      #fit_cv = cv.glmnet(x = SL1, y = impute_y, alpha = 1, standardize.response = FALSE); delta_hat_k1 = as.vector(coef(fit_cv, s = "lambda.min"))
      #delta_hat_k1 = as.vector(lm(impute_y ~ SL1)$coefficients) #the first element is intercept
      deltas[[k1]] = list(delta_hat_k1 = delta_hat_k1)
    }
  }
  
  delta_f = rep(0,(1+ncol(SL1)))
  deltas[[length(J1s)]] = list(delta_hat_k1 = delta_f)
  
  return(list(alphas,betas,gammas,deltas))
}


BQL_regimes_4 <- function(alphas,betas,gammas,deltas,S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms)
{
  n_test = nrow(S1_new)
  SL1 = as.matrix(S1_new[,L1])
  J1_hat = rep(0,n_test)
  a1_hat = rep(0,n_test)
  J2_hat = rep(0,n_test)
  a2_hat = rep(0,n_test)
  y_hat = rep(0,n_test)
  
  for(i in 1:n_test){
    SL1i = SL1[i,]
    SL1_delta = rep(0,length(J1s))
    for(k1 in 1:length(J1s)){
      delta_hat_k1 = deltas[[k1]]$delta_hat_k1
      SL1_delta[k1] = c(1,SL1i)%*%delta_hat_k1
    }
    J1_hati = which.max(SL1_delta)
    J1_hat[i] = J1_hati
    
    SJ1i = S1_new[i,J1s[[J1_hati]]]
    SJ1ib = c(1,SL1i,SJ1i)
    gamma_hat_k1 = gammas[[J1_hati]]$gamma_hat_k1
    a1_hat[i] = (SJ1ib%*%gamma_hat_k1)>0
  }
  
  S2_new = get_S2(S1_new,a1_hat)
  SL2 = as.matrix(S2_new[,L2])
  for(i in 1:n_test){
    J1_hati = J1_hat[i]
    SL2ib = c(SL1[i,],S1_new[i,J1s[[J1_hati]]],SL2[i,])
    SL2b_beta = rep(0,length(J2s))
    for(k2 in 1:length(J2s)){
      beta_hat_0k1k2 = betas[[J1_hati]][[k2]][[1]]$beta_hat_a1k1k2
      beta_hat_1k1k2 = betas[[J1_hati]][[k2]][[2]]$beta_hat_a1k1k2
      if(a1_hat[i]==0){
        SL2b_beta[k2] = c(1,SL2ib)%*%beta_hat_0k1k2
      }else{
        SL2b_beta[k2] = c(1,SL2ib)%*%beta_hat_1k1k2
      }
    }
    J2_hati = which.max(SL2b_beta)
    J2_hat[i] = J2_hati
    
    SJ2i = S2_new[i,J2s[[J2_hati]]]
    SJ2ib = c(1,SL2ib,SJ2i)
    if(a1_hat[i]==0){
      alpha_hat_a1k1k2 = alphas[[J1_hati]][[J2_hati]][[1]]$alpha_hat_a1k1k2
    }else{
      alpha_hat_a1k1k2 = alphas[[J1_hati]][[J2_hati]][[2]]$alpha_hat_a1k1k2
    }
    a2_hat[i] = (SJ2ib%*%alpha_hat_a1k1k2)>0
  }
  
  S_new = cbind(S1_new,a1_hat,S2_new)
  beta_y1 = parms[[1]]; beta_y2 = parms[[2]]; beta_y1A = parms[[3]]; beta_y2A = parms[[4]];
  y_new = S1_new%*%beta_y1 + S_new%*%beta_y2 + a1_hat*S1_new%*%beta_y1A + a2_hat*S_new%*%beta_y2A
  
  return(list(J1_hat,a1_hat,J2_hat,a2_hat,y_new))
}

CV_GLasso = function(X, y, group_jj, cost_jj) {
  if (all(cost_jj <= 0)) {
    #beta_hat = as.vector(lm(y ~ X)$coef)
    fit_cv = cv.glmnet(x = X, y = y, alpha = 1); beta_hat = as.vector(coef(fit_cv, s = "lambda.min"))
    return(beta_hat)
  }
  
  sorted_idx <- order(group_jj)
  X_sorted <- X[, sorted_idx]
  group_sorted <- group_jj[sorted_idx]

  fit0 <- cv.sparsegl(X_sorted,y,group=group_sorted,asparse=0.2,nlambda=50,family="gaussian",pf_group=cost_jj,standardize=FALSE,nfolds=5,lambda.factor=0.01)
  fit <- cv.sparsegl(X_sorted,y,group=group_sorted,asparse = 0.2,family="gaussian",pf_group=cost_jj,standardize=FALSE,lambda=fit0$lambda,nfolds=5,lambda.factor=0.01)
 
  #fit0 <- cv.sparsegl(X_sorted, y, group = group_sorted, asparse = 0.2, family = "gaussian", pf_group = cost_jj, standardize = FALSE)
  #fit <- cv.sparsegl(X_sorted, y, group = group_sorted, asparse = 0.2, family = "gaussian", pf_group = cost_jj, standardize = FALSE, lambda = fit0$lambda)
  
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
  
  best_beta_sorted <- coef_mat[, best_idx]
  best_beta <- best_beta_sorted[c(1, order(sorted_idx) + 1)]
  return(as.vector(best_beta))
}

HDQ_glasso = function(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
{
  #stage 2
  A1[A1==0] = -1; A2[A2==0] = -1;
  n = nrow(S1); d1 = ncol(S1); d2 = ncol(S2)
  
  H2 = cbind(S1, A1, S2)
  p2 = ncol(H2)
  H2A2 = sweep(H2, 1, A2, "*")
  Z2 = cbind(H2, A2, H2A2)
  
  groupZ2 = rep(1, (2*p2+1))
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
  beta20 = beta_psi2[1]; beta2 = beta_psi2[2:(1+p2)]
  psi2 = beta_psi2[(p2+2):(length(beta_psi2))] #the first element is intercept
  
  #stage 1
  rho2n = n^(-2/5)
  abs_HAp = abs(cbind(A2,H2A2)%*%psi2); ind = abs_HAp>rho2n
  y1_hat = beta20 + H2%*%beta2 + sweep(abs_HAp, MARGIN = 1, STATS = ind, FUN = "*") 
  H1 = S1
  p1 = ncol(H1)
  H1A1 = sweep(H1, MARGIN = 1, STATS = A1, FUN = "*")
  Z1 = cbind(H1, A1, H1A1)
  
  groupZ1 = rep(1, (2*p1+1))
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
  beta10 = beta_psi1[1]; beta1 = beta_psi1[2:(p1+1)]
  psi1 = beta_psi1[(p1+2):(length(beta_psi1))]
  return(list(psi1,psi2))
}

HDQ_glasso_regimes = function(psi1,psi2,S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
{
  n_test = nrow(S1_new); d1 = ncol(S1_new);
  H1_new = cbind(1, S1_new)
  a1_hat = as.vector(ifelse((H1_new%*%psi1) > ((C1s[2]-C1s[1])/2), 1, -1)); 
  a1_hat_ = a1_hat; a1_hat_[a1_hat_==-1] = 0;
  S2_new = get_S2(S1_new,a1_hat_)
  S_new = cbind(S1_new,a1_hat,S2_new); H2_new = cbind(1,S_new); S_new2 = cbind(S1_new,a1_hat_,S2_new);
  a2_hat = as.vector(ifelse((H2_new%*%psi2) > ((C2s[2]-C2s[1])/2), 1, -1)); 
  a2_hat_ = a2_hat; a2_hat_[a2_hat_==-1] = 0;
  
  beta_y1 = parms[[1]]; beta_y2 = parms[[2]]; beta_y1A = parms[[3]]; beta_y2A = parms[[4]];
  y_test = S1_new%*%beta_y1 + S_new2%*%beta_y2 + a1_hat_*S1_new%*%beta_y1A + a2_hat_*S_new2%*%beta_y2A
  
  psi21 = psi2[2:(d1+1)]
  psi11 = psi1[2:(d1+1)]
  #psi1_nz = union(which(psi21!=0), which(psi11!=0))
  psi1_nz = union(which(abs(psi21)>1e-6), which(abs(psi11)>1e-6))
  J_psi1 = c()
  
  for(k1 in 1:(length(J1s))){
    JJ1 = c(L1,J1s[[k1]])
    if(all(is.element(psi1_nz, JJ1))){
      J_psi1 = append(J_psi1,k1)
    }
  }
  
  psi22 = psi2[(d1+3):length(psi2)]
  #psi22_nz = which(psi22 != 0)
  psi22_nz = which(abs(psi22)>1e-6)
  J_psi22 = c()
  
  for(k2 in 1:(length(J2s))){
    JJ2 = c(L2,J2s[[k2]])
    if(all(is.element(psi22_nz, JJ2))){
      J_psi22 = append(J_psi22,k2)
    }
  }
  
  return(list(a1_hat_, a2_hat_, y_test, J_psi22, J_psi1))
}

RQL_glasso = function(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
{
  n = nrow(S1); d1 = ncol(S1); cf = sample(1:n,n/2)
  
  X2 = cbind(S1,A1,S2)
  g_hat1 = cv.glmnet(x = X2[cf,], y = A2[cf], alpha = 1, family = "binomial")
  g_hat2 = cv.glmnet(x = X2[-cf,], y = A2[-cf], alpha = 1, family = "binomial")
  f_hat1 = cv.glmnet(x = X2[cf,], y = y[cf], alpha = 1)
  f_hat2 = cv.glmnet(x = X2[-cf,], y = y[-cf], alpha = 1)
  #g_hat1 = SuperLearner(Y = A2[cf], X = as.data.frame(X2[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #g_hat2 = SuperLearner(Y = A2[-cf], X = as.data.frame(X2[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #f_hat1 = SuperLearner(Y = y[cf], X = as.data.frame(X2[cf,]), SL.library = Learners)
  #f_hat2 = SuperLearner(Y = y[-cf], X = as.data.frame(X2[-cf,]), SL.library = Learners)
  
  gg = A2
  gg[cf] = as.vector(predict(g_hat2, newx = X2[cf,], s = "lambda.min", type = "response"))
  gg[-cf] = as.vector(predict(g_hat1, newx = X2[-cf,], s = "lambda.min", type = "response"))
  #gg[cf] = as.vector(predict(g_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #gg[-cf] = as.vector(predict(g_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
  ff = y
  ff[cf] = as.vector(predict(f_hat2, newx = X2[cf,], s = "lambda.min"))
  ff[-cf] = as.vector(predict(f_hat1, newx = X2[-cf,], s = "lambda.min"))
  #ff[cf] = as.vector(predict(f_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #ff[-cf] = as.vector(predict(f_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
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
  
  g1 = as.vector(predict(g_hat1, newx = X2[cf,], s = "lambda.min", type = "response"))
  g2 = as.vector(predict(g_hat2, newx = X2[-cf,], s = "lambda.min", type = "response"))
  f1 = as.vector(predict(f_hat1, newx = X2[cf,], s = "lambda.min"))
  f2 = as.vector(predict(f_hat2, newx = X2[-cf,], s = "lambda.min"))
  #g1 = as.vector(predict(g_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #g2 = as.vector(predict(g_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  #f1 = as.vector(predict(f_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #f2 = as.vector(predict(f_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  impute_y1 = y[cf] - f1; impute_x1 = X2[cf,]*(A2[cf]-g1);
  impute_y2 = y[-cf] - f2; impute_x2 = X2[-cf,]*(A2[-cf]-g2);
  alpha_hat1 = CV_GLasso(impute_x1,impute_y1,groupX2,c_g2)
  alpha_hat2 = CV_GLasso(impute_x2,impute_y2,groupX2,c_g2)
  
  X1 = cbind(S1)
  X2_alpha = cbind(1,X2)%*%alpha_hat; y2 = y + X2_alpha*((X2_alpha>0)-A2)
  X2_alpha1 = cbind(1,X2[cf,])%*%alpha_hat1; y21 = y[cf] + X2_alpha1*((X2_alpha1>0)-A2[cf]) 
  X2_alpha2 = cbind(1,X2[-cf,])%*%alpha_hat2; y22 = y[-cf] + X2_alpha2*((X2_alpha2>0)-A2[-cf])
  
  g21 = cv.glmnet(x = X1[cf,], y = A1[cf], alpha = 1, family = "binomial")
  g22 = cv.glmnet(x = X1[-cf,], y = A1[-cf], alpha = 1, family = "binomial")
  f21 = cv.glmnet(x = X1[cf,], y = y21, alpha = 1)
  f22 = cv.glmnet(x = X1[-cf,], y = y22, alpha = 1)
  #g21 = SuperLearner(Y = A1[cf], X = as.data.frame(X1[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #g22 = SuperLearner(Y = A1[-cf], X = as.data.frame(X1[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #f21 = SuperLearner(Y = y21, X = as.data.frame(X1[cf,]), SL.library = Learners)
  #f22 = SuperLearner(Y = y22, X = as.data.frame(X1[-cf,]), SL.library = Learners)
  
  gg2 = A1
  gg2[cf] = as.vector(predict(g22, newx = X1[cf,], s = "lambda.min", type = "response"))
  gg2[-cf] = as.vector(predict(g21, newx = X1[-cf,], s = "lambda.min", type = "response"))
  #gg2[cf] = as.vector(predict(g22, as.data.frame(X1[cf,]), onlySL = TRUE)$pred)
  #gg2[-cf] = as.vector(predict(g21, as.data.frame(X1[-cf,]), onlySL = TRUE)$pred)
  
  ff2 = y2
  ff2[cf] = as.vector(predict(f22, newx = X1[cf,], s = "lambda.min"))
  ff2[-cf] = as.vector(predict(f21, newx = X1[-cf,], s = "lambda.min"))
  #ff2[cf] = as.vector(predict(f22, as.data.frame(X1[cf,]), onlySL = TRUE)$pred)
  #ff2[-cf] = as.vector(predict(f21, as.data.frame(X1[-cf,]), onlySL = TRUE)$pred)
  
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

RQL_gLasso_regimes <- function(alpha_hat,beta_hat,S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
{
  n_test = nrow(S1_new); d1 = ncol(S1_new); 
  X1_new = cbind(1,S1_new)
  a1_hat = as.vector(ifelse((X1_new%*%beta_hat) > (C1s[2]-C1s[1]), 1, 0)); 
  S2_new = get_S2(S1_new,a1_hat); d2 = ncol(S2_new);
  
  X2_new = cbind(1,S1_new,a1_hat,S2_new)
  a2_hat = as.vector(ifelse((X2_new%*%alpha_hat) > (C2s[2]-C2s[1]), 1, 0)); 
  
  S_new = cbind(S1_new,a1_hat,S2_new)
  beta_y1 = parms[[1]]; beta_y2 = parms[[2]]; beta_y1A = parms[[3]]; beta_y2A = parms[[4]];
  y_new = S1_new%*%beta_y1 + S_new%*%beta_y2 + a1_hat*S1_new%*%beta_y1A + a2_hat*S_new%*%beta_y2A
  
  alpha_hat1 = alpha_hat[2:(d1+1)]
  beta_hat1 = beta_hat[2:(d1+1)]
  ab_nz = union(which(abs(alpha_hat1)>1e-6), which(abs(beta_hat1)>1e-6))
  J_ab1 = c()
  
  for(k1 in 1:(length(J1s))){
    JJ1 = c(L1,J1s[[k1]])
    if(all(is.element(ab_nz,JJ1))){
      J_ab1 = append(J_ab1,k1)
    }
  }
  
  alpha_hat2 = alpha_hat[(d1+3):(d1+2+d2)]
  a2_nz = which(abs(alpha_hat2)>1e-6)
  J_a2 = c()
  
  for(k2 in 1:(length(J2s))){
    JJ2 = c(L2,J2s[[k2]])
    if(all(is.element(a2_nz,JJ2))){
      J_a2 = append(J_a2,k2)
    }
  }
  return(list(a1_hat,a2_hat,y_new,J_ab1,J_a2))
}

RQLo <- function(S1,S2,A1,A2,y) 
{
  n = nrow(S1)
  cf = sample(1:n,n/2)
  
  X2 = cbind(S1,A1,S2,rep(1,n))
  g_hat1 = cv.glmnet(x = X2[cf,], y = A2[cf], alpha = 1, family = "binomial", intercept = FALSE)
  g_hat2 = cv.glmnet(x = X2[-cf,], y = A2[-cf], alpha = 1, family = "binomial", intercept = FALSE)
  f_hat1 = cv.glmnet(x = X2[cf,], y = y[cf], alpha = 1, intercept = FALSE)
  f_hat2 = cv.glmnet(x = X2[-cf,], y = y[-cf], alpha = 1, intercept = FALSE)
  #g_hat1 = SuperLearner(Y = A2[cf], X = as.data.frame(X2[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #g_hat2 = SuperLearner(Y = A2[-cf], X = as.data.frame(X2[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #f_hat1 = SuperLearner(Y = y[cf], X = as.data.frame(X2[cf,]), SL.library = Learners)
  #f_hat2 = SuperLearner(Y = y[-cf], X = as.data.frame(X2[-cf,]), SL.library = Learners)
  
  gg = A2
  gg[cf] = as.vector(predict(g_hat2, newx = X2[cf,], s = "lambda.min", type = "response"))
  gg[-cf] = as.vector(predict(g_hat1, newx = X2[-cf,], s = "lambda.min", type = "response"))
  #gg[cf] = as.vector(predict(g_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #gg[-cf] = as.vector(predict(g_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
  ff = y
  ff[cf] = as.vector(predict(f_hat2, newx = X2[cf,], s = "lambda.min"))
  ff[-cf] = as.vector(predict(f_hat1, newx = X2[-cf,], s = "lambda.min"))
  #ff[cf] = as.vector(predict(f_hat2, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #ff[-cf] = as.vector(predict(f_hat1, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  
  impute_y = y-ff; impute_x = X2*(A2-gg);
  #alpha_hat = as.vector(lm(impute_y ~ impute_x-1)$coefficients)
  fit_cv = cv.glmnet(x = impute_x, y = impute_y, alpha = 1, intercept = FALSE); alpha_hat = as.vector(coef(fit_cv, s = "lambda.min"))[-1]
  
  g1 = as.vector(predict(g_hat1, newx = X2[cf,], s = "lambda.min", type = "response"))
  g2 = as.vector(predict(g_hat2, newx = X2[-cf,], s = "lambda.min", type = "response"))
  f1 = as.vector(predict(f_hat1, newx = X2[cf,], s = "lambda.min"))
  f2 = as.vector(predict(f_hat2, newx = X2[-cf,], s = "lambda.min"))
  #g1 = as.vector(predict(g_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #g2 = as.vector(predict(g_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  #f1 = as.vector(predict(f_hat1, as.data.frame(X2[cf,]), onlySL = TRUE)$pred)
  #f2 = as.vector(predict(f_hat2, as.data.frame(X2[-cf,]), onlySL = TRUE)$pred)
  impute_y1 = y[cf] - f1; impute_x1 = X2[cf,]*(A2[cf]-g1);
  impute_y2 = y[-cf] - f2; impute_x2 = X2[-cf,]*(A2[-cf]-g2);
  fit_cv1 = cv.glmnet(x = impute_x1, y = impute_y1, alpha = 1, intercept = FALSE); alpha_hat1 = as.vector(coef(fit_cv1, s = "lambda.min"))[-1]
  fit_cv2 = cv.glmnet(x = impute_x2, y = impute_y2, alpha = 1, intercept = FALSE); alpha_hat2 = as.vector(coef(fit_cv2, s = "lambda.min"))[-1]
  #alpha_hat1 = as.vector(lm(impute_y1 ~ impute_x1-1)$coefficients)
  #alpha_hat2 = as.vector(lm(impute_y2 ~ impute_x2-1)$coefficients)
  
  X1 = cbind(S1,rep(1,n))
  X2_alpha = X2%*%alpha_hat; y2 = y + X2_alpha*((X2_alpha>0)-A2)
  X2_alpha1 = X2[cf,]%*%alpha_hat1; y21 = y[cf] + X2_alpha1*((X2_alpha1>0)-A2[cf]) 
  X2_alpha2 = X2[-cf,]%*%alpha_hat2; y22 = y[-cf] + X2_alpha2*((X2_alpha2>0)-A2[-cf])
  
  g21 = cv.glmnet(x = X1[cf,], y = A1[cf], alpha = 1, family = "binomial")
  g22 = cv.glmnet(x = X1[-cf,], y = A1[-cf], alpha = 1, family = "binomial")
  f21 = cv.glmnet(x = X1[cf,], y = y21, alpha = 1)
  f22 = cv.glmnet(x = X1[-cf,], y = y22, alpha = 1)
  #g21 = SuperLearner(Y = A1[cf], X = as.data.frame(X1[cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #g22 = SuperLearner(Y = A1[-cf], X = as.data.frame(X1[-cf,]), family = binomial(), SL.library = Learners, method = "method.NNloglik")
  #f21 = SuperLearner(Y = y21, X = as.data.frame(X1[cf,]), SL.library = Learners)
  #f22 = SuperLearner(Y = y22, X = as.data.frame(X1[-cf,]), SL.library = Learners)
  
  gg2 = A1
  gg2[cf] = as.vector(predict(g22, newx = X1[cf,], s = "lambda.min", type = "response"))
  gg2[-cf] = as.vector(predict(g21, newx = X1[-cf,], s = "lambda.min", type = "response"))
  #gg2[cf] = as.vector(predict(g22, as.data.frame(X1[cf,]), onlySL = TRUE)$pred)
  #gg2[-cf] = as.vector(predict(g21, as.data.frame(X1[-cf,]), onlySL = TRUE)$pred)
  
  ff2 = y2
  ff2[cf] = as.vector(predict(f22, newx = X1[cf,], s = "lambda.min"))
  ff2[-cf] = as.vector(predict(f21, newx = X1[-cf,], s = "lambda.min"))
  #ff2[cf] = as.vector(predict(f22, as.data.frame(X1[cf,]), onlySL = TRUE)$pred)
  #ff2[-cf] = as.vector(predict(f21, as.data.frame(X1[-cf,]), onlySL = TRUE)$pred)
  
  impute_y2 = y2-ff2; impute_x2 = X1*(A1-gg2);
  fit_cv = cv.glmnet(x = impute_x2, y = impute_y2, alpha = 1, intercept = FALSE); beta_hat = as.vector(coef(fit_cv, s = "lambda.min"))[-1]
  #beta_hat = as.vector(lm(impute_y2 ~ impute_x2-1)$coefficients)
  
  return(list(alpha_hat,beta_hat))
}

RQLo_regimes <- function(alpha_hat,beta_hat,S1_new,C1s,C2s,parms)
{
  n_test = nrow(S1_new)
  X1_new = cbind(S1_new,rep(1,n_test))
  a1_hat = as.vector(ifelse((X1_new%*%beta_hat) > (C1s[2]-C1s[1]), 1, 0)); 
  S2_new = get_S2(S1_new,a1_hat)
  
  X2_new = cbind(S1_new,a1_hat,S2_new,rep(1,n_test))
  a2_hat = as.vector(ifelse((X2_new%*%alpha_hat) > (C2s[2]-C2s[1]), 1, 0)); 
  
  S_new = cbind(S1_new,a1_hat,S2_new)
  beta_y1 = parms[[1]]; beta_y2 = parms[[2]]; beta_y1A = parms[[3]]; beta_y2A = parms[[4]];
  y_new = S1_new%*%beta_y1 + S_new%*%beta_y2 + a1_hat*S1_new%*%beta_y1A + a2_hat*S_new%*%beta_y2A
  
  return(list(a1_hat,a2_hat,y_new))
}

HDQo = function(S1,S2,A1,A2,y) 
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

HDQo_regimes = function(psi1,psi2,S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
{
  n_test = nrow(S1_new); d1 = ncol(S1_new);
  H1_new = cbind(rep(1,n_test), S1_new)
  a1_hat = as.vector(ifelse((H1_new%*%psi1) > ((C1s[2]-C1s[1])/2), 1, -1)); 
  a1_hat_ = a1_hat; a1_hat_[a1_hat_==-1] = 0;
  S2_new = get_S2(S1_new,a1_hat_)
  S_new = cbind(S1_new,a1_hat,S2_new); H2_new = cbind(rep(1,n_test),S_new); S_new2 = cbind(S1_new,a1_hat_,S2_new);
  a2_hat = as.vector(ifelse((H2_new%*%psi2) > ((C2s[2]-C2s[1])/2), 1, -1)); 
  a2_hat_ = a2_hat; a2_hat_[a2_hat_==-1] = 0;
  
  beta_y1 = parms[[1]]; beta_y2 = parms[[2]]; beta_y1A = parms[[3]]; beta_y2A = parms[[4]];
  y_test = S1_new%*%beta_y1 + S_new2%*%beta_y2 + a1_hat_*S1_new%*%beta_y1A + a2_hat_*S_new2%*%beta_y2A
  
  psi21 = psi2[2:(d1+1)]
  psi11 = psi1[2:(d1+1)]
  #psi1_nz = union(which(psi21!=0), which(psi11!=0))
  psi1_nz = union(which(abs(psi21)>1e-6),which(abs(psi11)>1e-6))
  J_psi1 = c()
  
  for(k1 in 1:(length(J1s))){
    JJ1 = c(L1,J1s[[k1]])
    if(all(is.element(psi1_nz, JJ1))){
      J_psi1 = append(J_psi1,k1)
    }
  }
  
  psi22 = psi2[(d1+3):length(psi2)]
  #psi22_nz = which(psi22 != 0)
  psi22_nz = which(abs(psi22)>1e-6)
  J_psi22 = c()
  
  for(k2 in 1:(length(J2s))){
    JJ2 = c(L2,J2s[[k2]])
    if(all(is.element(psi22_nz, JJ2))){
      J_psi22 = append(J_psi22,k2)
    }
  }
  
  return(list(a1_hat_, a2_hat_, y_test, J_psi22, J_psi1))
}
