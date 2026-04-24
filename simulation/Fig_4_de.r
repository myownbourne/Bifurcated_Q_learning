library(SuperLearner)
library(foreach)
library(doParallel)
library(sparsegl)
library(ncvreg)
library(ranger)
source("methods_tpr_fpr.r")
set.seed(1234567)

get_S2 = function(S1,A1)
{
  n = nrow(S1)
  d2 = ncol(S1)
  S2 = S1 + A1*S1 + matrix(rnorm(n*d2),nrow = n)
  return(S2)
}

cl = makeCluster(40)
registerDoParallel(cl)

print(2222)
num_sim = 200
lambda_set = c(0,0.4,0.8,1.2,2,2.8,3.6,4.4,5.2,6,7,8,9,10)
num_J = length(lambda_set)
Learners = c("SL.glm","SL.ranger")

#==== gold ====
BQL_golds = foreach(j = 1:num_J, .packages = c("SuperLearner", "ranger")) %dopar% {
  print(j)
  
  n = 30000; d1 = 5; d2 = 5; d = 1+d1+d2;
  
  beta_A1 = rep(0,d1); beta_A1[1] = c(1);
  beta_A2 = rep(0,d); beta_A2[1] = c(1); beta_A2[3:10] = 0.5; beta_A2[8] = 0
  beta_y1 = rep(0,d1);  beta_y1[1] = c(1); beta_y1[3] = 0.5; 
  beta_y1A = rep(0,d1); beta_y1A[1] = c(1); beta_y1A[3] = 0.5; 
  beta_y2 = rep(0,d);   beta_y2[7] = c(1); beta_y2[9] = 1; beta_y2[10] = 1;
  beta_y2A = rep(0,d);  beta_y2A[7] = c(1); beta_y2A [9] = 1; beta_y2A[10] = 1;
  
  set.seed(1000 + j)
  
  S1 = matrix(rnorm(n*d1),nrow = n)
  A1 = rbinom(n, 1, 1/(1+exp(-S1%*%beta_A1)))
  S2 = get_S2(S1,A1); S = cbind(S1,A1,S2)
  A2 = rbinom(n, 1, 1/(1+exp(-S%*%beta_A2)))
  y = S1%*%beta_y1 + S%*%beta_y2 + A1*S1%*%beta_y1A + A2*S%*%beta_y2A + 0.5*rnorm(n)
  
  J11 = c(3,4,5)
  #J11 = seq(from = 2, to = d1 - 1, by = 1)
  J12 = seq(from = 2, to = d1, by = 1)
  J1s = list(J11,J12)
  L1 = c(1)
  
  J21 = seq(from = 2, to = d2 - 2, by = 1)
  J22 = seq(from = 2, to = d2 - 1, by = 1)
  J23 = seq(from = 2, to = d2, by = 1)
  J2s = list(J21,J22,J23)
  L2 = c(1)
  
  C1s = c(0,0)
  C2s = c(0,0)
  CJ1s = c(0,0.2*lambda_set[j])
  CJ2s = c(0,0.1*lambda_set[j],0.2*lambda_set[j])
 
  Learners = c("SL.glm", "SL.ranger")
  
  BQL_4(S1, S2, A1, A2, y, J1s, J2s, L1, L2, C1s, CJ1s, C2s, CJ2s, Learners)
}

print(111)


sim_result = array(data = 0, dim = c(num_J, 20, num_sim))


#for(i in 1:num_sim){
sim_result = foreach(i = 1:num_sim, .combine = 'c') %dopar% {
  library(SuperLearner)
  library(sparsegl)
  library(ncvreg)
  set.seed(2025*i)
  sim_resulti = matrix(data = 0, nrow = num_J, ncol = 20)
  n = 500; d1 = 5; d2 = 5; d = 1+d1+d2;
  
  beta_A1 = rep(0,d1); beta_A1[1] = c(1);
  beta_A2 = rep(0,d); beta_A2[1] = c(1); beta_A2[3:10] = 0.5; beta_A2[8] = 0
  beta_y1 = rep(0,d1);  beta_y1[1] = c(1); beta_y1[3] = 0.5; 
  beta_y1A = rep(0,d1); beta_y1A[1] = c(1); beta_y1A[3] = 0.5; 
  beta_y2 = rep(0,d);   beta_y2[7] = c(1); beta_y2[9] = 1; beta_y2[10] = 1;
  beta_y2A = rep(0,d);  beta_y2A[7] = c(1); beta_y2A [9] = 1; beta_y2A[10] = 1;
  
  S1 = matrix(rnorm(n*d1),nrow = n)
  A1 = rbinom(n, 1, 1/(1+exp(-S1%*%beta_A1)))
  S2 = get_S2(S1,A1); S = cbind(S1,A1,S2)
  A2 = rbinom(n, 1, 1/(1+exp(-S%*%beta_A2)))
  y = S1%*%beta_y1 + S%*%beta_y2 + A1*S1%*%beta_y1A + A2*S%*%beta_y2A + 0.5*rnorm(n)
  parms = list(beta_y1,beta_y2,beta_y1A,beta_y2A)
  
  n_test = 5000; S1_new = matrix(rnorm(n_test*d1),nrow = n_test)
  
  J11 = c(3,4,5)
  #J11 = seq(from = 2, to = d1 - 1, by = 1)
  J12 = seq(from = 2, to = d1, by = 1)
  J1s = list(J11,J12)
  L1 = c(1)
  
  J21 = seq(from = 2, to = d2 - 2, by = 1)
  J22 = seq(from = 2, to = d2 - 1, by = 1)
  J23 = seq(from = 2, to = d2, by = 1)
  J2s = list(J21,J22,J23)
  L2 = c(1)
  
  C1s = c(0,0)
  C2s = c(0,0)
  lambda_set = c(0,0.4,0.8,1.2,2,2.8,3.6,4.4,5.2,6,7,8,9,10)
  
  #add
  JJ11 = c(3,4,5)
  JJ12 = c(2)
  JJ1 = list(JJ11,JJ12)
  JJ21 = c(2,3)
  JJ22 = c(4)
  JJ23 = c(5)
  JJ2 = list(JJ21,JJ22,JJ23)
  
  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  RQLo_res = RQLo(S1,S2,A1,A2,y,Learners)
  
  for(j in 1:num_J){
    print(j)
    set.seed((123456+i)*j)
    CJ1s = c(0,0.2*lambda_set[j])
    CJ2s = c(0,0.1*lambda_set[j],0.2*lambda_set[j])
    CJJ1 = c(0,0.2*lambda_set[j])
    CJJ2 = c(0,0.1*lambda_set[j],0.1*lambda_set[j])
    
    HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,CJ1s,CJ2s,BQL_golds[[j]])
    RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms,L1,L2,J1s,J2s,CJ1s,CJ2s,BQL_golds[[j]])
    
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,CJ1s,CJ2s,BQL_golds[[j]])
    
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners)
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,CJ1s,CJ2s,parms,BQL_golds[[j]])
    
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms,BQL_golds[[j]])
    
    BQL_tpr1 = mean(BQL_reg[[6]], na.rm = TRUE); BQL_fpr1 = mean(BQL_reg[[7]], na.rm = TRUE);
    HDQ_tpr1 = mean(HDQ_reg[[6]], na.rm = TRUE); HDQ_fpr1 = mean(HDQ_reg[[7]], na.rm = TRUE);
    RQL_tpr1 = mean(RQL_reg[[6]], na.rm = TRUE); RQL_fpr1 = mean(RQL_reg[[7]], na.rm = TRUE);
    HDQo_tpr1 = mean(HDQo_reg[[6]], na.rm = TRUE); HDQo_fpr1 = mean(HDQo_reg[[7]], na.rm = TRUE);
    RQLo_tpr1 = mean(RQLo_reg[[6]], na.rm = TRUE); RQLo_fpr1 = mean(RQLo_reg[[7]], na.rm = TRUE);
    
    BQL_tpr2 = mean(BQL_reg[[8]], na.rm = TRUE); BQL_fpr2 = mean(BQL_reg[[9]], na.rm = TRUE);
    HDQ_tpr2 = mean(HDQ_reg[[8]], na.rm = TRUE); HDQ_fpr2 = mean(HDQ_reg[[9]], na.rm = TRUE);
    RQL_tpr2 = mean(RQL_reg[[8]], na.rm = TRUE); RQL_fpr2 = mean(RQL_reg[[9]], na.rm = TRUE);
    HDQo_tpr2 = mean(HDQo_reg[[8]], na.rm = TRUE); HDQo_fpr2 = mean(HDQo_reg[[9]], na.rm = TRUE);
    RQLo_tpr2 = mean(RQLo_reg[[8]], na.rm = TRUE); RQLo_fpr2 = mean(RQLo_reg[[9]], na.rm = TRUE);
    
    c1 = c(BQL_tpr1,BQL_fpr1,HDQ_tpr1,HDQ_fpr1,RQL_tpr1,RQL_fpr1,HDQo_tpr1,HDQo_fpr1,RQLo_tpr1,RQLo_fpr1)
    c2 = c(BQL_tpr2,BQL_fpr2,HDQ_tpr2,HDQ_fpr2,RQL_tpr2,RQL_fpr2,HDQo_tpr2,HDQo_fpr2,RQLo_tpr2,RQLo_fpr2)
    sim_resulti[j,] = c(c1,c2)
  }
  sim_resulti
}

stopCluster(cl)
sim_result = array(unlist(sim_result), dim = c(num_J, 20, num_sim))
#sim_result2 = apply(sim_result, c(1, 2), mean)
sim_result2 = apply(sim_result, c(1, 2), function(x) mean(x, na.rm = TRUE))
print(sim_result2)

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                              row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                              col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                              matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))
write.csv(long_format_data, file = "result_S12_tpr_fpr_5000.csv", row.names = FALSE)

