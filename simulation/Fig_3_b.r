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

cl = makeCluster(50)
registerDoParallel(cl)

num_sim = 200
CJ2s_set = c(0,0.4,0.8,1.2,1.5,2,3,4,5,6)*0.1
num_J = length(CJ2s_set)

print(1112)

#==== gold ====
BQL_golds = foreach(j = 1:num_J, .packages = c("SuperLearner", "ranger")) %dopar% {
  print(j)
  
  n = 30000; d1 = 5; d2 = 5; d = 1 + d1 + d2
  
  beta_A1 = rep(0, d1); beta_A1[1:2] = c(1,1)
  beta_A2 = rep(0, d);  beta_A2[1:2] = c(1,1); beta_A2[7:8] = c(1,1)
  beta_y1 = rep(0, d1); beta_y1[1:3] = c(1,1,0.5)
  beta_y1A = rep(0, d1); beta_y1A[1:3] = c(1,1,0.5)
  beta_y2 = rep(0, d); beta_y2[c(8:9)] = c(1,0.5); beta_y2[11] = 1
  beta_y2A = rep(0, d); beta_y2A[c(8:9)] = c(1,0.5); beta_y2A[11] = 1
  
  set.seed(1000 + j)
  
  S1 = matrix(rnorm(n * d1), nrow = n)
  A1 = rbinom(n, 1, 1 / (1 + exp(-S1 %*% beta_A1)))
  S2 = get_S2(S1, A1)
  S = cbind(S1, A1, S2)
  A2 = rbinom(n, 1, 1 / (1 + exp(-S %*% beta_A2)))
  y = S1 %*% beta_y1 + S %*% beta_y2 + A1 * S1 %*% beta_y1A + A2 * S %*% beta_y2A + 0.5 * rnorm(n)
  
  J11 = seq(from = 2, to = d1, by = 1)
  J1s = list(J11)
  L1 = c(1)
  
  J21 = seq(from = 2, to = d2 - 1, by = 1)
  J22 = seq(from = 2, to = d2, by = 1)
  J2s = list(J21, J22)
  L2 = c(1)
  
  C1s = c(0, 0)
  C2s = c(0, 0)
  CJ1s = c(0)
  CJ2s = c(0, CJ2s_set[j])
  Learners = c("SL.glm", "SL.ranger")
  
  BQL_4(S1, S2, A1, A2, y, J1s, J2s, L1, L2, C1s, CJ1s, C2s, CJ2s, Learners)
}

print(111)


sim_result = array(data = 0, dim = c(num_J, 10, num_sim))


#for(i in 1:num_sim){
sim_result = foreach(i = 1:num_sim, .combine = 'c') %dopar% {
  print(i)
  library(sparsegl)
  library(SuperLearner)
  library(ncvreg)
  set.seed(2025*i)
  sim_resulti = matrix(data = 0, nrow = num_J, ncol = 10)
  n = 500; d1 = 5; d2 = 5; d = 1+d1+d2;
  
  beta_A1 = rep(0,d1); beta_A1[1:2] = c(1,1);
  beta_A2 = rep(0,d); beta_A2[1:2] = c(1,1); beta_A2[7:8] = c(1,1);
  beta_y1 = rep(0,d1);  beta_y1[1:3] = c(1,1,0.5); 
  beta_y1A = rep(0,d1); beta_y1A[1:3] = c(1,1,0.5);
  beta_y2 = rep(0,d);   beta_y2[c(8:9)] = c(1,0.5); beta_y2[11] = 1;
  beta_y2A = rep(0,d);  beta_y2A[c(8:9)] = c(1,0.5); beta_y2A[11] = 1;
  
  S1 = matrix(rnorm(n*d1),nrow = n)
  A1 = rbinom(n, 1, 1/(1+exp(-S1%*%beta_A1)))
  S2 = get_S2(S1,A1); S = cbind(S1,A1,S2)
  A2 = rbinom(n, 1, 1/(1+exp(-S%*%beta_A2)))
  y = S1%*%beta_y1 + S%*%beta_y2 + A1*S1%*%beta_y1A + A2*S%*%beta_y2A + 0.5*rnorm(n)
  parms = list(beta_y1,beta_y2,beta_y1A,beta_y2A)
  
  n_test = 5000; S1_new = matrix(rnorm(n_test*d1),nrow = n_test)
  
  #J11 = c(2,3,5)
  J11 = seq(from = 2, to = d1, by = 1)
  J1s = list(J11)
  L1 = c(1)
  
  J21 = seq(from = 2, to = d2 - 1, by = 1)
  J22 = seq(from = 2, to = d2, by = 1)
  J2s = list(J21,J22)
  L2 = c(1)
  
  C1s = c(0,0)
  C2s = c(0,0)
  CJ2s_set = c(0,0.4,0.8,1.2,1.5,2,3,4,5,6)*0.1
  
  Learners = c("SL.glm","SL.ranger")
  #Learners = c("SL.glm")
  
  #add
  JJ11 = seq(from = 2, to = d1, by = 1)
  JJ1 = list(JJ11)
  JJ21 = seq(from = 2, to = d2 - 1, by = 1)
  JJ22 = c(d2)
  JJ2 = list(JJ21,JJ22)
  
  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  RQLo_res = RQLo(S1,S2,A1,A2,y,Learners)
  
  
  for(j in 1:num_J){
    set.seed((123456+i)*j)
    CJ1s = c(0)
    CJ2s = c(0,CJ2s_set[j])
    CJJ1 = c(0)
    CJJ2 = c(0,CJ2s_set[j])
    
    HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,CJ1s,CJ2s,BQL_golds[[j]])
    RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms,L1,L2,J1s,J2s,CJ1s,CJ2s,BQL_golds[[j]])
    
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,CJ1s,CJ2s,BQL_golds[[j]])
    
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners)
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,CJ1s,CJ2s,parms,BQL_golds[[j]])
    
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms,BQL_golds[[j]])
    
    BQL_tpr2 = mean(BQL_reg[[8]], na.rm = TRUE); BQL_fpr2 = mean(BQL_reg[[9]], na.rm = TRUE);
    HDQ_tpr2 = mean(HDQ_reg[[8]], na.rm = TRUE); HDQ_fpr2 = mean(HDQ_reg[[9]], na.rm = TRUE);
    RQL_tpr2 = mean(RQL_reg[[8]], na.rm = TRUE); RQL_fpr2 = mean(RQL_reg[[9]], na.rm = TRUE);
    HDQo_tpr2 = mean(HDQo_reg[[8]], na.rm = TRUE); HDQo_fpr2 = mean(HDQo_reg[[9]], na.rm = TRUE);
    RQLo_tpr2 = mean(RQLo_reg[[8]], na.rm = TRUE); RQLo_fpr2 = mean(RQLo_reg[[9]], na.rm = TRUE);
    
    
    sim_resulti[j,] = c(BQL_tpr2,BQL_fpr2,HDQ_tpr2,HDQ_fpr2,RQL_tpr2,RQL_fpr2,HDQo_tpr2,HDQo_fpr2,RQLo_tpr2,RQLo_fpr2)
  }
  sim_resulti
}

stopCluster(cl)
sim_result = array(unlist(sim_result), dim = c(num_J, 10, num_sim))
#sim_result2 = apply(sim_result, c(1, 2), mean)
sim_result2 = apply(sim_result, c(1, 2), function(x) mean(x, na.rm = TRUE))
print(sim_result2)

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                              row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                              col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                              matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))

write.csv(long_format_data, file = "result_S11_tpr_fpr_5000.csv", row.names = FALSE)
