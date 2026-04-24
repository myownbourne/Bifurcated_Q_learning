library(SuperLearner)
library(foreach)
library(doParallel)
library(sparsegl)
library(ncvreg)
library(ranger)
source("methods4_misspecification.r")
set.seed(1234567)

get_S2 = function(S1,A1)
{
  n = nrow(S1)
  d2 = ncol(S1)
  R = matrix(rnorm(n*d2),nrow = n)
  S2 = S1 + as.vector(A1) + R;
  return(S2)
}


num_sim = 200
CJ2s_set = c(0,1,2,3,4,5,6,7,9,11,13,15)*0.1
num_J = length(CJ2s_set)

sim_result = array(data = 0, dim = c(num_J, 15, num_sim))

cl = makeCluster(48)
registerDoParallel(cl)

#for(i in 1:num_sim){
sim_result = foreach(i = 1:num_sim, .combine = 'c') %dopar% {
  print(i)
  library(sparsegl)
  library(SuperLearner)
  library(ncvreg)
  library(glmnet)
  library(ranger)
  set.seed(2025*i)
  sim_resulti = matrix(data = 0, nrow = num_J, ncol = 15)
  n = 500; d1 = 5; d2 = 5; d = 1+d1+d2;
  
  beta_A1 = rep(0,d1); beta_A1[1:3] = c(1,1,0.5);  
  beta_A2 = rep(0,d); beta_A2[1:2] = c(1,1); beta_A2[(d1+2):(d1+3)] = c(1,1);
  beta_Y1 = rep(1,d1); beta_Y1 = beta_Y1/sqrt(d1);
  beta_R = rep(0,d2); beta_R[1] = c(1); beta_R[d2] = 1;
  beta_SR = rep(0,d); beta_SR[3:4] = c(1,1); beta_SR[(d1+1)] = 0.5; beta_SR[(d1+4):(d1+5)] = c(0.5,1)
  parms = list(beta_Y1, beta_R, beta_SR)

  k = 1
  S1 = matrix(rnorm(n*d1),nrow = n)
  linA1 = S1%*%beta_A1 + k*(S1[,1])^3+k*cos(S1[,3])
  A1 = rbinom(n, 1, 1/(1+exp(-linA1)))
  R = matrix(rnorm(n*d2),nrow = n)
  S2 = S1 + as.vector(A1) + R; S = cbind(S1,A1,S2)
  linA2 = S%*%beta_A2 + k*(S[,2])^3 + k*sin(S[,(d-1)])
  A2 = rbinom(n, 1, 1/(1+exp(-linA2)))
  SR = cbind(S1,A1,R) 

  y = SR%*%beta_SR + A1 * (S1%*% beta_Y1 + k * (S1[,1]^2-1)) + A2 * (R%*%beta_R + k * (R[,2]^2-1) + k * (sin(R[,d2]) - exp(-0.5)*R[,d2]))

  
  n_test = 5000; S1_new = matrix(rnorm(n_test*d1),nrow = n_test)
  
  J11 = seq(from = 2, to = d1, by = 1)
  J1s = list(J11)
  L1 = c(1)
  
  J21 = seq(from = 2, to = d2 - 1, by = 1)
  J22 = seq(from = 2, to = d2, by = 1)
  J2s = list(J21,J22)
  L2 = c(1)
  
  C1s = c(0,0)
  C2s = c(0,0)
  CJ2s_set = c(0,1,2,3,4,5,6,7,9,11,13,15)*0.1

  Learners = c("SL.glm","SL.ranger")
  Learners_ps = c("SL.glm","SL.ranger")
  
  #add
  JJ11 = seq(from = 2, to = d1, by = 1)
  JJ1 = list(JJ11)
  JJ21 = seq(from = 2, to = d2 - 1, by = 1)
  JJ22 = c(d2)
  JJ2 = list(JJ21,JJ22)

  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,k)

  RQLo_res = RQLo(S1,S2,A1,A2,y,Learners,Learners_ps)
  RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms,k)
  
  
  for(j in 1:num_J){
    set.seed((123456+i)*j)
    CJ1s = c(0)
    CJ2s = c(0,CJ2s_set[j])
    CJJ1 = c(0)
    CJJ2 = c(0,CJ2s_set[j])
    
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,k)
    
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners,Learners_ps)
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,k)
    
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners,Learners_ps) 
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms,k)
    
    J2_BQL = BQL_reg[[3]]; u_BQL = BQL_reg[[5]]; uc_BQL = u_BQL; idx2_BQL2 = which(J2_BQL==2); 
    uc_BQL[idx2_BQL2] = uc_BQL[idx2_BQL2] - CJ2s[2]
    
    u_HDQ = HDQ_reg[[3]]; J2_HDQ = HDQ_reg[[4]]; CJ2_HDQ = CJ2s[J2_HDQ]; 
    J2_HDQ_f = J2_HDQ[which.min(CJ2_HDQ)]; uc_HDQ = u_HDQ - min(CJ2_HDQ);
    
    u_RQL = RQL_reg[[3]]; J2_RQL = RQL_reg[[5]]; CJ2_RQL = CJ2s[J2_RQL];
    J2_RQL_f = J2_RQL[which.min(CJ2_RQL)]; uc_RQL = u_RQL - min(CJ2_RQL);

    u_HDQo = HDQo_reg[[3]]; J2_HDQo = HDQo_reg[[4]]; CJ2_HDQo = CJ2s[J2_HDQo]; 
    J2_HDQo_f = J2_HDQo[which.min(CJ2_HDQo)]; uc_HDQo = u_HDQo - min(CJ2_HDQo);
    
    u_RQLo = RQLo_reg[[3]]; uc_RQLo = u_RQLo - CJ2s[2];
    
    
    c1 = c(mean(J2_BQL==2),mean(u_BQL),mean(uc_BQL))
    c2 = c(mean(J2_HDQ_f==2),mean(u_HDQ),mean(uc_HDQ))
    c3 = c(mean(J2_RQL_f==2),mean(u_RQL),mean(uc_RQL))
    c4 = c(mean(J2_HDQo_f==2),mean(u_HDQo),mean(uc_HDQo))
    c5 = c(1,mean(u_RQLo),mean(uc_RQLo))
    
    sim_resulti[j,] = c(c1,c2,c3,c4,c5)
  }
  sim_resulti
}

stopCluster(cl)
sim_result = array(unlist(sim_result), dim = c(num_J, 15, num_sim))
sim_result2 = apply(sim_result, c(1, 2), mean)
print(sim_result2)

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                                row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                                col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                                matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))

write.csv(long_format_data, file = "result_S17_k1.csv", row.names = FALSE)
