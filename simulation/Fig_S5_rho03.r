library(SuperLearner)
library(foreach)
library(doParallel)
library(sparsegl)
library(ncvreg)
library(ranger)
source("methods2_correlated.r")

get_S2 = function(S1,A1,rho)
{
  n = nrow(S1)
  d2 = ncol(S1)
  idx = 1:d2; Sigma = rho^abs(outer(idx,idx,"-"))
  eps = MASS::mvrnorm(n = n, mu = rep(0, d2), Sigma = Sigma); eps = matrix(eps, nrow = n, ncol = d2)
  #eps = matrix(rnorm(n*d2),nrow = n)
  S2 = S1 + A1*S1 + eps
  return(S2)
}


cl = makeCluster(40)
registerDoParallel(cl)

num_sim = 200
CJ22_set = c(0,0.04,0.08,0.12,0.16,0.2,0.25,0.3,0.4,0.5,0.6,0.7)
CJ23_set = 2*CJ22_set
num_J = length(CJ22_set)
Learners = c("SL.glm","SL.ranger")
#Learners = c("SL.glm")

sim_result = foreach(j = 1:num_sim, .combine = 'c') %dopar% {
  set.seed(2025*j)
  library(SuperLearner)
  library(ncvreg)
  library(sparsegl)
  library(ranger)
  sim_resulti = matrix(0, nrow = num_J, ncol = 23)
  n = 500; d1 = 5; d2 = 5; d = 1+d1+d2;
  
  beta_A1 = 0.5^(1:d1);
  beta_A2 = 0.5^(1:d);
  beta_y1 = rep(0.2,d1); beta_y1[1] = 1; beta_y2 = rep(0.2,d); beta_y2[d-1] = 1.5; beta_y2[d] = 1;
  beta_y1A = rep(0.2,d1); beta_y1A[1] = 0.5; beta_y2A = rep(0.2,d); beta_y2A[d-1] = 1.5; beta_y2A[d] = 1;
  
  rho = 0.3
  idx = 1:d1; Sigma = rho^abs(outer(idx,idx,"-"))
  S1 = MASS::mvrnorm(n = n, mu = rep(0, d1), Sigma = Sigma); S1 = matrix(S1, nrow = n, ncol = d1)
  A1 = rbinom(n, 1, 1/(1+exp(-S1%*%beta_A1)))
  S2 = get_S2(S1,A1,rho); S = cbind(S1,A1,S2)
  A2 = rbinom(n, 1, 1/(1+exp(-S%*%beta_A2)))
  y = S1%*%beta_y1 + S%*%beta_y2 + A1*S1%*%beta_y1A + A2*S%*%beta_y2A + 0.4*rnorm(n)
  
  parms = list(beta_y1,beta_y2,beta_y1A,beta_y2A)
  n_test = 5000; S1_new = MASS::mvrnorm(n = n_test, mu = rep(0, d1), Sigma = Sigma); S1_new = matrix(S1, nrow = n, ncol = d1)
  
  L1 = c(1)
  J11 = seq(from = 2, to = d1, by = 1)
  J1s = list(J11)
  
  J21 = seq(from = 2, to = d2-2, by = 1)
  J22 = seq(from = 2, to = d2-1, by = 1)
  J23 = seq(from = 2, to = d2, by = 1)
  J2s = list(J21,J22,J23)
  L2 = c(1)
  C1s = c(0,0)
  C2s = c(0,0)
  CJ22_set = c(0,0.04,0.08,0.12,0.16,0.2,0.25,0.3,0.4,0.5,0.6,0.7)
  CJ23_set = 2*CJ22_set
  
  JJ11 = seq(from = 2, to = d2-2, by = 1)
  JJ1 = list(JJ11)
  JJ21 = c(2,3)
  JJ22 = c(4)
  JJ23 = c(5)
  JJ2 = list(JJ21,JJ22,JJ23)

  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,rho)
  
  RQLo_res = RQLo(S1,S2,A1,A2,y,Learners)
  RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms,rho)
  
  
  for(i in 1:num_J){
    set.seed((123456+i)*j)
    CJ1s = c(0)
    CJ2s = c(0,CJ22_set[i],CJ23_set[i])
    
    CJJ1 = c(0)
    CJJ23 = CJ23_set[i] - CJ22_set[i]
    CJJ2 = c(0,CJ22_set[i],CJJ23)
    
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,rho)
    
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners)
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms,rho)
    
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms,rho)
    
    J2_BQL = BQL_reg[[3]]; u_BQL = BQL_reg[[5]]; uc_BQL = u_BQL
    idx2_BQL1 = which(J2_BQL==1); idx2_BQL2 = which(J2_BQL==2); idx2_BQL3 = which(J2_BQL==3); 
    uc_BQL[idx2_BQL1] = uc_BQL[idx2_BQL1] - CJ2s[1]
    uc_BQL[idx2_BQL2] = uc_BQL[idx2_BQL2] - CJ2s[2]
    uc_BQL[idx2_BQL3] = uc_BQL[idx2_BQL3] - CJ2s[3]
    
    u_HDQ = HDQ_reg[[3]]; J2_HDQ = HDQ_reg[[4]]; CJ2_HDQ = CJ2s[J2_HDQ]; 
    J2_HDQ = J2_HDQ[which.min(CJ2_HDQ)]; uc_HDQ = u_HDQ - min(CJ2_HDQ);
    
    u_RQL = RQL_reg[[3]]; J2_RQL = RQL_reg[[5]]; CJ2_RQL = CJ2s[J2_RQL]; 
    J2_RQL = J2_RQL[which.min(CJ2_RQL)]; uc_RQL = u_RQL - min(CJ2_RQL);

    u_HDQo = HDQo_reg[[3]]; J2_HDQo = HDQo_reg[[4]]; CJ2_HDQo = CJ2s[J2_HDQo]; 
    J2_HDQo = J2_HDQo[which.min(CJ2_HDQo)]; uc_HDQo = u_HDQo - min(CJ2_HDQo);
    
    u_RQLo = RQLo_reg[[3]]; uc_RQLo = u_RQLo - CJ2s[3];
   
    
    sim_resulti1 = c(mean(J2_BQL==1),mean(J2_BQL==2),mean(J2_BQL==3),mean(u_BQL),mean(uc_BQL))
    sim_resulti2 = c(mean(J2_HDQ==1),mean(J2_HDQ==2),mean(J2_HDQ==3),mean(u_HDQ),mean(uc_HDQ))
    sim_resulti3 = c(mean(J2_RQL==1),mean(J2_RQL==2),mean(J2_RQL==3),mean(u_RQL),mean(uc_RQL))
    sim_resulti4 = c(mean(J2_HDQo==1),mean(J2_HDQo==2),mean(J2_HDQo==3),mean(u_HDQo),mean(uc_HDQo))
    sim_resulti5 = c(1,mean(u_RQLo),mean(uc_RQLo))
    sim_resulti[i, ] = c(sim_resulti1,sim_resulti2,sim_resulti3,sim_resulti4,sim_resulti5)
  }
  sim_resulti
}
stopCluster(cl)
sim_result = array(unlist(sim_result), dim = c(num_J, 23, num_sim))
sim_result2 = apply(sim_result, c(1, 2), mean)
print(sim_result2)

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                                row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                                col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                                matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))
write.csv(long_format_data, file = "result_S13_rho3.csv", row.names = FALSE)

