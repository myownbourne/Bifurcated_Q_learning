library(SuperLearner)
library(foreach)
library(doParallel)
library(sparsegl)
library(ncvreg)
library(ranger)
source("methods.r")

get_S2 = function(S1,A1)
{
  n = nrow(S1)
  d2 = ncol(S1)
  S2 = S1 + A1*S1 + matrix(rnorm(n*d2),nrow = n)
  return(S2)
}


cl = makeCluster(100)
registerDoParallel(cl)

xx = c(0,0.2,0.4,0.8,1.2,1.6,2.0,2.4,2.8,3.2,3.6,4.0,4.4,4.8,5.2,5.6,6.0)
num_J = length(xx)

num_sim = 200
Learners = c("SL.glm","SL.ranger")
#Learners = c("SL.glm")

sim_result = foreach(j = 1:num_sim, .combine = 'c') %dopar% {
  #for(j in 1:num_sim){
  print(j)
  set.seed(2024*j)
  library(ncvreg)
  library(SuperLearner)
  library(sparsegl)
  
  sim_resulti = matrix(0, nrow = num_J, ncol = 42)
  
  n = 500; d1 = 3; d2 = 3; d = 1+d1+d2;
  
  beta_A1 = 0.6^(1:d1); 
  beta_A2 = 0.6^(1:d);  beta_A2[5] = 0;
  beta_y1 = rep(0,d1);  beta_y1[1:2] = c(1,1); 
  beta_y1A = rep(0,d1); #beta_y1A[1:2] = c(1,1);
  beta_y2 = rep(0,d);   beta_y2[c(5:7)] = c(0,1,2); beta_y2[1] = 0.5
  beta_y2A = rep(0,d);  beta_y2A[c(5:7)] = c(0,1,2); beta_y2A[1] = 0.5
  
  S1 = matrix(rnorm(n*d1),nrow = n)
  A1 = rbinom(n, 1, 1/(1+exp(-S1%*%beta_A1)))
  S2 = get_S2(S1,A1); S = cbind(S1,A1,S2)
  A2 = rbinom(n, 1, 1/(1+exp(-S%*%beta_A2)))
  y = S1%*%beta_y1 + S%*%beta_y2 + A1*S1%*%beta_y1A + A2*S%*%beta_y2A + 0.5*rnorm(n)
  parms = list(beta_y1,beta_y2,beta_y1A,beta_y2A)
  
  n_test = 5000; S1_new = matrix(rnorm(n_test*d1),nrow = n_test)
  
  J11 = c(2,3)
  L1 = c(1)
  J1s = list(J11)
  
  J21 = c(); J22 = c(1); J23 = c(2); J24 = c(3);
  J25 = c(1,2); J26 = c(1,3); J27 = c(2,3);
  J28 = c(1,2,3);
  J2s = list(J21,J22,J23,J24,J25,J26,J27,J28)
  L2 = c()
  C1s = c(0,0)
  C2s = c(0,0)

  xx = c(0,0.2,0.4,0.8,1.2,1.6,2.0,2.4,2.8,3.2,3.6,4.0,4.4,4.8,5.2,5.6,6.0)
  CJ22_set = 0.2*xx
  CJ23_set = 0.2*xx
  CJ24_set = 0.2*xx
  CJ25_set = 0.4*xx
  CJ26_set = 0.4*xx
  CJ27_set = 0.4*xx
  CJ28_set = 0.6*xx
  
  JJ11 = c(2,3)
  JJ1 = list(JJ11)
  JJ21 = c(1); JJ22 = c(2); JJ23 = c(3);
  JJ2 = list(JJ21,JJ22,JJ23)

  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
  
  RQLo_res = RQLo(S1,S2,A1,A2,y,Learners)
  RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms)
  
  for(i in 1:num_J){
    set.seed((123456+i)*j)
    CJ2s = c(0,CJ22_set[i],CJ23_set[i],CJ24_set[i],CJ25_set[i],CJ26_set[i],CJ27_set[i],CJ28_set[i])
    CJ1s = c(0)
    
    CJJ1 = c(0)
    CJJ2 = c(CJ22_set[i],CJ22_set[i],CJ22_set[i])
    
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
    
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners)
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
    
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms)
    
    J2_BQL = BQL_reg[[3]]; u_BQL = BQL_reg[[5]]; uc_BQL = u_BQL
    idx2_BQL1 = which(J2_BQL==1); idx2_BQL2 = which(J2_BQL==2); idx2_BQL3 = which(J2_BQL==3); idx2_BQL4 = which(J2_BQL==4);
    idx2_BQL5 = which(J2_BQL==5); idx2_BQL6 = which(J2_BQL==6); idx2_BQL7 = which(J2_BQL==7); idx2_BQL8 = which(J2_BQL==8); 
    uc_BQL[idx2_BQL1] = uc_BQL[idx2_BQL1] - CJ2s[1]
    uc_BQL[idx2_BQL2] = uc_BQL[idx2_BQL2] - CJ2s[2]
    uc_BQL[idx2_BQL3] = uc_BQL[idx2_BQL3] - CJ2s[3]
    uc_BQL[idx2_BQL4] = uc_BQL[idx2_BQL4] - CJ2s[4]
    uc_BQL[idx2_BQL5] = uc_BQL[idx2_BQL5] - CJ2s[5]
    uc_BQL[idx2_BQL6] = uc_BQL[idx2_BQL6] - CJ2s[6]
    uc_BQL[idx2_BQL7] = uc_BQL[idx2_BQL7] - CJ2s[7]
    uc_BQL[idx2_BQL8] = uc_BQL[idx2_BQL8] - CJ2s[8]
    
    u_HDQ = HDQ_reg[[3]]; J2_HDQ = HDQ_reg[[4]]; CJ2_HDQ = CJ2s[J2_HDQ]; 
    J2_HDQ = J2_HDQ[which.min(CJ2_HDQ)]; uc_HDQ = u_HDQ - min(CJ2_HDQ);
    
    u_RQL = RQL_reg[[3]]; J2_RQL = RQL_reg[[5]]; CJ2_RQL = CJ2s[J2_RQL]; 
    J2_RQL = J2_RQL[which.min(CJ2_RQL)]; uc_RQL = u_RQL - min(CJ2_RQL);

    u_HDQo = HDQo_reg[[3]]; J2_HDQo = HDQo_reg[[4]]; CJ2_HDQo = CJ2s[J2_HDQo]; 
    J2_HDQo = J2_HDQo[which.min(CJ2_HDQo)]; uc_HDQo = u_HDQo - min(CJ2_HDQo);
    
    u_RQLo = RQLo_reg[[3]]; uc_RQLo = u_RQLo - CJ2s[8];
   
    
    J_BQLss = c(mean(J2_BQL==1),mean(J2_BQL==2),mean(J2_BQL==3),mean(J2_BQL==4),mean(J2_BQL==5),mean(J2_BQL==6),mean(J2_BQL==7),mean(J2_BQL==8))
    J_HDQss = c(mean(J2_HDQ==1),mean(J2_HDQ==2),mean(J2_HDQ==3),mean(J2_HDQ==4),mean(J2_HDQ==5),mean(J2_HDQ==6),mean(J2_HDQ==7),mean(J2_HDQ==8))
    J_RQLss = c(mean(J2_RQL==1),mean(J2_RQL==2),mean(J2_RQL==3),mean(J2_RQL==4),mean(J2_RQL==5),mean(J2_RQL==6),mean(J2_RQL==7),mean(J2_RQL==8))
    J_HDQoss = c(mean(J2_HDQo==1),mean(J2_HDQo==2),mean(J2_HDQo==3),mean(J2_HDQo==4),mean(J2_HDQo==5),mean(J2_HDQo==6),mean(J2_HDQo==7),mean(J2_HDQo==8))
  
    sim_resulti[i, ] = c(J_BQLss,mean(u_BQL),mean(uc_BQL),J_HDQss,mean(u_HDQ),mean(uc_HDQ),J_RQLss,mean(u_RQL),mean(uc_RQL),J_HDQoss,mean(u_HDQo),mean(uc_HDQo),mean(u_RQLo),mean(uc_RQLo))
  }
  sim_resulti
}
stopCluster(cl)

sim_result <- array(unlist(sim_result), dim = c(num_J, 42, num_sim))

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                                row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                                col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                                matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))

write.csv(long_format_data, file = "result_S14.csv", row.names = FALSE)

sim_result2 = apply(sim_result, c(1, 2), mean)
print(sim_result2)