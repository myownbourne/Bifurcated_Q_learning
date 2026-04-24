library(SuperLearner)
library(sparsegl)
library(foreach)
library(doParallel)
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


cl = makeCluster(24)
registerDoParallel(cl)

num_sim = 200
num_a2 = 11
#Learners = c("SL.glm")
Learners = c("SL.glm","SL.ranger")

sim_result = foreach(j = 1:num_sim, .combine = 'c') %dopar% {
#for(j in 1:num_sim){
  print(j)
  set.seed(2025*j)
  library(sparsegl)
  library(SuperLearner)
  library(ncvreg)
  
  sim_resulti = matrix(0, nrow = num_a2, ncol = 15)
  
  n = 500; d1 = 5; d2 = 5; d = 1+d1+d2;
  
  #define regression parameter
  beta_A1 = rep(0,d1); beta_A1[1:2] = c(1,1);
  beta_A2 = rep(0,d); beta_A2[1:2] = c(1,1); beta_A2[7:8] = c(1,1);
  beta_y1 = rep(0,d1);  beta_y1[1:3] = c(1,1,0.5); beta_y1[5] = 0.5; 
  beta_y1A = rep(0,d1); beta_y1A[1:3] = c(1,1,0.5); beta_y1A[5] = 0.5; 
  beta_y2 = rep(0,d);   beta_y2[c(8:9)] = c(1,0.5); beta_y2[11] = 0.5;
  beta_y2A = rep(0,d);  beta_y2A[c(8:9)] = c(1,0.5); beta_y2A[11] = 0.5;
  
  S1 = matrix(rnorm(n*d1),nrow = n)
  A1 = rbinom(n, 1, 1/(1+exp(-S1%*%beta_A1)))
  S2 = get_S2(S1,A1); S = cbind(S1,A1,S2)
  A2 = rbinom(n, 1, 1/(1+exp(-S%*%beta_A2)))
  y = S1%*%beta_y1 + S%*%beta_y2 + A1*S1%*%beta_y1A + A2*S%*%beta_y2A + 0.5*rnorm(n)
  parms = list(beta_y1,beta_y2,beta_y1A,beta_y2A)
  
  n_test = 5000; S1_new = matrix(rnorm(n_test*d1),nrow = n_test)
  
  J21 = seq(from = 2, to = d2-1, by = 1)
  J22 = seq(from = 2, to = d2, by = 1)
  J2s = list(J21,J22)
  L2 = c(1)
  
  J11 = seq(from = 2, to = d1 - 1, by = 1) 
  J12 = seq(from = 2, to = d1, by = 1)
  L1 = c(1)
  J1s = list(J11,J12)
  
  #add
  JJ11 = seq(from = 2, to = d1, by = 1)
  JJ1 = list(JJ11)
  JJ21 = seq(from = 2, to = d2 - 1, by = 1)
  JJ22 = c(d2)
  JJ2 = list(JJ21,JJ22)
  
  CJ1s = c(0,0)
  CJ2s = c(0,0)
  CJJ1 = c(0,0)
  CJJ2 = c(0,0)
  C1s = c(0,0)
  a2s = seq(from = 0, to = 15, length.out = num_a2)

  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  RQLo_res = RQLo(S1,S2,A1,A2,y,Learners)
  
  for(i in 1:num_a2){
    set.seed((123456+i)*j)
    C2s = c(7.5,a2s[i])
    
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
    
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners)
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
    
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms)

    HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
    RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms)
    
    a2_BQL = BQL_reg[[4]]; u_BQL = BQL_reg[[5]]; uc_BQL = u_BQL
    idxa20_BQL = which(a2_BQL==0); idxa21_BQL = which(a2_BQL==1); 
    uc_BQL[idxa20_BQL] = uc_BQL[idxa20_BQL] - C2s[1]
    uc_BQL[idxa21_BQL] = uc_BQL[idxa21_BQL] - C2s[2]
    
    a2_HDQ = HDQ_reg[[2]]; u_HDQ = HDQ_reg[[3]]; uc_HDQ = u_HDQ
    idxa20_HDQ = which(a2_HDQ==0); idxa21_HDQ = which(a2_HDQ==1); 
    uc_HDQ[idxa20_HDQ] = uc_HDQ[idxa20_HDQ] - C2s[1]
    uc_HDQ[idxa21_HDQ] = uc_HDQ[idxa21_HDQ] - C2s[2]
    
    a2_RQL = RQL_reg[[2]]; u_RQL = RQL_reg[[3]]; uc_RQL = u_RQL
    idxa20_RQL = which(a2_RQL==0); idxa21_RQL = which(a2_RQL==1); 
    uc_RQL[idxa20_RQL] = uc_RQL[idxa20_RQL] - C2s[1]
    uc_RQL[idxa21_RQL] = uc_RQL[idxa21_RQL] - C2s[2]

    a2_HDQo = HDQo_reg[[2]]; u_HDQo = HDQo_reg[[3]]; uc_HDQo = u_HDQo
    idxa20_HDQo = which(a2_HDQo==0); idxa21_HDQo = which(a2_HDQo==1); 
    uc_HDQo[idxa20_HDQo] = uc_HDQo[idxa20_HDQo] - C2s[1]
    uc_HDQo[idxa21_HDQo] = uc_HDQo[idxa21_HDQo] - C2s[2]

    a2_RQLo = RQLo_reg[[2]]; u_RQLo = RQLo_reg[[3]]; uc_RQLo = u_RQLo
    idxa20_RQLo = which(a2_RQLo==0); idxa21_RQLo = which(a2_RQLo==1); 
    uc_RQLo[idxa20_RQLo] = uc_RQLo[idxa20_RQLo] - C2s[1]
    uc_RQLo[idxa21_RQLo] = uc_RQLo[idxa21_RQLo] - C2s[2]

    sim45 = c(mean(a2_HDQo==1),mean(u_HDQo),mean(uc_HDQo),mean(a2_RQLo==1),mean(u_RQLo),mean(uc_RQLo))
    sim_resulti[i, ] = c(mean(a2_BQL==1),mean(u_BQL),mean(uc_BQL),mean(a2_HDQ==1),mean(u_HDQ),mean(uc_HDQ),mean(a2_RQL==1),mean(u_RQL),mean(uc_RQL),sim45)
  }
  sim_resulti
}
stopCluster(cl)
sim_result <- array(unlist(sim_result), dim = c(num_a2, 15, num_sim))

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                                row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                                col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                                matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))

write.csv(long_format_data, file = "result_S15.csv", row.names = FALSE)

sim_result2 = apply(sim_result, c(1, 2), mean)
print(sim_result2)