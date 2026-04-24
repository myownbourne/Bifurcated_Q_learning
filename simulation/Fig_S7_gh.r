library(SuperLearner)
library(foreach)
library(doParallel)
library(sparsegl)
library(ncvreg)
library(glmnet)
source("methods3_highd.r")
set.seed(1234567)
print(300400)

get_S2 = function(S1,A1)
{
  n = nrow(S1)
  d2 = ncol(S1)
  S2 = S1 + A1*S1 + matrix(rnorm(n*d2),nrow = n)
  return(S2)
}


num_sim = 200
#CJ2s_set = c(0,0.5,1,3,6,9,12)*0.1
#CJ2s_set = c(0,0.5,1,1.5,3,4.5,6,7.5,9)*0.1
CJ2s_set = c(0,0.4,0.8,1.2,1.6,2.3,3,4.5,5.25,6,6.75,7.5,8.25,9)*0.1
num_J = length(CJ2s_set)

sim_result = array(data = 0, dim = c(num_J, 15, num_sim))

cl = makeCluster(67)
registerDoParallel(cl)

print(111)
sim_result = foreach(i = 1:num_sim, .combine = 'c') %dopar% {
  print(i)
  library(sparsegl)
  library(ncvreg)
  library(glmnet)
  set.seed(2026*i)
  sim_resulti = matrix(data = 0, nrow = num_J, ncol = 15)
  n = 400; d1 = 300; d2 = 300; d = 1+d1+d2;
  
  beta_A1 = rep(0,d1); beta_A1[1:2] = c(1,1);
  beta_A2 = rep(0,d); beta_A2[1:2] = c(1,1); beta_A2[(d1+2):(d1+3)] = c(1,1);
  beta_y1 = rep(0,d1);  beta_y1[1:4] = c(1,1,1,1,1); 
  beta_y1A = rep(0,d1); beta_y1A[1:5] = c(1,1,1,1,1);
  beta_y2 = rep(0,d);   beta_y2[(d1+3):(d1+4)] = c(1,0.5); beta_y2[d] = 1;
  beta_y2A = rep(0,d);  beta_y2A[(d1+3):(d1+4)] = c(1,0.5); beta_y2A[d] = 1;
  
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
  CJ2s_set = c(0,0.4,0.8,1.2,1.6,2.3,3,4.5,5.25,6,6.75,7.5,8.25,9)*0.1

  #Learners = c("SL.glm","SL.ranger")
  #Learners = c("SL.glm")
  
  #add
  JJ11 = seq(from = 2, to = d1, by = 1)
  JJ1 = list(JJ11)
  JJ21 = seq(from = 2, to = d2 - 1, by = 1)
  JJ22 = c(d2)
  JJ2 = list(JJ21,JJ22)

  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
  
  RQLo_res = RQLo(S1,S2,A1,A2,y)
  RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms)
  
  
  for(j in 1:num_J){
    print(j)
    set.seed((123456+i)*j)
    CJ1s = c(0)
    CJ2s = c(0,CJ2s_set[j])
    CJJ1 = c(0)
    CJJ2 = c(0,CJ2s_set[j])
    
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
    
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)
    
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s) 
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms)
    
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

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                                row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                                col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                                matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))

write.csv(long_format_data, file = "result_S19_d300_n400.csv", row.names = FALSE)

sim_result2 = apply(sim_result, c(1, 2), mean)
print(sim_result2)
