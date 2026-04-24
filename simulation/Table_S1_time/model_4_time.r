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


cl = makeCluster(25)
registerDoParallel(cl)

xx = c(1)
num_J = length(xx)

num_sim = 50
Learners = c("SL.glm","SL.ranger")
#Learners = c("SL.glm")

sim_result = foreach(j = 1:num_sim, .combine = 'c') %dopar% {
  #for(j in 1:num_sim){
  print(j)
  set.seed(2024*j)
  library(ncvreg)
  library(SuperLearner)
  library(sparsegl)
  
  sim_resulti = matrix(0, nrow = num_J, ncol = 5)
  
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

  xx = c(1)
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

  t0 <- Sys.time()
  HDQo_res = HDQo(S1,S2,A1,A2,y) 
  t1 <- Sys.time() 
  HDQo_t = as.numeric(difftime(t1, t0, units = "secs"))
  HDQo_reg = HDQo_regimes(HDQo_res[[1]],HDQo_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)

  t0 <- Sys.time()
  RQLo_res = RQLo(S1,S2,A1,A2,y,Learners)
  t1 <- Sys.time() 
  RQLo_t = as.numeric(difftime(t1, t0, units = "secs"))
  RQLo_reg = RQLo_regimes(RQLo_res[[1]],RQLo_res[[2]],S1_new,C1s,C2s,parms)
  
  for(i in 1:num_J){
    set.seed((123456+i)*j)
    CJ2s = c(0,CJ22_set[i],CJ23_set[i],CJ24_set[i],CJ25_set[i],CJ26_set[i],CJ27_set[i],CJ28_set[i])
    CJ1s = c(0)
    
    CJJ1 = c(0)
    CJJ2 = c(CJ22_set[i],CJ22_set[i],CJ22_set[i])

    t0 <- Sys.time() 
    HDQ_res = HDQ_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2)
    t1 <- Sys.time() 
    HDQg_t = as.numeric(difftime(t1, t0, units = "secs"))
    HDQ_reg = HDQ_glasso_regimes(HDQ_res[[1]],HDQ_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)

    t0 <- Sys.time() 
    RQL_res = RQL_glasso(S1,S2,A1,A2,y,JJ1,JJ2,CJJ1,CJJ2,Learners)
    t1 <- Sys.time() 
    RQLg_t = as.numeric(difftime(t1, t0, units = "secs"))
    RQL_reg = RQL_gLasso_regimes(RQL_res[[1]],RQL_res[[2]],S1_new,J1s,J2s,L1,L2,C1s,C2s,parms)

    t0 <- Sys.time() 
    BQL_res = BQL_4(S1,S2,A1,A2,y,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
    t1 <- Sys.time() 
    BQL_t = as.numeric(difftime(t1, t0, units = "secs"))
    BQL_reg = BQL_regimes_4(BQL_res[[1]],BQL_res[[2]],BQL_res[[3]],BQL_res[[4]],S1_new,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,parms)
    
    
    sim_resulti[i,] = c(HDQo_t, RQLo_t, HDQg_t, RQLg_t, BQL_t)

  }
  sim_resulti
}
stopCluster(cl)

sim_result <- array(unlist(sim_result), dim = c(num_J, 5, num_sim))

result_matrix = sim_result
long_format_data = data.frame(value = c(result_matrix), 
                                row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
                                col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
                                matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))

write.csv(long_format_data, file = "result_S14_time.csv", row.names = FALSE)

sim_result2 = apply(sim_result, c(1, 2), mean)
print(sim_result2)