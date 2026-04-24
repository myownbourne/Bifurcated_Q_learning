library(SuperLearner)
library(randomForest)
library(foreach)
library(doParallel)
library(ncvreg)
library(glmnet)
library(ranger)
library(xgboost)
source("methods_binY2.r")
print(getwd()) 
set.seed(2025)

num_cores = 2
cl = makeCluster(num_cores)
registerDoParallel(cl)

S1_data = read.csv("S1_24_24_comp2.csv")
S2_data = read.csv("S2_24_24_comp2.csv")
Y_data = read.csv("y_24_24_comp2.csv")

median_A1 = median(S1_data$input_4hourly, na.rm = TRUE)
A1_all = ifelse(S1_data$input_4hourly > median_A1, 1, 0)
median_A2 = median(S2_data$input_4hourly, na.rm = TRUE)
A2_all = ifelse(S2_data$input_4hourly > median_A2, 1, 0)

indexA11 = which(S1_data$input_4hourly>median_A1); input_11 = mean(S1_data$input_4hourly[indexA11]) 
indexA10 = which(S1_data$input_4hourly<=median_A1); input_10 = mean(S1_data$input_4hourly[indexA10]) 
indexA21 = which(S2_data$input_4hourly>median_A2); input_21 = mean(S2_data$input_4hourly[indexA21]) 
indexA20 = which(S2_data$input_4hourly<=median_A2); input_20 = mean(S2_data$input_4hourly[indexA20]) 

colbin  = c('gender','re_admission','mechvent')
colnorm = c('age','Weight_kg','GCS','HR','SysBP','MeanBP','DiaBP','RR','Temp_C','FiO2_1',
            'Potassium','Sodium','Chloride','Glucose','Magnesium','Calcium',
            'Hb','WBC_count','Platelets_count','PTT','PT','Arterial_pH','paO2','paCO2',
            'Arterial_BE','Arterial_lactate','HCO3','Shock_Index','PaO2_FiO2','cumulated_balance','SOFA','SIRS')
collog  = c('SpO2','BUN','Creatinine','SGOT','SGPT','Total_bili','INR','max_dose_vaso','input_total','output_total','output_4hourly')
#delete 'input_4hourly'

colbin2  = c('mechvent')
colnorm2 = c('Weight_kg','GCS','HR','SysBP','MeanBP','DiaBP','RR','Temp_C','FiO2_1',
             'Potassium','Sodium','Chloride','Glucose','Magnesium','Calcium',
             'Hb','WBC_count','Platelets_count','PTT','PT','Arterial_pH','paO2','paCO2',
             'Arterial_BE','Arterial_lactate','HCO3','Shock_Index','PaO2_FiO2','cumulated_balance','SOFA','SIRS')
collog2  = c('SpO2','BUN','Creatinine','SGOT','SGPT','Total_bili','INR','max_dose_vaso','input_total','output_total','output_4hourly')

colall1 = c(colbin,colnorm,collog)
colall2 = c(colbin2,colnorm2,collog2)

c12 = match(colnorm, names(S1_data))
c13 = match(collog, names(S1_data))

c22 = match(colnorm2, names(S2_data))
c23 = match(collog2, names(S2_data))

S1_data[, c12] = as.data.frame(lapply(S1_data[,c12], function(x) {(x - mean(x)) / sd(x)}))
S1_data[, c13] = as.data.frame(lapply(S1_data[,c13], function(x) {
  x_log <- log(0.1 + x)  
  (x_log - mean(x_log)) / sd(x_log)  
}))
S2_data[, c22] = as.data.frame(lapply(S2_data[,c22], function(x) {(x - mean(x)) / sd(x)}))
S2_data[, c23] = as.data.frame(lapply(S2_data[,c23], function(x) {
  x_log <- log(0.1 + x)  
  (x_log - mean(x_log)) / sd(x_log)  
}))


colall1 = c(colbin,colnorm,collog)
call1 = match(colall1, names(S1_data))
colall2 = c(colbin2,colnorm2,collog2)
call2 = match(colall2, names(S2_data))

S1_all = as.matrix(S1_data[,call1])
S2_all = as.matrix(S2_data[,call2])
y_all = as.matrix(Y_data[,3]); y_all = 1-y_all;

num_sim = 1
num_lam = 1
print(250621)

for(i in 1:num_sim){
#sim_result = foreach(i = 1:num_sim, .combine = 'c') %dopar% {
  library(SuperLearner)
  library(randomForest)
  library(xgboost)
  library(ncvreg)
  library(ranger)
  library(glmnet)
  set.seed(250621*i)
  #sim_resulti = matrix(data = 0, nrow = num_lam, ncol = 36)
  n_all = nrow(S1_all); n = 14000;
  train_indices = sample(1:n_all, n)
  S1 = S1_all[train_indices,]; S2 = S2_all[train_indices,]; A1 = A1_all[train_indices]; A2 = A2_all[train_indices]; y = y_all[train_indices]
  S1_test = S1_all[-train_indices,]; S2_test = S2_all[-train_indices,]; A1_test = A1_all[-train_indices]; A2_test = A2_all[-train_indices]; y_test = y_all[-train_indices,]
  
  t1_name = c('Potassium','Sodium','Chloride','Glucose','Magnesium','Calcium','BUN','Creatinine','SGOT','SGPT','Total_bili')
  t2_name = c('Hb','WBC_count','Platelets_count')
  t3_name = c('PTT','PT','INR')
  t4_name = c('Arterial_pH','paO2','paCO2','Arterial_BE','Arterial_lactate','HCO3','PaO2_FiO2')
  SOFA_name = c('SOFA')
  SIRS_name = c('SIRS')
  tall_name = c(t1_name,t2_name,t3_name,t4_name,SOFA_name,SIRS_name)
  L1_name = setdiff(colall1, tall_name)
  
  L1 = match(L1_name,colall1)
  t11 = match(t1_name,colall1)
  t12 = match(t2_name,colall1)
  t13 = match(t3_name,colall1)
  t14 = match(t4_name,colall1)
  t1SOFA = match(SOFA_name,colall1)
  t1SIRS = match(SIRS_name,colall1)

  #67 24 22 255
  J11 = c() #0
  J12 = c(t12) #24
  J13 = c(t11,t12,t13) #113
  J14 = c(t14) #255
  J15 = c(t12,t14,t1SIRS) #279
  J16 = c(t11,t12,t13,t14,t1SOFA,t1SIRS) #368
  J1s = list(J11,J12,J13,J14,J15,J16)
  
  L2_name = setdiff(colall2, tall_name)
  L2 = match(L2_name,colall2)
  t21 = match(t1_name,colall2)
  t22 = match(t2_name,colall2)
  t23 = match(t3_name,colall2)
  t24 = match(t4_name,colall2)
  t2SOFA = match(SOFA_name,colall2)
  t2SIRS = match(SIRS_name,colall2)
  
  J21 = c()
  J22 = c(t22)
  J23 = c(t21,t22,t23)
  J24 = c(t24)
  J25 = c(t22,t24,t2SIRS)
  J26 = c(t21,t22,t23,t24,t2SOFA,t2SIRS)
  J2s = list(J21,J22,J23,J24,J25,J26)
  
  SL.ranger_50 <- function(...) {SL.ranger(..., num.trees = 100)}
  Learners <- c("SL.ranger_50", "SL.glm") 
  
  #RQL_res = RQL_new(S1,S2,A1,A2,y,Learners) 
  #HDQ_res = HDQ_new(S1,S2,A1,A2,y)
  
  #lams = c(0,0.0000001,0.000001,0.000002,0.000005,0.00001,0.00002,0.00005,0.0001)
  lams = c(0.00001)
  
  for(j in 1:length(lams)){
    set.seed((123321+i)*j)
    lam = lams[[j]]
    CJ1s = lam*6*c(0,24,113,255,279,368) #67 24 22 255
    CJ2s = lam*6*c(0,24,113,255,279,368)
    
    C1s = lam*6*c(0,(input_11-input_10))*0.3
    C2s = lam*6*c(0,(input_21-input_20))*0.3
    
    
    BQL_res = BQL_4(S1_all,S2_all,A1_all,A2_all,y_all,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,Learners) 
    saveRDS(BQL_res, file = "BQL_res.rds", compress = "xz")

    #SL.xgboost_100 <- function(...) {SL.xgboost(..., ntrees = 100)}
    #SL.ranger_100 <- function(...) {SL.ranger(..., num.trees = 100)}
    #Lrs <- c("SL.ranger_100", "SL.xgboost_100" ,"SL.glm", "SL.gam")

    
    #result = eva(S1_test,S2_test,A1_test,A2_test,y_test,J1s,J2s,L1,L2,C1s,CJ1s,C2s,CJ2s,RQL_res,HDQ_res,BQL_res,Lrs) 
    #result_BQL = result[[1]]; V_BQL = result_BQL[[1]]; P_BQL = result_BQL[[2]]; J1_BQL = result_BQL[[3]]; J2_BQL = result_BQL[[4]]; A1_BQL = result_BQL[[5]]; A2_BQL = result_BQL[[6]];
    #result_RQL = result[[2]]; V_RQL = result_RQL[[1]]; P_RQL = result_RQL[[2]]; A1_RQL = result_RQL[[3]]; A2_RQL = result_RQL[[4]];
    #result_HDQ = result[[3]]; V_HDQ = result_HDQ[[1]]; P_HDQ = result_HDQ[[2]]; A1_HDQ = result_HDQ[[3]]; A2_HDQ = result_HDQ[[4]]; J1_HDQ = result_HDQ[[5]]; J2_HDQ = result_HDQ[[6]];
    
    #cc1 = c(V_BQL,V_RQL,V_HDQ,P_BQL,P_RQL,P_HDQ)
    #cc2 = c(mean(J1_BQL==1),mean(J1_BQL==2),mean(J1_BQL==3),mean(J1_BQL==4),mean(J1_BQL==5),mean(J1_BQL==6))
    #cc3 = c(mean(J2_BQL==1),mean(J2_BQL==2),mean(J2_BQL==3),mean(J2_BQL==4),mean(J2_BQL==5),mean(J2_BQL==6),mean(A1_BQL==1),mean(A2_BQL==1))
    #cc4 = c(mean(A1_RQL==1),mean(A2_RQL==1))
    #cc5 = c(mean(J1_HDQ==1),mean(J1_HDQ==2),mean(J1_HDQ==3),mean(J1_HDQ==4),mean(J1_HDQ==5),mean(J1_HDQ==6))
    #cc6 = c(mean(J2_HDQ==1),mean(J2_HDQ==2),mean(J2_HDQ==3),mean(J2_HDQ==4),mean(J2_HDQ==5),mean(J2_HDQ==6),mean(A1_HDQ==1),mean(A2_HDQ==1))
    #sim_resulti[j,] = c(cc1,cc2,cc3,cc4,cc5,cc6)
  }
  #sim_resulti
}
#stopCluster(cl)

#sim_result = array(unlist(sim_result), dim = c(num_lam, 36, num_sim))
#sim_result2 = apply(sim_result, c(1, 2), mean)
#result_matrix = sim_result
#long_format_data = data.frame(value = c(result_matrix), 
#                                row = rep(1:dim(result_matrix)[1], times = dim(result_matrix)[2] * dim(result_matrix)[3]),
#                                col = rep(rep(1:dim(result_matrix)[2], each = dim(result_matrix)[1]), times = dim(result_matrix)[3]),
#                                matrix = rep(1:dim(result_matrix)[3], each = dim(result_matrix)[1] * dim(result_matrix)[2]))
#write.csv(long_format_data, file = "r.csv", row.names = FALSE)

#print(sim_result2[,1:6])
#print(sim_result2[,7:12])
#print(sim_result2[,13:20])
#print(sim_result2[,21:22])
#print(sim_result2[,23:28])
#print(sim_result2[,29:36])














