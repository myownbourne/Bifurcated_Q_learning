library(ggplot2)
set.seed(2025)

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
n <- nrow(S1_all)

BQL_res = readRDS("BQL_res.rds")
alphas_BQL = BQL_res[[1]]; betas_BQL = BQL_res[[2]]; gammas_BQL = BQL_res[[3]]; deltas_BQL = BQL_res[[4]]

s = 1

if (s == 1) {
  j1_grid <- 1:6
  a1_grid <- 0:1
  j2_grid <- 1:6
  n_combo <- length(j1_grid) * length(a1_grid) * length(j2_grid)
  abs_delta_mat <- matrix(NA_real_, nrow = n, ncol = n_combo)
  cc <- 1
  for (k1 in j1_grid) {
    for (k2 in j2_grid) {
      SJ2_b = cbind(S1_all[,L1],S1_all[,J1s[[k1]]],S2_all[,L2],S2_all[,J2s[[k2]]],rep(1,n))
      for (a1 in a1_grid) {
        alpha_hat <- alphas_BQL[[k1]][[k2]][[a1 + 1]]$alpha_hat_a1k1k2
        abs_delta_mat[, cc] <- abs(as.vector(SJ2_b %*% alpha_hat))
        cc <- cc + 1
      }
    }
  }
  
  # t grid
  positive_vals <- abs_delta_mat[abs_delta_mat > 0]
  t_grid <- exp(seq(log(min(positive_vals)), log(quantile(positive_vals, 1)), length.out = 500))
  #t_grid <- exp(seq(log(min(positive_vals)), log(max(positive_vals)), length.out = 500))
  
  
  # max freq
  max_freq <- sapply(t_grid, function(t0) {
    freq_each <- colMeans(abs_delta_mat < t0)
    max(freq_each)
  })
  
  df_plot <- data.frame(
    t = t_grid,
    p = max_freq
  )
  
  df_plot <- subset(df_plot, p > 0)
  
  x1 = -10; y1 = -7.4; x_min = -12; x_max = -2.8
  y_start = 1 * (x_min - x1) + y1
  y_end   = 1 * (x_max - x1) + y1
  
  ggplot(df_plot, aes(x = log(t), y = log(p))) +
    geom_step(direction = "hv", linewidth = 0.8) +
    annotate("segment", x = x_min, y = y_start, xend = x_max, yend = y_end, 
             color = "red", linewidth = 1.2, linetype = "dashed")+
    annotate("text", x = x_max, y = y_end, 
             label = "Slope = 1", 
             color = "red", 
             size = 6,
             fontface = "bold",
             hjust = 1.5, vjust = 1) +
    #labs(
    #  x = "log(t)",
    #  y = "log max empirical probability"
    #) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.title = element_text(size = 16, face = "bold"), 
      axis.text = element_text(size = 20),
    )
}

if (s == 2) {
  k1_grid <- 1:6
  k2_grid <- 1:6
  k22_grid <- 1:6
  a1_grid <- 0:1
  
  n_combo_max <- length(k1_grid) * length(k2_grid) * length(k22_grid) * length(a1_grid)
  abs_diff_mat <- matrix(NA_real_, nrow = n, ncol = n_combo_max)
  
  cc <- 1
  
  for (k1 in k1_grid) {
    S_L2b = cbind(S1_all[,L1],S1_all[,J1s[[k1]]],S2_all[,L2],rep(1,n));
    for (k2 in k2_grid) {
      for (k22 in k22_grid) {
        if (k2 != k22) {
          for (a1 in a1_grid) {
            idx_a1 <- a1 + 1
            beta1 <- betas_BQL[[k1]][[k2]][[idx_a1]]$beta_hat_a1k1k2
            beta2 <- betas_BQL[[k1]][[k22]][[idx_a1]]$beta_hat_a1k1k2
            Sbeta1 <- as.vector(S_L2b %*% beta1)
            Sbeta2 <- as.vector(S_L2b %*% beta2)
            abs_diff_mat[, cc] <- abs(Sbeta1 - Sbeta2)
            cc <- cc + 1
          }
        }
      }
    }
  }
  abs_diff_mat <- abs_diff_mat[, 1:(cc - 1), drop = FALSE]
  
  positive_vals <- abs_diff_mat[abs_diff_mat > 0]
  t_grid <- exp(seq(log(min(positive_vals)), log(quantile(positive_vals, 1)), length.out = 500))
  
  max_freq <- sapply(t_grid, function(t0) {
    freq_each <- colMeans(abs_diff_mat < t0)
    max(freq_each)
  })
  
  df_plot <- data.frame(
    t = t_grid,
    p = max_freq
  )
  
  df_plot <- subset(df_plot, p > 0)
  
  x1 = -15; y1 = -8; x_min = -15; x_max = -6.5
  y_start = 1 * (x_min - x1) + y1
  y_end   = 1 * (x_max - x1) + y1
  
  ggplot(df_plot, aes(x = log(t), y = log(p))) +
    geom_line(linewidth = 0.9) +
    annotate("segment",
             x = x_min, y = y_start,
             xend = x_max, yend = y_end,
             color = "red", linewidth = 1.2, linetype = "dashed") +
    annotate("text",
             x = x_max, y = y_end,
             label = "Slope = 1",
             color = "red",
             fontface = "bold",
             size = 6,
             hjust = 1.5, vjust = 1) +
    #labs(
    #  x = "log(t)",
    #  y = "log max empirical probability"
    #) +
    labs(x = NULL, y = NULL) +
    #theme_bw() +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 20)
    )
}

if (s == 3) {
  n <- nrow(S1_all)
  k1_grid <- 1:6
  
  abs_gamma_mat <- matrix(NA_real_, nrow = n, ncol = length(k1_grid))
  
  cc <- 1
  for (k1 in k1_grid) {
    gamma_BQL_k1 <- gammas_BQL[[k1]]$gamma_hat_k1
    SJ1_b = cbind(S1_all[,L1],S1_all[,J1s[[k1]]],rep(1,n))
    Sgamma <- as.vector(SJ1_b %*% gamma_BQL_k1)
    abs_gamma_mat[, cc] <- abs(Sgamma)
    cc <- cc + 1
  }
  
  positive_vals <- abs_gamma_mat[abs_gamma_mat > 0]
  t_grid <- exp(seq(log(min(positive_vals)), log(quantile(positive_vals, 1)), length.out = 500))
  
  max_freq <- sapply(t_grid, function(t0) {
    freq_each <- colMeans(abs_gamma_mat < t0)
    max(freq_each)
  })
  
  df_plot <- data.frame(
    t = t_grid,
    p = max_freq
  )
  
  df_plot <- subset(df_plot, p > 0)
  
  x1 = -10; y1 = -6.8; x_min = -12; x_max = -3
  y_start = 1 * (x_min - x1) + y1
  y_end   = 1 * (x_max - x1) + y1
  
  ggplot(df_plot, aes(x = log(t), y = log(p))) +
    geom_line(linewidth = 0.9) +
    annotate("segment",
             x = x_min, y = y_start,
             xend = x_max, yend = y_end,
             color = "red", linewidth = 1.2, linetype = "dashed") +
    annotate("text",
             x = x_max, y = y_end,
             label = "Slope = 1",
             color = "red",
             size = 6,
             fontface = "bold",
             hjust = 1.5, vjust = 1) +
    #labs(
    #  x = "log(t)",
    #  y = "log max empirical probability"
    #) +
    #theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 20)
    )
}

if (s == 4) {
  
  library(ggplot2)
  
  n <- nrow(S1_all)
  k1_grid <- 1:6
  k12_grid <- 1:6
  
  n_combo_max <- length(k1_grid) * length(k12_grid)
  abs_delta_mat <- matrix(NA_real_, nrow = n, ncol = n_combo_max)
  
  SL1 = cbind(S1_all[,L1],rep(1,n))
  
  cc <- 1
  for (k1 in k1_grid) {
    for (k12 in k12_grid) {
      if (k1 != k12) {
        delta_BQL_k1  <- deltas_BQL[[k1]]$delta_hat_k1
        delta_BQL_k12 <- deltas_BQL[[k12]]$delta_hat_k1
        
        Sdelta1 <- as.vector(SL1 %*% delta_BQL_k1)
        Sdelta2 <- as.vector(SL1 %*% delta_BQL_k12)
        
        abs_delta_mat[, cc] <- abs(Sdelta1 - Sdelta2)
        cc <- cc + 1
      }
    }
  }
  
  abs_delta_mat <- abs_delta_mat[, 1:(cc - 1), drop = FALSE]
  
  positive_vals <- abs_delta_mat[abs_delta_mat > 0]
  t_grid <- exp(seq(log(min(positive_vals)), log(quantile(positive_vals, 1)), length.out = 500))
  
  max_freq <- sapply(t_grid, function(t0) {
    freq_each <- colMeans(abs_delta_mat < t0)
    max(freq_each)
  })
  
  df_plot <- data.frame(
    t = t_grid,
    p = max_freq
  )
  
  df_plot <- subset(df_plot, p > 0)
  
  x1 = -12; y1 = -6.8; x_min = -14; x_max = -5
  y_start = 1 * (x_min - x1) + y1
  y_end   = 1 * (x_max - x1) + y1
  
  ggplot(df_plot, aes(x = log(t), y = log(p))) +
    geom_line(linewidth = 0.9) +
    annotate("segment",
             x = x_min, y = y_start,
             xend = x_max, yend = y_end,
             color = "red", linewidth = 1.2, linetype = "dashed") +
    annotate("text",
             x = x_max, y = y_end,
             label = "Slope = 1",
             color = "red",
             size = 6,
             fontface = "bold",
             hjust = 1.5, vjust = 1) +
    #labs(
    #  x = "log(t)",
    #  y = "log max empirical probability"
    #) +
    labs(x = NULL, y = NULL) +
    #theme_bw() +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 20)
    )
}