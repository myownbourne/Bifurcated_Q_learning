library(ggplot2)
library(gridExtra)
library(latex2exp)
library(reshape2)
library(cowplot)     
library(patchwork)   

setting = 1
if(setting==1){
  read_data1 = read.csv("result_S11_tpr_fpr_5000.csv")
  sim_result = array(data = read_data1$value, 
                     dim = c(max(read_data1$row), 
                             max(read_data1$col), 
                             max(read_data1$matrix)))
  
  sim_result2 = apply(sim_result, c(1, 2), function(x) mean(x, na.rm = TRUE))
  sim_result2[is.nan(sim_result2)] <- NA
  
  xx = c(0.8,1.2,1.5,2,3,4,5,6)
  num_J = length(xx)
  
  data1 = data.frame("x" = xx,
                     "BQL TPR" = sim_result2[3:10,1],
                     "BQL FPR" = sim_result2[3:10,2],
                     "HDQg TPR" = sim_result2[3:10,3]+0.005,
                     "HDQg FPR" = sim_result2[3:10,4],
                     "RQLg TPR" = sim_result2[3:10,5]+0.01,
                     "RQLg FPR" = sim_result2[3:10,6],
                     "HDQ TPR" = sim_result2[3:10,7]-0.005,
                     "HDQ FPR" = sim_result2[3:10,8],
                     "RQL TPR" = sim_result2[3:10,9]-0.01,
                     "RQL FPR" = sim_result2[3:10,10])
  
  my_data1 = melt(data1, id = "x")
  colnames(my_data1) = c("CJ2", "method", "value")
  
  my_data1$value[is.nan(my_data1$value)] <- NA
  my_data_line <- subset(my_data1, is.finite(value))
  
  texnames = c("BQL TPR","BQL FPR",
               "HDQg TPR","HDQg FPR",
               "RQLg TPR","RQLg FPR",
               "HDQ TPR","HDQ FPR",
               "RQL TPR","RQL FPR")
  
  
  plot1 = ggplot(data = my_data1,
                 aes(x = CJ2, y = value, 
                     group = method, color = method, shape = method)) +
    geom_point(data = my_data_line, size = 1.8) +
    geom_line(data = my_data_line, aes(linetype = method), size = 0.75) +
    scale_shape_manual(values = c(4,4,2,2,1,1,8,8,0,0),
                       labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,2,1,2,1,2,1,2,1,2),
                          labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228",
                                  "#0C4E9B","#0C4E9B",
                                  "#F98F34","#F98F34",
                                  "#1F1F1F","#1F1F1F",
                                  "#1B9E77","#1B9E77"),
                       labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0.8,6), breaks = seq(1,6,1)) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  
  legend_plot1 = ggplot(data = my_data1,
                        aes(x = CJ2, y = value, 
                            group = method, color = method, shape = method)) +
    geom_point(data = my_data_line, size = 1.8) +
    geom_line(data = my_data_line, aes(linetype = method), size = 0.75) +
    scale_shape_manual(values = c(4,4,2,2,1,1,8,8,0,0),
                       labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,2,1,2,1,2,1,2,1,2),
                          labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228",
                                  "#0C4E9B","#0C4E9B",
                                  "#F98F34","#F98F34",
                                  "#1F1F1F","#1F1F1F",
                                  "#1B9E77","#1B9E77"),
                       labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0.8,6), breaks = seq(1,6,1)) +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  final_plot11 <- (wrap_elements(legend1_grob)) / (plot1) +
    plot_layout(heights = c(2,10))
  
  print(final_plot11)
}

if(setting==2){
  read_data1 = read.csv("result_S12_tpr_fpr_5000.csv")
  sim_result = array(data = read_data1$value, 
                     dim = c(max(read_data1$row), 
                             max(read_data1$col), 
                             max(read_data1$matrix)))
  
  sim_result2 = apply(sim_result, c(1, 2), function(x) mean(x, na.rm = TRUE))
  sim_result2[is.nan(sim_result2)] <- NA
  
  xx = c(0.4,0.8,1.2,2,2.8,3.6,4.4,5.2,6,7,8,9,10)
  num_J = length(xx)
  
  data1 = data.frame("x" = xx,
                     "BQL TPR" = sim_result2[-1,1],
                     "BQL FPR" = sim_result2[-1,2]-0.005,
                     "HDQg TPR" = sim_result2[-1,3]-0.005,
                     "HDQg FPR" = sim_result2[-1,4]+0.005,
                     "RQLg TPR" = sim_result2[-1,5]-0.01,
                     "RQLg FPR" = sim_result2[-1,6],
                     "HDQ TPR" = sim_result2[-1,7]+0.005,
                     "HDQ FPR" = sim_result2[-1,8],
                     "RQL TPR" = sim_result2[-1,9]+0.01,
                     "RQL FPR" = sim_result2[-1,10])
  
  my_data1 = melt(data1, id = "x")
  colnames(my_data1) = c("CJ2", "method", "value")
  
  my_data1$value[is.nan(my_data1$value)] <- NA
  my_data_line <- subset(my_data1, is.finite(value))
  
  texnames = c("BQL TPR","BQL FPR",
               "HDQg TPR","HDQg FPR",
               "RQLg TPR","RQLg FPR",
               "HDQ TPR","HDQ FPR",
               "RQL TPR","RQL FPR")
  
  
  plot1 = ggplot(data = my_data1,
                 aes(x = CJ2, y = value, 
                     group = method, color = method, shape = method)) +
    geom_point(data = my_data_line, size = 1.8) +
    geom_line(data = my_data_line, aes(linetype = method), size = 0.75) +
    scale_shape_manual(values = c(4,4,2,2,1,1,8,8,0,0),
                       labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,2,1,2,1,2,1,2,1,2),
                          labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228",
                                  "#0C4E9B","#0C4E9B",
                                  "#F98F34","#F98F34",
                                  "#1F1F1F","#1F1F1F",
                                  "#1B9E77","#1B9E77"),
                       labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  
  legend_plot1 = ggplot(data = my_data1,
                        aes(x = CJ2, y = value, 
                            group = method, color = method, shape = method)) +
    geom_point(data = my_data_line, size = 1.8) +
    geom_line(data = my_data_line, aes(linetype = method), size = 0.75) +
    scale_shape_manual(values = c(4,4,2,2,1,1,8,8,0,0),
                       labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,2,1,2,1,2,1,2,1,2),
                          labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228",
                                  "#0C4E9B","#0C4E9B",
                                  "#F98F34","#F98F34",
                                  "#1F1F1F","#1F1F1F",
                                  "#1B9E77","#1B9E77"),
                       labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  final_plot11 <- (wrap_elements(legend1_grob)) / (plot1) +
    plot_layout(heights = c(1.5,10))
  
  print(final_plot11)
}

if(setting==22){
  read_data1 = read.csv("result_S12_tpr_fpr_5000.csv")
  sim_result = array(data = read_data1$value, 
                     dim = c(max(read_data1$row), 
                             max(read_data1$col), 
                             max(read_data1$matrix)))
  
  sim_result2 = apply(sim_result, c(1, 2), function(x) mean(x, na.rm = TRUE))
  sim_result2[is.nan(sim_result2)] <- NA
  
  xx = c(0.4,0.8,1.2,2,2.8,3.6,4.4,5.2,6,7,8,9,10)
  num_J = length(xx)
  
  data1 = data.frame("x" = xx,
                     "BQL TPR" = sim_result2[-1,11]-0.005,
                     "BQL FPR" = sim_result2[-1,12],
                     "HDQg TPR" = sim_result2[-1,13]-0.01,
                     "HDQg FPR" = sim_result2[-1,14],
                     "RQLg TPR" = sim_result2[-1,15],
                     "RQLg FPR" = sim_result2[-1,16],
                     "HDQ TPR" = sim_result2[-1,17]+0.005,
                     "HDQ FPR" = sim_result2[-1,18],
                     "RQL TPR" = sim_result2[-1,19]+0.01,
                     "RQL FPR" = sim_result2[-1,20])
  
  my_data1 = melt(data1, id = "x")
  colnames(my_data1) = c("CJ2", "method", "value")
  
  my_data1$value[is.nan(my_data1$value)] <- NA
  my_data_line <- subset(my_data1, is.finite(value))
  
  texnames = c("BQL TPR","BQL FPR",
               "HDQg TPR","HDQg FPR",
               "RQLg TPR","RQLg FPR",
               "HDQ TPR","HDQ FPR",
               "RQL TPR","RQL FPR")
  
  
  plot1 = ggplot(data = my_data1,
                 aes(x = CJ2, y = value, 
                     group = method, color = method, shape = method)) +
    geom_point(data = my_data_line, size = 1.8) +
    geom_line(data = my_data_line, aes(linetype = method), size = 0.75) +
    scale_shape_manual(values = c(4,4,2,2,1,1,8,8,0,0),
                       labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,2,1,2,1,2,1,2,1,2),
                          labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228",
                                  "#0C4E9B","#0C4E9B",
                                  "#F98F34","#F98F34",
                                  "#1F1F1F","#1F1F1F",
                                  "#1B9E77","#1B9E77"),
                       labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  
  legend_plot1 = ggplot(data = my_data1,
                        aes(x = CJ2, y = value, 
                            group = method, color = method, shape = method)) +
    geom_point(data = my_data_line, size = 1.8) +
    geom_line(data = my_data_line, aes(linetype = method), size = 0.75) +
    scale_shape_manual(values = c(4,4,2,2,1,1,8,8,0,0),
                       labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,2,1,2,1,2,1,2,1,2),
                          labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228",
                                  "#0C4E9B","#0C4E9B",
                                  "#F98F34","#F98F34",
                                  "#1F1F1F","#1F1F1F",
                                  "#1B9E77","#1B9E77"),
                       labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  final_plot11 <- (wrap_elements(legend1_grob)) / (plot1) +
    plot_layout(heights = c(1.5,10))
  
  print(final_plot11)
}