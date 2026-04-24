library(ggplot2)
library(gridExtra)
library(latex2exp)
library(reshape2)
library(cowplot)     
library(patchwork)    
setting = 1

if(setting==1){
  read_data = read.csv("result_S11.csv")
  sim_result  = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result2 = apply(sim_result, c(1, 2), mean)
  xx = c(0,0.4,0.8,1.2,1.5,2,3,4,5,6)
  num_J = length(xx)
  data1 = data.frame("x" = xx,
                     "BQL j22" = sim_result2[,1],
                     "HDQg j22" = sim_result2[,4],
                     "RQLg j22" = sim_result2[,7],
                     "HDQ j22" = sim_result2[,10]-0.01,
                     "RQLg j22" = sim_result2[,13]+0.01)
  
  colnames(data1) <- gsub("\\.", " ", colnames(data1))
  colnames(data1) <- gsub("and", "&", colnames(data1))
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,3],'HDQg'= sim_result2[,6],'RQLg'= sim_result2[,9],'HDQ'= sim_result2[,12],'RQL'= sim_result2[,15])
  my_data1 = melt(data1,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("CJ2","method","value")
  colnames(my_data3) = c("CJ2","method","value")
  
  texnames = c( "BQL $j_{22}$", "HDQg $j_{22}$", "RQLg $j_{22}$", "HDQ $j_{22}$", "RQL $j_{22}$")
  
  
  plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#0C4E9B","#F98F34","#1F1F1F","#1B9E77"), labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+
    scale_y_continuous(limits = c(1.6,2.4),breaks = seq(1.6,2.4,0.2))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#0C4E9B","#F98F34","#1F1F1F","#1B9E77"), labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+
    scale_y_continuous(limits = c(1.6,2.4),breaks = seq(1.6,2.4,0.2))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  final_plot11 <- (wrap_elements(legend1_grob))/(plot1)  +
    plot_layout(heights = c(2,10))
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  
  final_plot13 <- (wrap_elements(legend3_grob))/(plot3)  +
    plot_layout(heights = c(2,10))
  
  #final_plot13
  final_plot11
}


if(setting==1.5){
  read_data = read.csv("result_S11n.csv")
  sim_result = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result2 = apply(sim_result, c(1, 2), mean)
  
  xx = c(100,200,300,1000,2000,5000)
  data1 = data.frame("x" = xx,
                     "BQL J22" = sim_result2[,1],
                     "HDQg J22" = sim_result2[,4]-0.001,
                     "RQLg J22" = sim_result2[,7]+0.001,
                     "HDQ J22" = sim_result2[,10]-0.002,
                     "RQL J22" = sim_result2[,13]+0.002)
  colnames(data1) <- gsub("\\.", " ", colnames(data1))
  colnames(data1) <- gsub("and", "&", colnames(data1))
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,3],'HDQg'= sim_result2[,6],'RQLg'= sim_result2[,9],'HDQ'= sim_result2[,12],'RQL'= sim_result2[,15])
  my_data1 = melt(data1,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("CJ2","method","value")
  colnames(my_data3) = c("CJ2","method","value")
  
  texnames = c("BQL $j_{22}$", "HDQg $j_{22}$", "RQLg $j_{22}$", "HDQ $j_{22}$", "RQL $j_{22}$")
  
  
  plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34", "#1F1F1F","#1B9E77"),labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  #scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.2))
  
  plot2 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34", "#1F1F1F","#1B9E77")) +
    scale_y_continuous(limits = c(2.15,2.45),breaks = seq(2.2,2.4,0.1))+
    labs(x = NULL, y = NULL)+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34", "#1F1F1F","#1B9E77"),labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot2 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34", "#1F1F1F","#1B9E77")) +
    scale_y_continuous(limits = c(2.1,2.45),breaks = seq(2.1,2.45,0.1))+
    labs(x = NULL, y = NULL)+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  final_plot11 <- (wrap_elements(legend1_grob))/(plot1)  +
    plot_layout(heights = c(2,10))
  
  legend2 <- cowplot::get_legend(legend_plot2)
  legend2_grob <- cowplot::as_grob(legend2)
  
  final_plot12 <- (wrap_elements(legend2_grob))/(plot2)  +
    plot_layout(heights = c(2,10))
  
  final_plot11
  #final_plot12
  
}

if(setting==2||setting==23||setting==25||setting==27){
  if(setting==2){read_data = read.csv("result_S13.csv")}
  if(setting==23){read_data = read.csv("result_S13_rho3.csv")}
  if(setting==25){read_data = read.csv("result_S13_rho5.csv")}
  if(setting==27){read_data = read.csv("result_S13_rho7.csv")}
  sim_result = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result2 = apply(sim_result, c(1, 2), mean)
  
  xx = c(0,0.04,0.08,0.12,0.16,0.2,0.25,0.3,0.4,0.5,0.6,0.7)/0.1
  num_J = length(xx)
  data1 = data.frame("x" = xx,
                     "BQL j21" = sim_result2[,1],
                     "BQL j22" = sim_result2[,2],
                     "BQL j23" = sim_result2[,3],
                     "HDQg j21" = sim_result2[,6],
                     "HDQg j22" = sim_result2[,7]+0.01,
                     "HDQg j23" = sim_result2[,8]+0.02,
                     "RQLg j21" = sim_result2[,11]-0.02,
                     "RQLg j22" = sim_result2[,12]-0.01,
                     "RQLg j23" = sim_result2[,13]-0.02,
                     "HDQo j23" = sim_result2[,18]-0.01,
                     "RQLo j23" = sim_result2[,21]+0.01)
  colnames(data1) <- gsub("\\.", " ", colnames(data1))
  colnames(data1) <- gsub("and", "&", colnames(data1))
  data2 = data.frame("x" = xx, "BQL" = sim_result2[,4],'HDQ'= sim_result2[,9],'RQL' = sim_result2[,14])
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,5],'HDQg'= sim_result2[,10],'RQLg' = sim_result2[,15],'HDQ'= sim_result2[,20],'RQL' = sim_result2[,23])
  my_data1 = melt(data1,id="x");my_data2 = melt(data2,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("ii","method","value"); colnames(my_data2) = c("ii","method","value"); colnames(my_data3) = c("ii","method","value")
  
  texnames = c("BQL $j_{21}$", "BQL $j_{22}$",  "BQL $j_{23}$","HDQg $j_{21}$","HDQg $j_{22}$", "HDQg $j_{23}$","RQLg $j_{21}$", "RQLg $j_{22}$","RQLg $j_{23}$","HDQ $j_{23}$","RQL $j_{23}$")
  
  
  plot1 = ggplot(data = my_data1,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,0,4,2,0,4,2,0,0,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(3,5,1,3,5,1,3,5,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#0C4E9B","#0C4E9B","#0C4E9B", "#F98F34","#F98F34","#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot1 = ggplot(data = my_data1,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,0,4,2,0,4,2,0,0,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(3,5,1,3,5,1,3,5,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#0C4E9B","#0C4E9B","#0C4E9B","#F98F34","#F98F34","#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 4, byrow = TRUE),
      shape = guide_legend(nrow = 4, byrow = TRUE),
      linetype = guide_legend(nrow = 4, byrow = TRUE)
    )
  
  if(setting==2){yy1 = c(1.4,2.9);yy2 = seq(1.4,2.9,0.3)}
  if(setting==23){yy1 = c(1.95,3.5);yy2 = seq(2,3.5,0.3)}
  if(setting==25){yy1 = c(2.35,3.9);yy2 = seq(2.4,3.9,0.3)}
  if(setting==27){yy1 = c(2.8,4.3);yy2 = seq(2.8,4.3,0.3)}
  
  plot3 = ggplot(data = my_data3,aes(x=ii,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    scale_y_continuous(limits = yy1,breaks = yy2)+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=ii,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = expression(lambda), y = TeX("Profit"))+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5), 
      #legend.justification = c("right", "center"),
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  
  final_plot <- (wrap_elements(legend1_grob))/(plot1)  +
    plot_layout(heights = c(2,10))
  
  final_plot3 <- (wrap_elements(legend3_grob))/(plot3)  +
    plot_layout(heights = c(2,10))
  
  
  print(final_plot)
  #print(final_plot3)
  #plot_grid(final_plot, final_plot3, nrow = 1)
}

if(setting==22){
  read_data1 = read.csv("result_S13_rho3.csv")
  read_data2 = read.csv("result_S13_rho5.csv")
  read_data3 = read.csv("result_S13_rho7.csv")
  sim_result1 = array(data = read_data1$value, dim = c(max(read_data1$row), max(read_data1$col), max(read_data1$matrix)))
  sim_result12 = apply(sim_result1, c(1, 2), mean)
  sim_result2 = array(data = read_data2$value, dim = c(max(read_data2$row), max(read_data2$col), max(read_data2$matrix)))
  sim_result22 = apply(sim_result2, c(1, 2), mean)
  sim_result3 = array(data = read_data3$value, dim = c(max(read_data3$row), max(read_data3$col), max(read_data3$matrix)))
  sim_result32 = apply(sim_result3, c(1, 2), mean)
  
  xx = c(0,0.04,0.08,0.12,0.16,0.2,0.25,0.3,0.4,0.5,0.6,0.7)/0.1
  num_J = length(xx)
  data1 = data.frame("x" = xx,
                     "BQL j21 (rho=0.3)" = sim_result12[,1],
                     "BQL j22 (rho=0.3)" = sim_result12[,2],
                     "BQL j23 (rho=0.3)" = sim_result12[,3],
                     "BQL j21 (rho=0.5)" = sim_result22[,1],
                     "BQL j22 (rho=0.5)" = sim_result22[,2],
                     "BQL j23 (rho=0.5)" = sim_result22[,3],
                     "BQL j21 (rho=0.7)" = sim_result32[,1],
                     "BQL j22 (rho=0.7)" = sim_result32[,2],
                     "BQL j23 (rho=0.7)" = sim_result32[,3]
  )
  data2 = data.frame("x" = xx,
                     "HDQg j21 (rho=0.3)" = sim_result12[,6],
                     "HDQg j21 (rho=0.5)" = sim_result22[,6],
                     "HDQg j21 (rho=0.7)" = sim_result32[,6],
                     "HDQg j23 (rho=0.3)" = sim_result12[,8],
                     "HDQg j23 (rho=0.5)" = sim_result22[,8],
                     "HDQg j23 (rho=0.7)" = sim_result32[,8],
                     "HDQg j22 (rho=0.3,0.5,0.7)" = sim_result32[,7]-0.01,
                     "HDQ j23 (rho=0.3,0.5,0.7)" = sim_result12[,18]+0.01)
  data3 = data.frame("x" = xx,
                     "RQLg j21 (rho=0.3)" = sim_result12[,11],
                     "RQLg j21 (rho=0.5)" = sim_result22[,11],
                     "RQLg j21 (rho=0.7)" = sim_result32[,11],
                     "RQLg j23 (rho=0.3)" = sim_result12[,13],
                     "RQLg j23 (rho=0.5)" = sim_result22[,13],
                     "RQLg j23 (rho=0.7)" = sim_result32[,13],
                     "RQLg j22 (rho=0.3,0.5,0.7)" = sim_result32[,7]-0.01,
                     "RQL j23 (rho=0.3,0.5,0.7)" = sim_result12[,21]+0.01)
  data4 = data.frame("x" = xx, "BQL (rho=0.3)" = sim_result12[,5],'HDQg (rho=0.3)'= sim_result12[,10],'RQLg (rho=0.3)' = sim_result12[,15],'HDQ (rho=0.3)'= sim_result12[,20],'RQL (rho=0.3)' = sim_result12[,23]-0.015,
                     "BQL (rho=0.5)" = sim_result22[,5],'HDQg (rho=0.5)'= sim_result22[,10],'RQLg (rho=0.5)' = sim_result22[,15],'HDQ (rho=0.5)'= sim_result22[,20],'RQL (rho=0.5)' = sim_result22[,23]+0.01,
                     "BQL (rho=0.7)" = sim_result32[,5],'HDQg (rho=0.7)'= sim_result32[,10],'RQLg (rho=0.7)' = sim_result32[,15],'HDQ (rho=0.7)'= sim_result32[,20],'RQL (rho=0.7)' = sim_result32[,23]-0.01)
  my_data1 = melt(data1,id="x");my_data2 = melt(data2,id="x");my_data3 = melt(data3,id="x");my_data4 = melt(data4,id="x")
  colnames(my_data1) = c("ii","method","value"); colnames(my_data2) = c("ii","method","value"); colnames(my_data3) = c("ii","method","value"); colnames(my_data4) = c("ii","method","value")
  
  texnames1 = c("BQL $j_{21}$ ($\\rho=0.3$)", "BQL $j_{22}$ ($\\rho=0.3$)",  "BQL $j_{23}$ ($\\rho=0.3$)", "BQL $j_{21}$ ($\\rho=0.5$)", "BQL $j_{22}$ ($\\rho=0.5$)",  "BQL $j_{23}$ ($\\rho=0.5$)", "BQL $j_{21}$ ($\\rho=0.7$)", "BQL $j_{22}$ ($\\rho=0.7$)",  "BQL $j_{23}$ ($\\rho=0.7$)")
  texnames2 = c("HDQg $j_{21}$ ($\\rho=0.3$)",  "HDQg $j_{21}$ ($\\rho=0.5$)", "HDQg $j_{21}$ ($\\rho=0.7$)", "HDQg $j_{23}$ ($\\rho=0.3$)", "HDQg $j_{23}$ ($\\rho=0.5$)", "HDQg $j_{23}$ ($\\rho=0.7$)" ,"HDQg $j_{22}$ ($\\rho=0.3,0.5,0.7$)", "HDQ $j_{23}$ ($\\rho=0.3,0.5,0.7$)")
  texnames3 = c("RQLg $j_{21}$ ($\\rho=0.3$)",  "RQLg $j_{21}$ ($\\rho=0.5$)", "RQLg $j_{21}$ ($\\rho=0.7$)", "RQLg $j_{23}$ ($\\rho=0.3$)", "RQLg $j_{23}$ ($\\rho=0.5$)", "RQLg $j_{23}$ ($\\rho=0.7$)" ,"RQLg $j_{22}$ ($\\rho=0.3,0.5,0.7$)", "RQL $j_{23}$ ($\\rho=0.3,0.5,0.7$)")
  texnames4 = c("BQL ($\\rho=0.3$)", "HDQg ($\\rho=0.3$)","RQLg ($\\rho=0.3$)","HDQ ($\\rho=0.3$)","RQL ($\\rho=0.3$)",
                "BQL ($\\rho=0.5$)", "HDQg ($\\rho=0.5$)","RQLg ($\\rho=0.5$)","HDQ ($\\rho=0.5$)","RQL ($\\rho=0.5$)",
                "BQL ($\\rho=0.7$)", "HDQg ($\\rho=0.7$)","RQLg ($\\rho=0.7$)","HDQ ($\\rho=0.7$)","RQL ($\\rho=0.7$)")
  
  
  plot1 = ggplot(data = my_data1,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,0,4,2,0,4,2,0),labels = unname(TeX(texnames1))) +
    scale_linetype_manual(values = c(1,1,1,2,2,2,3,3,3),labels = unname(TeX(texnames1))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#C72228","#C72228","#C72228","#C72228","#C72228","#C72228"),labels = unname(TeX(texnames1))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot1 = ggplot(data = my_data1,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,0,4,2,0,4,2,0),labels = unname(TeX(texnames1))) +
    scale_linetype_manual(values = c(1,1,1,2,2,2,3,3,3),labels = unname(TeX(texnames1))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#C72228","#C72228","#C72228","#C72228","#C72228","#C72228"),labels = unname(TeX(texnames1))) +
    labs(x = NULL, y = NULL)+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 3, byrow = TRUE),
      shape = guide_legend(nrow = 3, byrow = TRUE),
      linetype = guide_legend(nrow = 3, byrow = TRUE)
    )
  
  plot2 = ggplot(data = my_data2,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,4,4,0,0,0,2,0),labels = unname(TeX(texnames2))) +
    scale_linetype_manual(values = c(1,2,3,1,2,3,1,1),labels = unname(TeX(texnames2))) +
    scale_color_manual(values = c("#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#1F1F1F"),labels = unname(TeX(texnames2))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot2 = ggplot(data = my_data2,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,4,4,0,0,0,2,0),labels = unname(TeX(texnames2))) +
    scale_linetype_manual(values = c(1,2,3,1,2,3,1,1),labels = unname(TeX(texnames2))) +
    scale_color_manual(values = c("#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#1F1F1F"),labels = unname(TeX(texnames2))) +
    labs(x = NULL, y = NULL)+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 3, byrow = FALSE),
      shape = guide_legend(nrow = 3, byrow = FALSE),
      linetype = guide_legend(nrow = 3, byrow =FALSE)
    )
  
  plot3 = ggplot(data = my_data3,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,4,4,0,0,0,2,0),labels = unname(TeX(texnames3))) +
    scale_linetype_manual(values = c(1,2,3,1,2,3,1,1),labels = unname(TeX(texnames3))) +
    scale_color_manual(values = c("#F98F34","#F98F34","#F98F34","#F98F34","#F98F34","#F98F34","#F98F34","#1B9E77"),labels = unname(TeX(texnames3))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,4,4,0,0,0,2,0),labels = unname(TeX(texnames3))) +
    scale_linetype_manual(values = c(1,2,3,1,2,3,1,1),labels = unname(TeX(texnames3))) +
    scale_color_manual(values = c("#F98F34","#F98F34","#F98F34","#F98F34","#F98F34","#F98F34","#F98F34","#1B9E77"),labels = unname(TeX(texnames3))) +
    labs(x = NULL, y = NULL)+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 3, byrow = FALSE),
      shape = guide_legend(nrow = 3, byrow = FALSE),
      linetype = guide_legend(nrow = 3, byrow =FALSE)
    )
  
  plot4 = ggplot(data = my_data4,aes(x=ii,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0,4,2,1,8,0,4,2,1,8,0),labels = unname(TeX(texnames4))) +
    scale_linetype_manual(values = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),labels = unname(TeX(texnames4))) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames4))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    scale_y_continuous(limits = c(1.95,4.3),breaks = seq(2.0,4.4,0.4))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot4 = ggplot(data = my_data4,aes(x=ii,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0,4,2,1,8,0,4,2,1,8,0),labels = unname(TeX(texnames4))) +
    scale_linetype_manual(values = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),labels = unname(TeX(texnames4))) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames4))) +
    labs(x = expression(lambda), y = TeX("Profit"))+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5), 
      #legend.justification = c("right", "center"),
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 3, byrow =TRUE),
      shape = guide_legend(nrow = 3, byrow = TRUE),
      linetype = guide_legend(nrow = 3, byrow = TRUE)
    )
  
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  final_plot1 <- (wrap_elements(legend1_grob))/(plot1)  +
    plot_layout(heights = c(1.5,10))
  
  legend2 <- cowplot::get_legend(legend_plot2)
  legend2_grob <- cowplot::as_grob(legend2)
  
  final_plot2 <- (wrap_elements(legend2_grob))/(plot2)  +
    plot_layout(heights = c(1.5,10))
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  
  final_plot3 <- (wrap_elements(legend3_grob))/(plot3)  +
    plot_layout(heights = c(1.5,10))
  
  legend4 <- cowplot::get_legend(legend_plot4)
  legend4_grob <- cowplot::as_grob(legend4)
  
  final_plot4 <- (wrap_elements(legend4_grob))/(plot4)  +
    plot_layout(heights = c(1.5,10))
  
  print(final_plot4)
  #print(final_plot3)
  #plot_grid(final_plot, final_plot3, nrow = 1)
}




if(setting==3){
  read_data = read.csv("result_S12.csv")
  sim_result = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result2 = apply(sim_result, c(1, 2), mean)
  xx = c(0,0.4,0.8,1.2,2,2.8,3.6,4.4,5.2,6,7,8,9,10)
  num_J = length(xx)
  data11 = data.frame("x" = xx,
                     "BQL j12" = sim_result2[,1]-0.01,
                     "BQL j21" = 1 - sim_result2[,2] - sim_result2[,3],
                     "BQL j22" = sim_result2[,2],
                     "BQL j23" = sim_result2[,3]+0.01
                     )
  data12 = data.frame("x" = xx,
                     "HDQg j12" = sim_result2[,6]+0.01,
                     "HDQg j21" = 1 - sim_result2[,7] - sim_result2[,8],
                     "HDQg j22" = sim_result2[,7],
                     "HDQg j23" = sim_result2[,8]-0.01,
                     "HDQ j12" = sim_result2[,16],
                     #"HDQo j21" = 1 - sim_result2[,17] - sim_result2[,18],
                     "HDQ j22" = sim_result2[,17],
                     "HDQ j23" = sim_result2[,18]
  )
  data13 = data.frame("x" = xx,
                     "RQLg j12" = sim_result2[,11]-0.01,
                     "RQLg j21" = 1 - sim_result2[,12] - sim_result2[,13],
                     "RQLg j22" = sim_result2[,12],
                     "RQLg j23" = sim_result2[,13]+0.01,
                     "RQL j12 and j23" = sim_result2[,21]
  )
  colnames(data11) <- gsub("\\.", " ", colnames(data11))
  colnames(data11) <- gsub("and", "&", colnames(data11))
  data2 = data.frame("x" = xx, "BQL" = sim_result2[,4],'HDQ'= sim_result2[,9],'RQL'= sim_result2[,14])
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,5],'HDQg'= sim_result2[,10],'RQLg'= sim_result2[,15],'HDQ'= sim_result2[,20],'RQL'= sim_result2[,23])
  my_data11 = melt(data11,id="x"); my_data12 = melt(data12,id="x");my_data13 = melt(data13,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data11) = c("CJ2","method","value")
  colnames(my_data12) = c("CJ2","method","value")
  colnames(my_data13) = c("CJ2","method","value")
  colnames(my_data3) = c("CJ2","method","value")
  
  expressions11 = list(
    expression(BQL~j[12]),
    expression(BQL~j[21]),
    expression(BQL~j[22]),
    expression(BQL~j[23])
  )
  expressions12 = list(
    expression(HDQg~j[12]),
    expression(HDQg~j[21]),
    expression(HDQg~j[22]),
    expression(HDQg~j[23]),
    expression(HDQ~j[12]),
    expression(HDQ~j[22]),
    expression(HDQ~j[23])
  )
  expressions13 = list(
    expression(RQLg~j[12]),
    expression(RQLg~j[21]),
    expression(RQLg~j[22]),
    expression(RQLg~j[23]),
    expression(RQL~ j[12] ~"&" ~j[23]) 
  )
  
  plot11 = ggplot(data = my_data11,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.2)+
    geom_line(aes(linetype = method),size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0),labels = expressions11) +
    scale_linetype_manual(values = c(1,4,5,1),labels = expressions11) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#C72228"),labels = expressions11) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  
  plot12 = ggplot(data = my_data12,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(aes(linetype = method),size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,4,2,1,0),labels = expressions12) +
    scale_linetype_manual(values = c(1,4,5,1,1,4,5,1),labels = expressions12) +
    scale_color_manual(values = c("#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#1F1F1F","#1F1F1F","#1F1F1F","#1F1F1F"),labels = expressions12) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  
  plot13 = ggplot(data = my_data13,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.2)+
    geom_line(aes(linetype = method),size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,4),labels = expressions13) +
    scale_linetype_manual(values = c(1,4,5,1,1),labels = expressions13) +
    scale_color_manual(values = c("#F98F34","#F98F34","#F98F34","#F98F34","#1B9E77"),labels = expressions13) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    scale_y_continuous(breaks = seq(1.5,2.7,0.3))+  coord_cartesian(ylim = c(1.5, 2.7))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot11 = ggplot(data = my_data11,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.2)+
    geom_line(aes(linetype = method),size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0),labels = expressions11) +
    scale_linetype_manual(values = c(1,4,5,1),labels = expressions11) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#C72228"),labels = expressions11) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",         
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  legend_plot12 = ggplot(data = my_data12,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.2)+
    geom_line(aes(linetype = method),size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,4,2,1,0),labels = expressions12) +
    scale_linetype_manual(values = c(1,4,5,1,1,4,5,1),labels = expressions12) +
    scale_color_manual(values = c("#0C4E9B","#0C4E9B","#0C4E9B","#0C4E9B","#1F1F1F","#1F1F1F","#1F1F1F","#1F1F1F"),labels = expressions12) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",         
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 3, byrow = TRUE),
      shape = guide_legend(nrow = 3, byrow = TRUE),
      linetype = guide_legend(nrow = 3, byrow = TRUE)
    )
  
  legend_plot13 = ggplot(data = my_data13,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.2)+
    geom_line(aes(linetype = method),size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,4),labels = expressions13) +
    scale_linetype_manual(values = c(1,4,5,1,1),labels = expressions13) +
    scale_color_manual(values = c("#F98F34","#F98F34","#F98F34","#F98F34","#1B9E77"),labels = expressions13) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",         
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
    scale_y_continuous(limits = c(-0.4,2.7),breaks = seq(-0.4,2.7,0.5))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5), 
      #legend.justification = c("right", "center"),
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  legend11 <- cowplot::get_legend(legend_plot11)
  legend11_grob <- cowplot::as_grob(legend11)
  
  legend12 <- cowplot::get_legend(legend_plot12)
  legend12_grob <- cowplot::as_grob(legend12)
  
  legend13 <- cowplot::get_legend(legend_plot13)
  legend13_grob <- cowplot::as_grob(legend13)
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  
  final_plot11 <- (wrap_elements(legend11_grob))/(plot11)  +
    plot_layout(heights = c(1.5,10))
  
  final_plot12 <- (wrap_elements(legend12_grob))/(plot12)  +
    plot_layout(heights = c(1.5,10))
  
  final_plot13 <- (wrap_elements(legend13_grob))/(plot13)  +
    plot_layout(heights = c(1.5,10))
  
  final_plot3 <- (wrap_elements(legend3_grob))/(plot3)  +
    plot_layout(heights = c(1.5,10))
  
  print(final_plot3)
  #print(final_plot3)
  #plot_grid(final_plot11,final_plot12,final_plot13, final_plot3, nrow = 2)
}

if(setting==4){
  read_data = read.csv("result_S14.csv")
  sim_result = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result2 = apply(sim_result, c(1, 2), mean)
  xx = c(0,0.2,0.4,0.8,1.2,1.6,2.0,2.4,2.8,3.2,3.6,4.0,4.4,4.8,5.2,5.6,6.0)
  data1 = data.frame("x" = xx,
                     "BQL j21" = sim_result2[,1],
                     "BQL j24" = sim_result2[,4],
                     "BQL j27" = sim_result2[,7]-0.01,
                     "BQL j28" = sim_result2[,8]+0.01)
  data12 = data.frame("x" = xx,
                     "HDQg j21" = sim_result2[,11],
                     "HDQg j24" = sim_result2[,14]-0.01,
                     "HDQg j27" = sim_result2[,17],
                     "HDQg j28" = sim_result2[,18]+0.01,
                     "HDQ j27" = sim_result2[,37])
  data13 = data.frame("x" = xx,
                      "RQLg j21" = sim_result2[,21],
                      "RQLg j24" = sim_result2[,24],
                      "RQLg j27" = sim_result2[,27]-0.01,
                      "RQLg j28" = sim_result2[,28]+0.01,
                      "RQL j28" = 1)
  data2 = data.frame("x" = xx, "BQL" = sim_result2[,9],'HDQ'= sim_result2[,19],'RQL' = sim_result2[,29])
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,10],'HDQg'= sim_result2[,20],'RQLg' = sim_result2[,30],'HDQ'= sim_result2[,40],'RQL' = sim_result2[,42])
  my_data1 = melt(data1,id="x");my_data12 = melt(data12,id="x");my_data13 = melt(data13,id="x");my_data2 = melt(data2,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("CJ2","method","value"); colnames(my_data12) = c("CJ2","method","value"); colnames(my_data13) = c("CJ2","method","value"); colnames(my_data2) = c("CJ2","method","value"); colnames(my_data3) = c("CJ2","method","value")
  
  texnames = c("BQL $j_{21}$", "BQL $j_{24}$",  "BQL $j_{27}$", "BQL $j_{28}$")
  texnames2 = c("HDQg $j_{21}$", "HDQg $j_{24}$",  "HDQg $j_{27}$", "HDQg $j_{28}$","HDQ $j_{27}$")
  texnames3 = c("RQLg $j_{21}$", "RQLg $j_{24}$",  "RQLg $j_{27}$", "RQLg $j_{28}$","RQL $j_{28}$")
  
  
  plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group=method,color=method,shape=method))+
    geom_point(size = 2.2)+
    scale_linetype_manual(values = c(3,5,4,1),labels = unname(TeX(texnames))) +
    scale_shape_manual(values = c(4,2,1,0),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#C72228"),labels = unname(TeX(texnames))) +
    geom_line(aes(linetype = method),size = 0.6)+
    labs(x = NULL, y = NULL)+
    theme(
      axis.title.x = element_text(size = 30, margin = margin(t = 12)),
      legend.position = 'none',              
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20)
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))
  
  plot12 = ggplot(data = my_data12,aes(x=CJ2,y=value,group=method,color=method,shape=method))+
    geom_point(size = 2.2)+
    scale_linetype_manual(values = c(3,5,4,1,4),labels = unname(TeX(texnames2))) +
    scale_shape_manual(values = c(4,2,1,0,1),labels = unname(TeX(texnames2))) +
    scale_color_manual(values = c( "#0C4E9B",  "#0C4E9B",  "#0C4E9B", "#0C4E9B", "#1F1F1F"),labels = unname(TeX(texnames2))) +
    geom_line(aes(linetype = method),size = 0.6)+
    labs(x = NULL, y = NULL)+
    theme(
      axis.title.x = element_text(size = 30, margin = margin(t = 12)),
      legend.position = 'none',              
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20)
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))

  plot13 = ggplot(data = my_data13,aes(x=CJ2,y=value,group=method,color=method,shape=method))+
    geom_point(size = 2.2)+
    scale_linetype_manual(values = c(3,5,4,1,1),labels = unname(TeX(texnames3))) +
    scale_shape_manual(values = c(4,2,1,0,0),labels = unname(TeX(texnames3))) +
    scale_color_manual(values = c( "#F98F34","#F98F34","#F98F34","#F98F34","#1B9E77"),labels = unname(TeX(texnames3))) +
    geom_line(aes(linetype = method),size = 0.6)+
    labs(x = NULL, y = NULL)+
    theme(
      axis.title.x = element_text(size = 30, margin = margin(t = 12)),
      legend.position = 'none',              
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20)
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))
  
  
  
  plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34", "#1F1F1F","#1B9E77")) +
    #geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray") +
    labs(x = NULL, y = NULL)+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+
    scale_y_continuous(limits = c(-0.7,3),breaks = seq(-0.7,3,0.5))
  
  legend_plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group=method,color=method,shape=method))+
    geom_point(size = 2.2)+
    scale_linetype_manual(values = c(3,5,4,1),labels = unname(TeX(texnames))) +
    scale_shape_manual(values = c(4,2,1,0),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#C72228"),labels = unname(TeX(texnames))) +
    geom_line(aes(linetype = method),size = 0.6)+
    labs(x = expression(lambda), y = TeX("Frequency"))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+ guides(
      color = guide_legend(ncol = 2),
      shape = guide_legend(ncol = 2),
      linetype = guide_legend(ncol = 2)
    )
  
  legend_plot12 = ggplot(data = my_data12,aes(x=CJ2,y=value,group=method,color=method,shape=method))+
    geom_point(size = 2.2)+
    scale_linetype_manual(values = c(3,5,4,1,4),labels = unname(TeX(texnames2))) +
    scale_shape_manual(values = c(4,2,1,0,1),labels = unname(TeX(texnames2))) +
    scale_color_manual(values = c("#0C4E9B",  "#0C4E9B",  "#0C4E9B", "#0C4E9B", "#1F1F1F"),labels = unname(TeX(texnames2))) +
    geom_line(aes(linetype = method),size = 0.6)+
    labs(x = expression(lambda), y = TeX("Frequency"))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  legend_plot13 = ggplot(data = my_data13,aes(x=CJ2,y=value,group=method,color=method,shape=method))+
    geom_point(size = 2.2)+
    scale_linetype_manual(values = c(3,5,4,1,1),labels = unname(TeX(texnames3))) +
    scale_shape_manual(values = c(4,2,1,0,0),labels = unname(TeX(texnames3))) +
    scale_color_manual(values = c("#F98F34","#F98F34","#F98F34","#F98F34","#1B9E77"),labels = unname(TeX(texnames3))) +
    geom_line(aes(linetype = method),size = 0.6)+
    labs(x = expression(lambda), y = TeX("Frequency"))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+ guides(
      color = guide_legend(ncol = 3),
      shape = guide_legend(ncol = 3),
      linetype = guide_legend(ncol = 3)
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34", "#1F1F1F","#1B9E77")) +
    #geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray") +
    labs(x = expression(lambda), y = TeX("Profit"))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5),              
      #legend.justification = c("right", "center"),
      #legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+
    scale_x_continuous(limits = c(0,6),breaks = seq(0,6,1))+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  final_plot <- wrap_elements(legend1_grob) / (plot1)  +
    plot_layout(heights = c(1.5, 10))
  
  legend12 <- cowplot::get_legend(legend_plot12)
  legend12_grob <- cowplot::as_grob(legend12)
  final_plot12 <- wrap_elements(legend12_grob) / (plot12)  +
    plot_layout(heights = c(1.5, 10))
  
  legend13 <- cowplot::get_legend(legend_plot13)
  legend13_grob <- cowplot::as_grob(legend13)
  final_plot13 <- wrap_elements(legend13_grob) / (plot13)  +
    plot_layout(heights = c(1.5, 10))
  
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  final_plot3 <- wrap_elements(legend3_grob) / (plot3)  +
    plot_layout(heights = c(1.5, 10))
  
  
  
  print(final_plot3)
  #print(final_plot3)
  #plot_grid(final_plot,final_plot12,final_plot13,final_plot3, nrow = 2)
}

if(setting==5){
  read_data = read.csv("result_S15.csv")
  new_array = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result = new_array
  num_a2 = 11
  sim_result2 = apply(sim_result, c(1, 2), mean)
  xx = seq(from = 0, to = 15, length.out = num_a2)
  data1 = data.frame("x" = xx, "BQL" = sim_result2[,1],'HDQg'= sim_result2[,4],'RQLg' = sim_result2[,7],'HDQ'= sim_result2[,10],'RQL' = sim_result2[,13])
  data2 = data.frame("x" = xx, "BQL" = sim_result2[,2],'HDQg'= sim_result2[,5],'RQLg' = sim_result2[,8],'HDQ'= sim_result2[,11],'RQL' = sim_result2[,14])
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,3],'HDQg'= sim_result2[,6],'RQLg' = sim_result2[,9],'HDQ'= sim_result2[,12],'RQL' = sim_result2[,15])
  my_data1 = melt(data1,id="x");my_data2 = melt(data2,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("C2","method","value"); colnames(my_data2) = c("C2","method","value"); colnames(my_data3) = c("C2","method","value")
  plot1 = ggplot(data = my_data1,aes(x=C2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 3.5)+
    geom_line(size = 0.6)+
    geom_vline(xintercept = 7.5, linetype = "dashed", color = "gray30", size = 0.6)+
    labs(x = NULL, y = NULL)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5),              
      #legend.justification = c("right", "center"),
      #legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  plot2 = ggplot(data = my_data2,aes(x=C2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 3.5)+
    geom_line(size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,8)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    geom_vline(xintercept = 7.5, linetype = "dashed", color = "gray30", size = 0.6)+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    scale_y_continuous(limits = c(1,2.2),breaks = seq(1,2.2,0.4))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5),              
      #legend.justification = c("right", "center"),
      #legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  plot3 = ggplot(data = my_data3,aes(x=C2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 3.5)+
    geom_line(size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,8)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    geom_vline(xintercept = 7.5, linetype = "dashed", color = "gray30", size = 0.6)+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    scale_y_continuous(limits = c(-6.7,1.6),breaks = seq(-6.5,1.5,2.5))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5),              
      #legend.justification = c("right", "center"),
      #legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  #plot1
  #plot2
  plot3
  #plot_grid(plot1,plot3, nrow = 1)
}

if(setting==6){
  read_data = read.csv("result_S16.csv")
  new_array = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result = new_array
  sim_result2 = apply(sim_result, c(1, 2), mean)
  num_a1 = 11
  xx = seq(from = 0, to = 15, length.out = num_a1)
  data1 = data.frame("x" = xx, "BQL" = sim_result2[,1],'HDQg'= sim_result2[,4],'RQLg' = sim_result2[,7],'HDQ'= sim_result2[,10],'RQL' = sim_result2[,13])
  data2 = data.frame("x" = xx, "BQL" = sim_result2[,2],'HDQg'= sim_result2[,5],'RQLg' = sim_result2[,8],'HDQ'= sim_result2[,11],'RQL' = sim_result2[,14])
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,3],'HDQg'= sim_result2[,6],'RQLg' = sim_result2[,9],'HDQ'= sim_result2[,12],'RQL' = sim_result2[,15])
  my_data1 = melt(data1,id="x");my_data2 = melt(data2,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("C1","method","value"); colnames(my_data2) = c("C1","method","value"); colnames(my_data3) = c("C1","method","value")
  
  plot1 = ggplot(data = my_data1,aes(x=C1,y=value,group =method,color=method,shape=method))+
    geom_point(size = 3.5)+
    geom_line(size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,8)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    geom_vline(xintercept = 7.5, linetype = "dashed", color = "gray30", size = 0.6)+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5),              
      #legend.justification = c("right", "center"),
      #legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  plot2 = ggplot(data = my_data2,aes(x=C1,y=value,group =method,color=method,shape=method))+
    geom_point(size = 3.5)+
    geom_line(size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,8)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    geom_vline(xintercept = 7.5, linetype = "dashed", color = "gray30", size = 0.6)+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    scale_y_continuous(limits = c(0.6,2.2),breaks = seq(0.6,2.2,0.4))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5),              
      #legend.justification = c("right", "center"),
      #legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  plot3 = ggplot(data = my_data3,aes(x=C1,y=value,group =method,color=method,shape=method))+
    geom_point(size = 3.5)+
    geom_line(size = 0.6)+
    scale_shape_manual(values = c(4,2,1,0,8)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    geom_vline(xintercept = 7.5, linetype = "dashed", color = "gray30", size = 0.6)+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    scale_y_continuous(limits = c(-7.5,1.1),breaks = seq(-7.5,0,2.5))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5),              
      #legend.justification = c("right", "center"),
      #legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  #plot1
  #plot2
  plot3
  #plot_grid(plot1,plot3, nrow = 1)
}

if(setting==7){
  read_data1 = read.csv("result_S17_k0.csv")
  read_data2 = read.csv("result_S17_k1.csv")
  read_data3 = read.csv("result_S17_k2.csv")
  sim_result1  = array(data = read_data1$value, dim = c(max(read_data1$row), max(read_data1$col), max(read_data1$matrix)))
  sim_result12 = apply(sim_result1, c(1, 2), mean)
  sim_result2  = array(data = read_data2$value, dim = c(max(read_data2$row), max(read_data2$col), max(read_data2$matrix)))
  sim_result22 = apply(sim_result2, c(1, 2), mean)
  sim_result3  = array(data = read_data3$value, dim = c(max(read_data3$row), max(read_data3$col), max(read_data3$matrix)))
  sim_result32 = apply(sim_result3, c(1, 2), mean)
  xx = c(0,1,2,3,4,5,6,7,9,11,13,15)
  num_J = length(xx)
  data1 = data.frame("x" = xx,
                     "BQL j22 (kappa=0)" = sim_result12[,1],
                     "HDQg j22 (kappa=0)" = sim_result12[,4],
                     "RQLg j22 (kappa=0)" = sim_result12[,7],
                     "BQL j22 (kappa=1)" = sim_result22[,1],
                     "HDQg j22 (kappa=1)" = sim_result22[,4],
                     "RQLg j22 (kappa=1)" = sim_result22[,7],
                     "BQL j22 (kappa=2)" = sim_result32[,1],
                     "HDQg j22 (kappa=2)" = sim_result32[,4],
                     "RQLg j22 (kappa=2)" = sim_result32[,7],
                     "HDQ j22 (kappa=0,1,2)" = sim_result12[,10]-0.01,
                     "RQL j22 (kappa=0,1,2)" = sim_result12[,13]+0.01)
  
  colnames(data1) <- gsub("\\.", " ", colnames(data1))
  colnames(data1) <- gsub("and", "&", colnames(data1))
  data3 = data.frame("x" = xx, "BQL (kappa=0)" = sim_result12[,3],'HDQg (kappa=0)'= sim_result12[,6],'RQLg (kappa=0)'= sim_result12[,9],'HDQ (kappa=0)'= sim_result12[,12],'RQL (kappa=0)'= sim_result12[,15],
                     "BQL (kappa=1)" = sim_result22[,3],'HDQg (kappa=1)'= sim_result22[,6],'RQLg (kappa=1)'= sim_result22[,9],'HDQ (kappa=1)'= sim_result22[,12],'RQL (kappa=1)'= sim_result22[,15],
                     "BQL (kappa=2)" = sim_result32[,3],'HDQg (kappa=2)'= sim_result32[,6],'RQLg (kappa=2)'= sim_result32[,9],'HDQ (kappa=2)'= sim_result32[,12],'RQL (kappa=2)'= sim_result32[,15])
  my_data1 = melt(data1,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("CJ2","method","value")
  colnames(my_data3) = c("CJ2","method","value")
  
  texnames1 = c( "BQL $j_{22}$ ($\\kappa=0$)", "HDQg $j_{22}$ ($\\kappa=0$)", "RQLg $j_{22}$ ($\\kappa=0$)")
  texnames2 = c( "BQL $j_{22}$ ($\\kappa=1$)", "HDQg $j_{22}$ ($\\kappa=1$)", "RQLg $j_{22}$ ($\\kappa=1$)")
  texnames3 = c( "BQL $j_{22}$ ($\\kappa=2$)", "HDQg $j_{22}$ ($\\kappa=2$)", "RQLg $j_{22}$ ($\\kappa=2$)")
  texnames4 = c("HDQ $j_{22}$ ($\\kappa=0,1,2$)", "RQL $j_{22}$ ($\\kappa=0,1,2$)")
  texnames = c(texnames1,texnames2,texnames3,texnames4)
  
  texnames1_ = c( "BQL ($\\kappa=0$)", "HDQg ($\\kappa=0$)", "RQLg ($\\kappa=0$)", "HDQ ($\\kappa=0$)", "RQL ($\\kappa=0$)")
  texnames2_ = c( "BQL ($\\kappa=1$)", "HDQg ($\\kappa=1$)", "RQLg ($\\kappa=1$)", "HDQ ($\\kappa=1$)", "RQL ($\\kappa=1$)")
  texnames3_ = c( "BQL ($\\kappa=2$)", "HDQg ($\\kappa=2$)", "RQLg ($\\kappa=2$)", "HDQ ($\\kappa=2$)", "RQL ($\\kappa=2$)")
  texnames_ = c(texnames1_,texnames2_,texnames3_)
  
  
  plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,4,2,1,4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,2,2,2,3,3,3,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#0C4E9B","#F98F34","#C72228","#0C4E9B","#F98F34","#C72228","#0C4E9B","#F98F34","#1F1F1F","#1B9E77"), labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,4,2,1,4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,2,2,2,3,3,3,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#0C4E9B","#F98F34","#C72228","#0C4E9B","#F98F34","#C72228","#0C4E9B","#F98F34","#1F1F1F","#1B9E77"), labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 4, byrow = TRUE),
      shape = guide_legend(nrow = 4, byrow = TRUE),
      linetype = guide_legend(nrow = 4, byrow = TRUE)
    )
  
  plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.8)+
    scale_linetype_manual(values = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),labels = unname(TeX(texnames_))) +
    scale_shape_manual(values = c(4,2,1,8,0,4,2,1,8,0,4,2,1,8,0),labels = unname(TeX(texnames_))) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames_))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    scale_y_continuous(limits = c(-1,1.17),breaks = seq(-1,1.1,0.3))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_linetype_manual(values = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),labels = unname(TeX(texnames_))) +
    scale_shape_manual(values = c(4,2,1,8,0,4,2,1,8,0,4,2,1,8,0),labels = unname(TeX(texnames_))) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77","#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames_))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,3))+
    scale_y_continuous(limits = c(-1,1.17),breaks = seq(-1,1.1,0.3))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 3, byrow = TRUE),
      shape = guide_legend(nrow = 3, byrow = TRUE),
      linetype = guide_legend(nrow = 3, byrow = TRUE)
    )
  
  
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  final_plot1 <- wrap_elements(legend1_grob) / (plot1)  +
    plot_layout(heights = c(1.5, 10))
  
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  final_plot3 <- wrap_elements(legend3_grob) / (plot3)  +
    plot_layout(heights = c(1.5, 10))
  
  #final_plot1
  
  
  final_plot3
}


if(setting==8){
  read_data = read.csv("result_S18.csv")
  sim_result = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  sim_result2 = apply(sim_result, c(1, 2), mean)
  
  xx = c(0,0.05,0.1,0.15,0.2,0.5,0.8,1.1,1.4,1.7,2.0,2.3,2.5)/0.5
  num_J = length(xx)
  data1 = data.frame("x" = xx,
                     "BQL j21" = sim_result2[,1],
                     "BQL j22" = sim_result2[,2],
                     "BQL j23" = sim_result2[,3],
                     "HDQg j21" = sim_result2[,6],
                     "HDQg j22" = sim_result2[,7]+0.01,
                     "HDQg j23" = sim_result2[,8]+0.02,
                     "RQLg j21" = sim_result2[,11]-0.02,
                     "RQLg j22" = sim_result2[,12]-0.01,
                     "RQLg j23" = sim_result2[,13]-0.02,
                     "HDQ j23" = sim_result2[,18]-0.01,
                     "RQL j23" = sim_result2[,21]+0.01)
  colnames(data1) <- gsub("\\.", " ", colnames(data1))
  colnames(data1) <- gsub("and", "&", colnames(data1))
  data2 = data.frame("x" = xx, "BQL" = sim_result2[,4],'HDQ'= sim_result2[,9],'RQL' = sim_result2[,14])
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,5],'HDQg'= sim_result2[,10],'RQLg' = sim_result2[,15],'HDQ'= sim_result2[,20],'RQL' = sim_result2[,23])
  my_data1 = melt(data1,id="x");my_data2 = melt(data2,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("ii","method","value"); colnames(my_data2) = c("ii","method","value"); colnames(my_data3) = c("ii","method","value")
  
  texnames = c("BQL $j_{21}$", "BQL $j_{22}$",  "BQL $j_{23}$","HDQg $j_{21}$","HDQg $j_{22}$", "HDQg $j_{23}$","RQLg $j_{21}$", "RQLg $j_{22}$","RQLg $j_{23}$","HDQ $j_{23}$","RQL $j_{23}$")
  
  
  plot1 = ggplot(data = my_data1,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,0,4,2,0,4,2,0,0,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(3,5,1,3,5,1,3,5,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#0C4E9B","#0C4E9B","#0C4E9B", "#F98F34","#F98F34","#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,5),breaks = seq(0,5,1))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot1 = ggplot(data = my_data1,aes(x=ii,y=value,group=method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,0,4,2,0,4,2,0,0,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(3,5,1,3,5,1,3,5,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#C72228","#C72228","#0C4E9B","#0C4E9B","#0C4E9B","#F98F34","#F98F34","#F98F34","#1F1F1F","#1B9E77"),labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.text.align = 0,
      legend.position = "bottom",              
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0),
      legend.spacing.x = unit(0.5, "cm"),
      legend.direction = "vertical"
    )+guides(
      color = guide_legend(nrow = 4, byrow = TRUE),
      shape = guide_legend(nrow = 4, byrow = TRUE),
      linetype = guide_legend(nrow = 4, byrow = TRUE)
    )
  
  
  plot3 = ggplot(data = my_data3,aes(x=ii,y=value,group =method,color=method,shape=method))+
    geom_point(size = 2.5)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,5),breaks = seq(0,5,1))+
    scale_y_continuous(limits = c(-0.43,4.65),breaks = seq(-0.4,4.6,0.5))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.title.x = element_text(size = 30, margin = margin(t = 12))
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=ii,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = expression(lambda), y = TeX("Profit"))+
    scale_x_continuous(limits = c(0,7),breaks = seq(0,7,1))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      #legend.text.align = 0,
      #legend.position = c(1, 0.5), 
      #legend.justification = c("right", "center"),
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  
  final_plot <- (wrap_elements(legend1_grob))/(plot1)  +
    plot_layout(heights = c(2,10))
  
  final_plot3 <- (wrap_elements(legend3_grob))/(plot3)  +
    plot_layout(heights = c(2,10))
  
  
  #print(final_plot)
  print(final_plot3)
  #plot_grid(final_plot, final_plot3, nrow = 1)
}


if(setting==91||setting==92||setting==93||setting==94){
  if(setting==91){read_data = read.csv("result_S19_d100_n300.csv")}
  if(setting==92){read_data = read.csv("result_S19_d100_n400.csv")}
  if(setting==93){read_data = read.csv("result_S19_d200_n400.csv")}
  if(setting==94){read_data = read.csv("result_S19_d300_n400.csv")}
  sim_result  = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
  #sim_result = sim_result[,,1:100]
  sim_result2 = apply(sim_result, c(1, 2), mean)
  xx = c(0,0.4,0.8,1.2,1.6,2.3,3,4.5,5.25,6,6.75,7.5,8.25,9)
  #xx = c(0,0.5,1,1.5,3,4.5,6,7.5,9)
  num_J = length(xx)
  data1 = data.frame("x" = xx,
                     "BQL j22" = sim_result2[,1],
                     "HDQg j22" = sim_result2[,4],
                     "RQLg j22" = sim_result2[,7],
                     "HDQo j22" = sim_result2[,10]-0.01,
                     "RQLg j22" = sim_result2[,13]+0.01)
  
  colnames(data1) <- gsub("\\.", " ", colnames(data1))
  colnames(data1) <- gsub("and", "&", colnames(data1))
  data3 = data.frame("x" = xx, "BQL" = sim_result2[,3],'HDQg'= sim_result2[,6],'RQLg'= sim_result2[,9],'HDQ'= sim_result2[,12],'RQL'= sim_result2[,15])
  my_data1 = melt(data1,id="x");my_data3 = melt(data3,id="x")
  colnames(my_data1) = c("CJ2","method","value")
  colnames(my_data3) = c("CJ2","method","value")
  
  texnames = c( "BQL $j_{22}$", "HDQg $j_{22}$", "RQLg $j_{22}$", "HDQ $j_{22}$", "RQL $j_{22}$")
  
  
  plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#0C4E9B","#F98F34","#1F1F1F","#1B9E77"), labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,9),breaks = seq(0,9,1))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot1 = ggplot(data = my_data1,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(aes(linetype = method),size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0),labels = unname(TeX(texnames))) +
    scale_linetype_manual(values = c(1,1,1,1,1),labels = unname(TeX(texnames))) +
    scale_color_manual(values = c("#C72228","#0C4E9B","#F98F34","#1F1F1F","#1B9E77"), labels = unname(TeX(texnames))) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,9),breaks = seq(0,9,1))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,9),breaks = seq(0,9,1))+
    scale_y_continuous(limits = c(1.4,2.45),breaks = seq(1.4,2.4,0.2))+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend_plot3 = ggplot(data = my_data3,aes(x=CJ2,y=value,group =method,color=method,shape=method))+
    geom_point(size = 1.8)+
    geom_line(size = 0.75)+
    scale_shape_manual(values = c(4,2,1,8,0)) +
    scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34","#1F1F1F","#1B9E77")) +
    labs(x = NULL, y = NULL)+
    scale_x_continuous(limits = c(0,9),breaks = seq(0,9,1))+
    scale_y_continuous(limits = c(1.4,2.45),breaks = seq(1.4,2.4,0.2))+
    theme(
      legend.key.width = unit(2, "cm"),
      legend.position = "top",
      legend.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 0)
    )+guides(
      color = guide_legend(nrow = 2, byrow = FALSE),
      shape = guide_legend(nrow = 2, byrow = FALSE),
      linetype = guide_legend(nrow = 2, byrow = FALSE)
    )
  
  legend1 <- cowplot::get_legend(legend_plot1)
  legend1_grob <- cowplot::as_grob(legend1)
  
  legend3 <- cowplot::get_legend(legend_plot3)
  legend3_grob <- cowplot::as_grob(legend3)
  
  final_plot <- (wrap_elements(legend1_grob))/(plot1)  +
    plot_layout(heights = c(1.5,10))
  
  final_plot3 <- (wrap_elements(legend3_grob))/(plot3)  +
    plot_layout(heights = c(1.5,10))
  
  final_plot
  
  #plot1
  #plot3
  #or plot1 | plot 3
  #plot_grid(plot1, plot3, nrow = 1)
}
