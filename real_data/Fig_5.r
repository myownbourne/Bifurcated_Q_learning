library(ggplot2)
library(tidyr)
library(dplyr)

df <- data.frame(
  tau = c("Infty", "100000", "10000", "5000", "2000", "1000", "500", "200", "100"),
  BQL  = c(0.8487, 0.8502, 0.8448, 0.8428, 0.8401, 0.8335, 0.8253, 0.8145, 0.8151),
  RQLg = c(0.8482, 0.8480, 0.8443, 0.8414, 0.8301, 0.8229, 0.8160, 0.8093, 0.8105),
  HDQg = c(0.8296, 0.8297, 0.8249, 0.8190, 0.8051, 0.8020, 0.7788, 0.7956, 0.8063),
  RQL  = c(0.8487, 0.8483, 0.8435, 0.8381, 0.8221, 0.7963, 0.7441, 0.5955, 0.3673),
  HDQ  = c(0.8319, 0.8314, 0.8259, 0.8205, 0.8022, 0.7744, 0.7237, 0.5825, 0.3643)
)

df_long <- df %>%
  pivot_longer(cols = -tau, names_to = "Method", values_to = "Profit")

df_long$tau <- factor(
  df_long$tau,
  levels = c("Infty", "100000", "10000", "5000", "2000", "1000", "500", "200", "100")
)

df_long$Method <- factor(df_long$Method, levels = c("BQL", "RQLg", "HDQg", "RQL", "HDQ"))

ggplot(df_long, aes(x = tau, y = Profit, group = Method,
                    color = Method, shape = Method)) +
  geom_line(linewidth = 0.75, linetype = "solid") +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#C72228", "#0C4E9B", "#F98F34", "#1F1F1F", "#1B9E77")) +
  scale_shape_manual(values = c(4, 2, 1, 8, 0)) +
  labs(
    x = expression(tau),
    y = "Profit",
    color = NULL,
    shape = NULL
  ) +
  coord_cartesian(ylim = c(0.75, 0.85)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.key.width = unit(2, "cm"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 0),
    legend.position = "top"
  )