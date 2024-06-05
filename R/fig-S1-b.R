# fig-S1-b ----------------------------------------------------------------


library(ggplot2)
library(patchwork)

load("temp_data/eic_logfc2_data.rdata")
p_value <- data.table::fread("temp_data/Pro - Raw.csv")
p_value$.features_id <- as.character(p_value$.features_id)
p_value <- dplyr::select(p_value, .features_id, P.Value)
df <- dplyr::select(data$eic.df, .features_id, real.time.min, mz, int, group)
df <- dplyr::left_join(df, p_value, by = ".features_id")
df_pro <- dplyr::filter(df, group == "Pro")
df_raw <- dplyr::filter(df, group == "Raw")

p_pro <- ggplot(df_pro) +
  geom_point(
    aes(
      x = real.time.min,
      y = mz,
      size = int,
      fill = P.Value
    ), shape = 21, color = "white", stroke = .1
  ) +
  scale_size(range = c(4, 16))+
  scale_fill_gradient(low = "#361918", high = "#f27970")+
  scale_x_continuous(limits = c(0,22), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,900), expand = c(0,0)) +
  labs(
    x = "Retention time (minutes)", y = "Mass-to-Charge Ratio(m/z)",
    fill = "Pro"
  ) +
  #ggtitle("AM features LC-MS/MS TIC cloud plot") +
  theme_bw()+
  theme_linedraw()+
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"), 
    plot.title = element_text(hjust = .5, vjust = .8, size = 12, face = "bold"),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"))+
  guides(
    size = "none"
  )
print(p_pro)

p_raw <- ggplot(df_raw) +
  geom_point(
    aes(
      x = real.time.min,
      y = mz,
      size = int,
      fill = P.Value
    ), shape = 21, color = "white", stroke = .1
  ) +
  scale_size(range = c(4, 16))+
  scale_fill_gradient(low = "#34002B", high = "#7ccba2")+
  scale_x_continuous(limits = c(0,22), expand=c(0,0)) +
  scale_y_reverse(limits = c(900,0), expand=c(0,0)) +
  labs(
    x = "Retention time (minutes)", y = "Mass-to-Charge Ratio(m/z)",
    fill = "Raw"
  ) +
  theme_bw()+
  theme_linedraw()+
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"), 
    plot.title = element_text(hjust = .5, vjust = .8, size = 12, face = "bold"),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"))+
  guides(
    size = "none"
  )
print(p_raw)

p <- p_pro / p_raw
print(p)

ggsave("fig-S1-b.png", p, height = 5, width = 8, dpi = 600)
ggsave("fig-S1-b.pdf", p, height = 5, width = 8)
