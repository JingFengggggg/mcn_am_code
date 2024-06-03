library(ggplot2)
library(patchwork)

load("temp_data/eic_data.rdata")
p_value <- data.table::fread("temp_data/Pro - Raw.csv")

p_value$.features_id <- as.character(p_value$.features_id)
p_value <- dplyr::select(p_value, .features_id, P.Value)
df <- data$eic.df
#for (i in 1:length(df$group)) {
#  ifelse(df$group[i] == "Pro", df$mz[i] <- df$mz[i], df$mz[i] <- -df$mz[i])
#}
df <- dplyr::select(df, .features_id, real.time.min, mz, int, group)
df <- dplyr::left_join(df, p_value, by='.features_id')
df$fill_group <- cut(df$P.Value, breaks = 7, labels = FALSE)
pro_palette <- colorRampPalette(c('#361918', '#f27970'))
pro_fill <- pro_palette(7)
raw_palette <- colorRampPalette(c("#34002B", '#7ccba2'))
raw_fill <- raw_palette(7)
df1 <- dplyr::filter(df, group == 'Pro')
df2 <- dplyr::filter(df, group == 'Raw')

p1 <- ggplot(df1) +
  geom_point(
    aes(x = real.time.min,
        y = mz,
        size = int,
        fill = factor(fill_group)), shape = 21, color = "white", stroke = .1) +
  scale_size(range = c(4, 16))+
  scale_fill_manual(values = pro_fill,
                    labels = paste(1:7))+
  scale_y_continuous(limits = c(0,1000), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,22), expand = c(0,0)) +
  labs(x = "Retention time (minutes)", y = "Mass-to-Charge Ratio(m/z)",
       color = "Pro") +
  ggtitle("AMR features LC-MS/MS TIC cloud plot") +
  theme_bw()+
  theme_linedraw()+
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(face="bold"),
    axis.text.y = element_text(face = 'bold'), 
    plot.title = element_text(hjust = .5, vjust = .8, size = 12, face = 'bold'),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = 'transparent'),
    legend.title = element_text(face = 'bold'),
    legend.text = element_text(face = 'bold'))+
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    fill = "none",
    size = 'none'
  )
print(p1)

p2 <- ggplot(df2) +
  geom_point(
    aes(x = real.time.min,
        y = mz,
        size = int,
        fill = factor(fill_group)), shape = 21, color = "white", stroke = .1) +#设置点的形状为实心圆
  scale_size(range = c(4, 16))+
  scale_fill_manual(values = raw_fill,
                    labels = paste(1:7))+
  scale_y_reverse(limits = c(1000,0), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,22), expand=c(0,0)) +
  labs(x = "Retention time (minutes)", y = "Mass-to-Charge Ratio(m/z)",
       color = 'Raw') +
  theme_bw()+
  theme_linedraw()+
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(face="bold"),
    axis.text.y = element_text(face = 'bold'), 
    plot.title = element_text(hjust = .5, vjust = .8, size = 12, face = 'bold'),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = 'transparent'),
    legend.title = element_text(face = 'bold'),
    legend.text = element_text(face = 'bold'))+
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    fill = "none",     
    size = 'none'
  )
print(p2)

p3 <- p1/p2
p3

ggsave("fig-S1-b.png", p3, height = 5, width = 8, dpi = 600)
ggsave("fig-S1-b.pdf", p3, height = 5, width = 8)
