# fig-05-c ----------------------------------------------------------------


library(ggplot2)

df <- tibble::tibble(
  group = factor(rep(c("UNIFI-pos", "UNIFI-neg", "GNPS-pos", 
                "GNPS-neg", "SIRIUS-pos", "SIRIUS-neg"), 2),
                levels = c("UNIFI-pos", "UNIFI-neg", "GNPS-pos", 
                           "GNPS-neg", "SIRIUS-pos", "SIRIUS-neg")),
  value = c(20, 25,74,46,61,94,80,75,26,54,39,6),
  type = rep(c("Match", "No-Match"), each = 6)
)

pc <- ggplot(df, aes(group, value, fill=type))+
  geom_bar(position = position_fill(), stat = 'identity', width = 0.5)+
  geom_text(aes(label=paste0(value, "%")),  
            position = position_fill(0.5))+
  scale_fill_manual(values = c("#C82423", "#2878B5")) +
  labs(x='', y='')+
  theme_void()+
  theme(legend.position = 'none',
    axis.text.x = element_text(face = "bold", vjust = 2),
        legend.title = element_blank())
pc

ggsave("fig-05-c.tiff", pc, units = "in", width = 6, height = 4, dpi = 600)
ggsave("fig-05-c.pdf", pc, width = 6, height = 4)
