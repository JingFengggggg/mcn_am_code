# fig-05-a ----------------------------------------------------------------


df <- list(
  `Manual` = c(1:17,22:28,36:52, 59:61, 68, 81),
  `UNIFI` = c(1:21,36:58,68:80,89:91),
  `SIRIUS-MCnebula2` = c(1:67),
  `GNPS` = c(1:35,68:88)
)
df$Manual <- as.integer(df$Manual)

if (!require(ggVennDiagram))devtools::install_github("gaospecial/ggVennDiagram")
library(ggVennDiagram)
library(ggplot2)

pa <- ggVennDiagram(df, label_alpha = 0)+
  scale_fill_gradient(low = "#2878B5", high = "#C82423")
pa

ggsave("fig-05-a.tiff", units = "in", width = 5, height = 4, dpi = 600)
ggsave("fig-05-a.pdf", width = 5, height = 4)
