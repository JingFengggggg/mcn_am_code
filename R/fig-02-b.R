# fig-02-b ----------------------------------------------------------------


library(ggplot2)

pos_ms <- read_csv("pos_ms.csv")
pos_msms <- read_csv("pos_msms.csv")
pos_sirius <- data.table::fread("../sirius_AMR2_pos/compound_identifications_adducts.tsv")
neg_ms <- read_csv("neg_ms.csv")
neg_msms <- read_csv("neg_msms.csv")
neg_sirius <- data.table::fread("../sirius_AMR2_neg/compound_identifications_adducts.tsv")

data_eva <- tibble::tibble(
  x = rep(c("MS", "MS/MS", "SIRIUS"), each = 2),
  y = c(
    length(pos_ms[[1]]) - 1, length(neg_ms[[1]]) - 1,
    length(pos_msms[[1]]) - 1, length(neg_msms[[1]]) - 1,
    length(pos_sirius[[1]]) - 1, length(neg_sirius[[1]]) - 1
  ),
  type = rep(c("positive", "negative"), 3)
)
data_eva$type <- factor(data_eva$type, levels = c("positive", "negative"))

p <- ggplot(data_eva, aes(x, y, fill = type)) +
  geom_bar(
    stat = 'identity',
    position = position_dodge(.5),
    width = 0.5, color = "black", 
    linewidth = 1
  ) +
  geom_text(
    aes(label = y), size = 4, vjust = -0.3, fontface = "bold", 
    position = position_dodge(.5)
  ) +
  scale_fill_manual(values = c("#00bfc4", "#f8766d")) +
  labs(x = NULL, y = "Number") +
  scale_y_continuous(
    limits = c(0, 27000),
    expand = c(0,0)
  ) +
  theme_prism(
    base_fontface = "bold",
    #base_family = "serif",
    base_size = 16,
    base_line_size = 0.8
  ) +
  theme(
    axis.text = element_text(face = "bold"), 
    legend.position = c(.8, .9),
    legend.text = element_text(face = "bold")
  )
p

ggsave("fig-02-b.pdf", p, width = 8, height = 5)
ggsave("fig-02-b.png", p, width = 8, height = 5, dpi = 600)
