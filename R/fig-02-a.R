# fig-02-a ----------------------------------------------------------------


library(tidyverse)
library(ggprism)
library(moonBook)
library(webr)

pos_ms <- read_csv("pos_ms.csv")
pos_msms <- read_csv("pos_msms.csv")
pos_sirius <- data.table::fread("../sirius_AMR2_pos/compound_identifications_adducts.tsv")
neg_ms <- read_csv("neg_ms.csv")
neg_msms <- read_csv("neg_msms.csv")
neg_sirius <- data.table::fread("../sirius_AMR2_neg/compound_identifications_adducts.tsv")

nums_stats <- function(x, y){
  n <- length(x[[1]]) - 1
  n2 <- length(y[[1]]) - 1
  data <- data.frame(
    id = 1:n,
    mode = ifelse(startsWith(names(x[4]), "pos"), rep("Positive", n), rep("Negative")),
    type = c(rep("Annotated", n2), rep("Un-Annotated", n - n2))
  )
  return(data)
}

pos_df <- nums_stats(pos_msms, pos_sirius)
neg_df <- nums_stats(neg_msms, neg_sirius)
don_df <- 
  bind_rows(neg_df, pos_df) |> 
  PieDonut(
    aes(mode, type),
    color = "black",
    pieAlpha = 0.9,
    r0 = 0.4,
    r1 = 0.7, 
    r2 = .9, 
    showRatioThreshold = 0.01,
    #start=pi/1.275,
    selected = c(1,3),
    explodeDonut = TRUE,
    labelposition = 0,
    pieLabelSize = 5.2,
    donutLabelSize = 4,
    showPieName = FALSE,
    showDonutName = FALSE
  ) +
  theme_void()

#建议手动导出, width = 10, height = 10
ggsave("fig-02-a.png", width = 8, height = 5, dpi = 600)