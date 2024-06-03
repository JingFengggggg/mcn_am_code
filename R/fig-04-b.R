# fig4
library(MCnebula2)
library(exMCnebula2)
library(ggrepel)

load("mcn_hie4.rdata")
list_of_vectors <- select_features(mcn_hie4)
list_of_vectors <- Filter(
  function(x) length(x) != 0 && !all(x == ""), list_of_vectors
)
reference_strings <- exMCnebula2::select_features(
  mcn_hie4, , order_by_coef = 1, 
  coef=1, togather = T
)[1:200]

is_in_reference <- function(str, ref_list) {
  any(ref_list == str)
}

filter_vectors <- function(vec, ref_list) {
  vec[sapply(vec, function(x) is_in_reference(x, ref_list))]
}

filtered_list <- lapply(list_of_vectors, function(vec) {
  filter_vectors(vec, reference_strings)
})

filtered_list <- lapply(names(list_of_vectors), function(name) {
  filtered_vec <- filter_vectors(list_of_vectors[[name]], reference_strings)
  names(filtered_vec) <- NULL
  filtered_vec
})

names(filtered_list) <- names(list_of_vectors)

tracer <- filtered_list <- Filter(
  function(x) length(x) != 0 && !all(x == ""), filtered_list
)

# topclass <- names(child_nebulae(mcn2)@ggset)
# tracer <- select_features(mcn2, topclass, coef = 1:length(topclass))
# lapply(names(tracer),
#        function(name) {
#          tracer[[ name ]] <<- intersect(tracer[[ name ]], tops)
#       })
# tracer <- tracer[order(sapply(tracer, length), decreasing = TRUE)]
# tracer <- tracer[1:7]

df <- readr::read_csv(("temp_data/Pro - Raw.csv"))
df$group <- ifelse(
  df$adj.P.Val < 0.05 & abs(df$logFC) >= 1, 
  ifelse(df$logFC >= 1, "UP", "DOWN"), "NS"
)

df$group <- ifelse(df$logFC > 0, "UP", "DOWN")

for (j in 1:length(tracer)) {
  df$group[df$.features_id %in% tracer[[j]]] <- names(tracer)[j]
}

# for (i in 1:nrow(df)) {
#   for (j in 1:length(tracer)) {
#     if (df$.features_id[i] %in% tracer[[j]]) {
#       df$group[i] <- names(tracer)[j]
#     }
#   }
# }

#df$group <- factor(df$group, levels = c("DOWN", "NS", "UP", names(tracer)))
df$group <- factor(df$group, levels = c("DOWN", "UP", names(tracer)))

# df$label <- ifelse(df$adj.P.Val < 0.05,
#                    ifelse(abs(df$logFC) >= 10,"Y","N"),"N")
# df$label <- ifelse(df$label == "Y", as.character(df$.features_id), "")


p <- ggplot(df, aes(logFC, -log10(adj.P.Val))) + 
  geom_point(
    aes(
      color = group, 
      alpha = ifelse(adj.P.Val < 0.05 & abs(logFC) > 2, 1, .1),
      shape = group,
      size = ifelse(group == "UP" | group == "DOWN", 1, 2.5)
    )
  ) +
  scale_color_manual(
    values = c(
      c("#2878B5", "#c82423"), 
      #c("#FFA500", "#008000", "#FFFF00", "#800080", "#FF0000", "#00FF00", "#FF1493")
      colorRampPalette(palette_set(mcn_hie4))(length(filtered_list))
      )
  ) +
  scale_shape_manual(values = c(c(19 ,19), c(rep(17, length(filtered_list))))) +
  scale_size_continuous(range = c(1, 2.5)) +
  geom_vline(xintercept = c(-2, 2), lty = 3, color = 'black', lwd = 1) +
  #geom_vline(xintercept = c(-10, 10), lty = 3, color = "black", lwd = 1) +
  geom_hline(yintercept = -log10(0.05), lty = 3,color = 'black', lwd = 1) +
  # geom_text(
  #   aes(x = -3, y = -log10(0.05) - .1, label = c("P-value = 0.05")), 
  #   size = 5
  # ) +
  # geom_text_repel(
  #   aes(logFC, -log10(adj.P.Val), label = df$label),                       
  #   max.overlaps = 10000,
  #   size = 3,
  #   box.padding = unit(0.8,'lines'),
  #   point.padding = unit(0.8, 'lines'),
  #   segment.color = 'black',
  #   show.legend = FALSE
  # ) +
  labs(title = "Volcanoplot") +
  scale_y_continuous(name = expression(bold("-log"[10]*"(Pvalue)"))) +
  scale_x_continuous(name = expression(bold("log"[2]*"(Fold Change)"))) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    panel.grid = element_blank()
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 6, alpha = 1)), 
    alpha = "none", size = "none"
  )
p

ggsave("fig-04-b.png", p, width = 12, height = 7, dpi = 600)
ggsave("fig-04-b.pdf", p, width = 12, height = 7)
