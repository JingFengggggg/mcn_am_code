# fig-04-c ----------------------------------------------------------------


library(MCnebula2)
library(exMCnebula2)
library(patchwork)

load("mcn_hie4.rdata")
tops <- select_features(
  mcn_hie4, order_by_coef = 1, coef=1,
  togather = T
)[1:200]

# logfc > .3
back <- test <- top_table(statistic_set(mcn_hie4))[[1]]
test <- dplyr::filter(test, logFC > 0.3)
top_table(statistic_set(mcn_hie4))[[1]] <- test

topclass <- names(child_nebulae(mcn_hie4)@ggset)
top_up <- select_features(mcn_hie4, topclass, coef = 1:length(topclass))
lapply(names(top_up),
       function(name) {
         top_up[[ name ]] <<- intersect(top_up[[ name ]], tops)
       })
top_up <- top_up[order(sapply(top_up, length), decreasing = TRUE)]
length_up <- sapply(top_up, length)
df_up <- data.frame(classes = names(top_up), number = length_up)

p_up <- ggplot(
  df_up,
  aes(x = reorder(classes, number), y = number)
  ) +
  geom_bar(
    stat = "identity",
    fill = "#c82423", 
    #color = "yellow",
    width = 0.8,
    size = 0.05
  ) +
  labs(
    title = "The number of top-features contained in the AM classes",
    x = "AM-Classes",
    y = "Number of top-features"
  ) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  coord_flip() +
  #theme_linedraw() +
  theme(
    plot.title = element_text(hjust = .5, face = "bold"), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_blank()
  ) 
p_up

# logfc < .3 
top_table(statistic_set(mcn_hie4))[[1]] <- back
test <- top_table(statistic_set(mcn_hie4))[[1]]
test <- dplyr::filter(test, logFC < 0.3)
top_table(statistic_set(mcn_hie4))[[1]] <- test

top_down <- select_features(mcn_hie4, topclass, coef = 1:length(topclass))
lapply(names(top_down),
       function(name) {
         top_down[[ name ]] <<- intersect(top_down[[ name ]], tops)
       })
top_down <- top_down[order(sapply(top_down, length), decreasing = TRUE)]
length_down <- sapply(top_down, length)
df_down <- data.frame(classes = names(top_down), number = length_down)

p_down <- ggplot(
  df_down,
  aes(x = reorder(classes, number), y = number)
) +
  geom_bar(
    stat = "identity",
    fill = "#2878B5", 
    #color = "yellow",
    width = 0.8,
    size = 0.05
  ) +
  labs(
    title = "The number of top-down-features contained in the AM classes",
    x = "AM-Classes",
    y = "Number of top-features"
  ) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  coord_flip() +
  #theme_linedraw() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_blank()
  ) 
p_down

p <- p_up / p_down
p

ggsave(
  "fig-04-c.png", p, 
  width = 9, height = 8, dpi = 600
)
ggsave(
  "fig-04-c.pdf", p, 
  width = 9, height = 8
)
