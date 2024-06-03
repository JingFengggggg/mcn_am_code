# fig-05-b ----------------------------------------------------------------


library(ggplot2)
library(patchwork)

set_df <- function(x, a, b){
  x <- tibble::tibble(
    Group = c("Match", "No-Match"),
    nums = c(a, b)
  )
  x <- x |> 
    dplyr::arrange(desc(x$Group)) |> 
    dplyr::mutate(pgroup = round(nums/sum(x$nums)*100)) |>  
    dplyr::mutate(lab.y = cumsum(pgroup) - 0.5*pgroup)
  return(x)
}
df_M <- set_df(df_M, 46, 54)
df_U <- set_df(df_U, 60, 40)
df_G <- set_df(df_G, 56, 44)
df_S <- set_df(df_S, 67, 33)

p1 <- ggplot(df_M , aes(x = '', y = nums, fill = Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.y, label = paste0(nums, "%")), size = 6) +
  scale_fill_manual(values = c("#C82423","#2878B5")) +
  labs(title = "Manual")+
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 0.01,
                                  size = 14, face = "bold"))
p1

p2 <- ggplot(df_U , aes(x = '', y = nums, fill = Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.y, label = paste0(nums, "%")), size = 6) +
  scale_fill_manual(values = c("#C82423","#2878B5")) +
  labs(title = "UNIFI")+
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 0.01,
                                  size = 14, face = "bold"))
p2  

p3 <- ggplot(df_G , aes(x = '', y = nums, fill = Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.y, label = paste0(nums, "%")), size = 6) +
  scale_fill_manual(values = c("#C82423","#2878B5")) +
  labs(title = "GNPS")+
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 0.01,
                                  size = 14, face = "bold"))
p3  

p4 <- ggplot(df_S , aes(x = '', y = nums, fill = Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.y, label = paste0(nums, "%")), size = 6) +
  scale_fill_manual(values = c("#C82423","#2878B5")) +
  labs(title = "SIRIUS- \n MCnebula2")+
  theme_void() +
  theme(legend.position = 'none',
    plot.title = element_text(hjust = 0.5, vjust = 0.01,
                                  size = 14, face = "bold"))
p4  

pb <- (p1 | p2) / (p3 | p4)
pb

ggsave("fig-05-b.tiff", units = "in", width = 5, height = 4, dpi = 600)
ggsave("fig-05-b.pdf", width = 5, height = 4)
