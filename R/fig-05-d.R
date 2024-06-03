# fig-05-d ----------------------------------------------------------------


if(!requireNamespace("waffle"))install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(patchwork)

set_df <- function(x,a,b,c){
  x <- c(
    `Unique-Match` = a,
    Match = b,
    `No-Match` = c
  )
  return(x)
}

df_M <- set_df(df_M,16,30,54)
df_U <- set_df(df_U,15,45,40)
df_G <- set_df(df_G,21,35,44)
df_S <- set_df(df_S,32,35,33) 

p1 <- waffle(df_M, rows = 4, colors = c("#54B345","#C82423","#2878B5"))+
  labs(y='Manual')+
  theme(legend.position = 'none',
        axis.title.y = element_text(face = 'bold'))

p2 <- waffle(df_U, rows = 4, colors = c("#54B345","#C82423","#2878B5"))+
  labs(y='UNIFI')+
  theme(legend.position = 'none',
        axis.title.y = element_text(face = 'bold'))

p3 <- waffle(df_G, rows = 4, colors = c("#54B345","#C82423","#2878B5"))+
  labs(y='GNPS')+
  theme(legend.position = 'none',
        axis.title.y = element_text(face = 'bold'))

p4 <- waffle(df_S, rows = 4, colors = c("#54B345","#C82423","#2878B5"))+
  labs(y="SIRIUS- \n MCnebula2")+
  theme(axis.title.y = element_text(face = 'bold'))

pd <- p1/p2/p3/p4
pd

# planB
library(waffle)

set_df <- function(x, a, b, c){
  x <- data.frame(group = as.factor(c("Unique-Match", "Match", "Un-Match")), 
                  fill = c(a, b, c)
  )
  return(x)
}

df_M <- set_df(df_M,16,30,54)
df_U <- set_df(df_U,15,45,40)
df_G <- set_df(df_G,21,35,44)
df_S <- set_df(df_S,32,35,33) 

p1 <- ggplot(df_M, aes(fill = group, values = fill)) +
  geom_waffle(n_rows = 4, size = .33, color = "white", na.rm = TRUE) +
  scale_fill_manual(name = NULL, 
                    values = c("#54B345","#C82423","#2878B5"), 
                    labels = df_M$group) +
  coord_equal() +
  theme_void() +
  labs(y = "Manual") +
  theme(legend.position = 'none',
        axis.title.y = element_text(angle = 90, face = "bold"))
p1 

ggsave(
  paste0("waffle_", 1, ".tiff"), p1, 
  units = "in", width = 4, height = 2, dpi = 600
)
ggsave(
  paste0("waffle_", 1, ".pdf"), p1, 
  width = 4, height = 2
)




p2 <- ggplot(df_U, aes(fill = group, values = fill)) +
  geom_waffle(n_rows = 4, size = .33, color = "white", na.rm = TRUE) +
  scale_fill_manual(name = NULL, 
                    values = c("#54B345","#C82423","#2878B5"), 
                    labels = df_U$group) +
  coord_equal() +
  theme_void() +
  labs(y = "UNIFI") +
  theme(legend.position = 'none',
        axis.title.y = element_text(angle = 90, face = "bold"))
p2 

ggsave(
  paste0("waffle_", 2, ".tiff"), p2, 
  units = "in", width = 4, height = 2, dpi = 600
)
ggsave(
  paste0("waffle_", 2, ".pdf"), p2, 
  width = 4, height = 2
)

p3 <- ggplot(df_G, aes(fill = group, values = fill)) +
  geom_waffle(n_rows = 4, size = .33, color = "white", na.rm = TRUE) +
  scale_fill_manual(name = NULL, 
                    values = c("#54B345","#C82423","#2878B5"), 
                    labels = df_G$group) +
  coord_equal() +
  theme_void() +
  labs(y = "GNPS") +
  theme(legend.position = 'none',
        axis.title.y = element_text(angle = 90, face = "bold"))
p3 

ggsave(
  paste0("waffle_", 3, ".tiff"), p3, 
  units = "in", width = 4, height = 2, dpi = 600
)
ggsave(
  paste0("waffle_", 3, ".pdf"), p3, 
  width = 4, height = 2
)

p4 <- ggplot(df_S, aes(fill = group, values = fill)) +
  geom_waffle(n_rows = 4, size = .33, color = "white", na.rm = TRUE) +
  scale_fill_manual(name = NULL, 
                    values = c("#54B345","#C82423","#2878B5"), 
                    labels = df_S$group) +
  coord_equal() +
  theme_void() +
  labs(y = "SIRIUS- \n MCnebual2") +
  theme(legend.position = 'none',
        axis.title.y = element_text(angle = 90, face = "bold"))
p4 

ggsave(
  paste0("waffle_", 4, ".tiff"), p4, 
  units = "in", width = 4, height = 2, dpi = 600
)
ggsave(
  paste0("waffle_", 4, ".pdf"), p4, 
  width = 4, height = 2
)
