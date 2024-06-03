# fig-02-d ----------------------------------------------------------------


library(MCnebula2)
library(exMCnebula2)
library(ggsci)

class_hie4 <- data.frame(
  class = factor(c("Level-2", "Level-3", "Level-4", "Level-5")),
  nums = c(3, 28, 31, 28)
) |> 
  dplyr::mutate(lab.y = cumsum(nums) - 0.5 * nums)

ggplot(class_hie4 , aes(x = "", y = nums, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  scale_fill_manual(values = c("#2878B5", "#9AC9DB", "#F8AC8C", "#C82423")) +
  coord_polar("y", start = -pi/2)+
  geom_text(aes(label = nums), color = "white", position = position_stack(vjust = .5))+
  #scale_fill_npg()+
  theme_void() +
  theme(legend.position = "left")
  guides(fill = "none")

ggsave("Level.png", width = 6, height = 3, dpi = 600)
ggsave("Level.pdf", width = 6, height = 3)


load("mcn_hie4.rdata")
classes_num <- data.frame(rev(sort(table(stardust_classes(mcn_hie4)$class.name))))
names(classes_num) <- c("classes", "nums")
classes_num <- classes_num |> 
  dplyr::mutate(classes = dplyr::case_when(
    dplyr::row_number() >= 11 ~ "others", 
    TRUE ~ classes
  )) |> 
  dplyr::group_by(classes) |> 
  dplyr::summarise(nums = sum(nums)) |> 
  dplyr::ungroup()
classes_num$classes <- factor(classes_num$classes, levels = classes_num$classes)

classes_num <- classes_num |>  
  dplyr::arrange(desc(classes_num$classes)) |>  
  dplyr::mutate(pclasses = round(nums/sum(classes_num$nums) * 100)) |>  
  dplyr::mutate(lab.y = cumsum(pclasses) - 0.5 * pclasses)

ggplot(classes_num , aes(x = "none", y = pclasses, fill = classes)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  scale_fill_manual(values = c("#63b2ee", "#76da91", "#f8cb7f", "#f89588", 
                               "#7cd6cf", "#9192ab", "#7898e1", "#efa666", 
                               "#eddd86", "#9987ce", "#c97937")
  ) +
  labs(fill = "Classes") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.y, label = nums), color = "white") +
  #xlim(c(0.2, 4 + 0.5)) +
  #scale_fill_ucscgb()+
  theme_void() +
  theme(legend.title = element_text(face = "bold"))

ggsave("fig-02-d.png", width = 8, height = 6, dpi = 600)
ggsave("fig-02-d.pdf", width = 8, height = 6)
