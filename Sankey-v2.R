library(tidyverse)
library(ggalluvial)
library(stringr)

cpms <- read.csv("CPM consistency_22022021-2.csv")

A_col <- "blueviolet"
B_col <- "firebrick2"
C_col <- "chartreuse2"

cpms %>%
  group_by(CPM) %>% 
  mutate(n_this_CPM = n()) %>% 
  ungroup() %>% 
  group_by(Development, Validation, Implementation) %>%
  summarise(Freq = n(),
            Freq_weighted = n() / mean(n_this_CPM) * 10) %>%
  mutate(
    Development = str_wrap(Development, width = 20),
    Validation = str_wrap(Validation, width = 20),
    Implementation = str_wrap(Implementation, width = 20)
  ) %>% 
  
  ggplot(aes(y = Freq_weighted, axis1 = Development,
             axis2 = Validation, axis3 = Implementation)) + 
  geom_alluvium(aes(fill = Development), width = 0.4, knot.pos = 0.5) +  # Adjusted width and knot position
  guides(fill = FALSE) +
  geom_stratum(width = 0.4, alpha = 0.5) +
  geom_text(stat = "stratum", size = 5, aes(label = after_stat(stratum)),
            reverse = TRUE, lineheight = 1.2, fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 18, face = "bold"),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_x_continuous(breaks = 1:3, 
                     labels = c("Development", "Validation", "Implementation")) +
  scale_fill_manual(values = c(A_col, B_col, C_col)) +
  ylab(NULL)
