install.packages("tidyverse")
install.packages("ggalluvial")
install.packages("shades")


library(tidyverse)
library(ggalluvial)
library(shades)


read.csv("CPM consistency_22022021.csv")

cpms <- read.csv("CPM consistency_22022021.csv")

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
  mutate(Implementation = ifelse(Implementation == "Missing values considered normal",
                                 "Missing values \n considered normal",
                                 ifelse(Implementation == "Unknown category for missingness",
                                        "Unknown category \n for missingness",
                                        Implementation))) %>% 
  

  ggplot(aes(y = Freq_weighted, axis1 = Development,
             axis2 = Validation, axis3 = Implementation)) + 
  geom_alluvium(aes(fill = Development)) +
  guides(fill = FALSE) +
  
  geom_stratum(width = 1/3, alpha = 0, reverse = TRUE) +
  geom_text(stat = "stratum", size = 3, aes(label = after_stat(stratum)),
            reverse = TRUE) +
  theme_minimal() +
  

  theme(plot.title = element_text(hjust = 0.5)) +
  
  ggtitle("Missing Data Handling Across the Pipeline of CPMs") +
  scale_x_continuous(breaks = 1:3, 
                     labels = c("Development", "Validation", "Implementation")) +
  
  
  
  theme(axis.text = element_text(size = 16, face = "bold")) +
  

  
  scale_fill_manual(values = (c(A_col, B_col, C_col))) +
  ylab(NULL)

saturation(c(A_col, B_col, C_col))




