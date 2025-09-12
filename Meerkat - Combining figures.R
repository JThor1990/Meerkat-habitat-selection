########################################################

# R SCRIPT
# THE BIND OF THE BURROW: HABITAT SELECTION BY MEERKATS
# COMBINING ALL THE FIGURES FOR THE PAPER
# Author: Jack Thorley (jackthorley1@gmail.com)

########################################################

setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat habitat selection/Final Github scripts/")

library(patchwork) ; library(ggplot2)

# FIGURE 1:-----------------------------

# Photographs of meerkats at the burrow produced outside of R 

# FIGURE 2:-----------------------------

# call in the habitat map, overall space use, and the resource selection functions
p1a <- readRDS("Plots/plot_habitatclass_map.RDS")
p1b <- readRDS("Plots/plot_habitatclass_spaceuse.RDS")
p1c <- readRDS("Plots/plot_RSFburrow_season.RDS")
p1d <- readRDS("Plots/plot_RSFforaging_season.RDS")

# finalise the first
p1a <- p1a + theme(legend.position = "right") 
p1b <- p1b + theme(axis.text = element_text(colour = "black")) 
# update the tags on 1c and 1d
p1c <- p1c + labs(tag = "C")
p1d <- p1d + labs(tag = "D")

p1_upper <- (p1a + p1b)
p1_lower <- (p1c/p1d)
#ggsave("Figure2_upper.pdf", p1_upper, device = "pdf", units = "in", width = 9.5, height = 6.0, dpi = 600)
#ggsave("Figure2_upper.png", p1_upper, device = "png", units = "in", width = 9.5, height = 6.0, dpi = 600)
#ggsave("Figure2_lower.pdf", p1_lower, device = "pdf", units = "in", width = 5.5, height = 6.0, dpi = 600)
#ggsave("Figure2_lower.png", p1_lower, device = "png", units = "in", width = 5.5, height = 6.0, dpi = 600)

# FIGURE 3:-----------------------------

# Produced within the resource selection function script

# FIGURE 4:-----------------------------

# Call in the burrow switching plot

p_burrow <- readRDS("Plots/plot_burrowswitching_habitat.RDS")

p_burrow <- p_burrow + 
  labs(x = NULL, 
       y = "\nProbability of burrow switch", 
       tag = "A") + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 9), 
        axis.text.y = element_text(size = 10)) 
  
p_burrow <- p_burrow + 
  # wet season
  annotate("segment", x = 0.92, xend = 1.08, y = 0.48, yend = 0.48, size  = 0.6, colour = "gray50") + 
  annotate("text", x = 1, y = 0.485, label = "*", size = 4, colour = "gray50") +
  # dry season
  annotate("segment", x = 1.92, xend = 2.24, y = 0.41, yend = 0.41, size  = 0.6, colour = "gray50") + 
  annotate("segment", x = 1.92, xend = 2.08, y = 0.418, yend = 0.418, size  = 0.6, colour = "gray50") + 
  annotate("text", x = 2.08, y = 0.424, label = "*", size = 4, colour = "gray50")

# Call in the morning weight gain plot 
p_weightgain <- readRDS("Plots/plot_mwg_habitat.RDS")

p_weightgain <- p_weightgain + 
  labs(x = NULL, tag = "B") + 
  theme(#legend.position = "none", 
        axis.text.x = element_text(size = 9), 
        axis.text.y = element_text(size = 10))

p_weightgain <- p_weightgain + 
  # wet season
  annotate("segment", x = 0.75, xend = 1.24, y = 8.72, yend = 8.72, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.75, xend = 1.08, y = 8.82, yend = 8.82, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.92, xend = 1.24, y = 8.92, yend = 8.92, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.92, xend = 1.08, y = 9.02, yend = 9.02, size = 0.5, colour = "gray50") + 
  annotate("text", x = 1, y = 9.1, label = "*", size = 4, colour = "gray50") +
  # dry season
  annotate("segment", x = 1.75, xend = 2.24, y = 6, yend = 6, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 1.75, xend = 2.08, y = 6.1, yend = 6.1, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 1.92, xend = 2.24, y = 6.2, yend = 6.2, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 1.92, xend = 2.08, y = 6.3, yend = 6.3, size = 0.5, colour = "gray50")  + 
  annotate("text", x = 2, y = 6.37, label = "*", size = 4, colour = "gray50")


# Call in the movement speed plot 
p_speed <- readRDS("Plots/plot_movespeed_habitat.RDS")

p_speed <- p_speed + 
  labs(x = NULL, 
       y = "Movement speed\n(metres/hr)",
       colour = NULL, 
       tag = "C") + 
  theme(legend.position = "none", 
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 10))

p_speed <- p_speed + 
  # wet season
  annotate("segment", x = 0.75, xend = 1.24, y = 244, yend = 244, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.75, xend = 1.08, y = 246, yend = 246, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.92, xend = 1.24, y = 248, yend = 248, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.92, xend = 1.08, y = 250, yend = 250, size = 0.5, colour = "gray50") + 
  annotate("text", x = 1, y = 252, label = "*", size = 4, colour = "gray50") +
  # dry season
  annotate("segment", x = 1.75, xend = 2.24, y = 216, yend = 216, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 1.75, xend = 2.08, y = 218, yend = 218, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 1.92, xend = 2.24, y = 220, yend = 220, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 1.92, xend = 2.08, y = 222, yend = 222, size = 0.5, colour = "gray50")  + 
  annotate("text", x = 2, y = 224, label = "*", size = 4, colour = "gray50")


# Call in the path length plot
p_tracklength <- readRDS("Plots/plot_tracklength_habitat.RDS")

p_tracklength <- p_tracklength + 
  labs(x = NULL, 
       y = "Morning track length\n(metres)",
       colour = NULL, 
       tag = "D") + 
  theme(legend.position = "none", 
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 10))

p_tracklength <- p_tracklength + 
  # wet season
  annotate("segment", x = 0.75, xend = 1.24, y = 690, yend = 690, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.75, xend = 1.08, y = 694, yend = 694, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.92, xend = 1.24, y = 698, yend = 698, size = 0.5, colour = "gray50") + 
  annotate("segment", x = 0.92, xend = 1.08, y = 702, yend = 702, size = 0.5, colour = "gray50") + 
  annotate("text", x = 1, y = 705, label = "*", size = 4, colour = "gray50") +
  # dry season
  annotate("segment", x = 1.92, xend = 2.24, y = 586, yend = 586, size = 0.5, colour = "gray50") +
  annotate("segment", x = 1.92, xend = 2.08, y = 590, yend = 590, size = 0.5, colour = "gray50") + 
  annotate("text", x = 2.08, y = 594, label = "*", size = 4, colour = "gray50") 

p3 <- p_burrow + p_weightgain + p_speed + p_tracklength + 
  plot_layout(nrow = 2)
#ggsave("Figure4.pdf", p3, device = "pdf", units = "in", width = 7.25, height = 5.25, dpi = 600)
#ggsave("Figure4.png", p3, device = "png", units = "in", width = 7.25, height = 5.25, dpi = 600)

############################# END OF SCRIPT ####################################