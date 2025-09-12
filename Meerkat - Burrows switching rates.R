########################################################

# R SCRIPT
# THE BIND OF THE BURROW: HABITAT SELECTION BY MEERKATS
# ANALYSIS OF BURROW SWITCHING RATES 
# Author: Jack Thorley (jackthorley1@gmail.com)

########################################################

setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat habitat selection/Final Github scripts/")

# load in the packages
lapply(c("tidyverse", "nlme", "glmmTMB", "ggeffects", "patchwork", "emmeans"), FUN = library, character.only = TRUE)
select <- dplyr::select

# load in the data
burrow_sequences <- read.csv("df_burrowsequences.csv", header = TRUE)

# Move probability
nrow(burrow_sequences) ; table(burrow_sequences$BurrowChange)
13112/(13112 + 28460) # Raw switch rate = 32%

table(burrow_sequences$BurrowChange[burrow_sequences$Babysitting == "N"])
11826/(11826 + 17938) # closer to 40% when not babysitting

# What proprtion of observations involved babysitting in wet vs dry
burrow_sequences %>% 
  group_by(season, Babysitting) %>% 
  summarise(n = n(), 
            PercentBurrowSwitching = sum(BurrowChange)/n())
8485/(8485 + 16241)   # wet season babysitting 34% of the time
3323 /(3323  +  13523)  # dry season 19.7%

# So grassland is heavily under-utilised as burrowing habitat generally 
table(burrow_sequences$habitat)


# Fit intial GLM ON P|burrow switch      -----------------------------------

# We will model breeding season as a factor to allow for us to look at per-year effects
# m1 <- glm(BurrowChange ~ habitat*season + GroupSize + I(GroupSize^2) + PupPresence*season + breedingyear, 
#          data = burrow_sequences, 
#          family = "binomial")
# summary(m1)

# plot(ggeffect(m1, term = "habitat"))
# plot(ggeffect(m1, term = "season"))
# plot(ggeffect(m1, terms = c("season", "habitat")))
# plot(ggeffect(m1, term = "GroupSize"))
# plot(ggeffect(m1, term = c("PupPresence", "season")))
# plot(ggeffect(m1, term = "breedingyear"))

# Translate the burrow swithcing GLM  into an LMMs with an appropriate random effects structure
burrow_sequences$GroupSize_z <- as.numeric(scale(burrow_sequences$GroupSize))
burrow_sequences$season <- as.character(burrow_sequences$season)
burrow_sequences$PupPresence <- as.character(burrow_sequences$PupPresence)

m1 <- glmmTMB(BurrowChange ~ habitat*season + GroupSize_z + I(GroupSize_z^2) + 
            PupPresence*season + breedingyear + (1|GroupRef) + (1|BurrowRef_New), 
            data = burrow_sequences, 
            family = "binomial")
summary(m1)

# Marginal effects
# plot(ggeffect(m1, term = "habitat"))
# plot(ggeffect(m1, term = "season"))
# plot(ggeffect(m1, terms = c("season", "habitat")))
# plot(ggeffect(m1, term = "GroupSize_z"))
# plot(ggeffect(m1, term = "PupPresence"))
# plot(ggeffect(m1, term = c("PupPresence", "season")))
# plot(ggeffect(m1, term = "breedingyear"))

# Extract the meginal means to compare the habitat contrasts per season
plot(emmeans(m1, ~ habitat + season)) # will be similar to marginal effect
burrow_contrast <- emmeans(m1, pairwise ~ habitat, by="season")
burrow_contrast <- data.frame(burrow_contrast$contrasts) %>%
  mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
         Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
  mutate(estimate = estimate*-1) # easier to compare the levels

# Check if there is any outstanding spatial effect:
#resids <- residuals(m1, type = "pearson")

#ggplot(burrow_sequences, aes(x = X, y = Y, fill = resids)) +
#  geom_point(shape = 21, size = 2) +
#  scale_fill_gradient2() +
#  theme_minimal() +
#  coord_equal() +
#  ggtitle("Spatial distribution of Pearson residuals")
# nothing obvious, supporting little need to accommodate spatial structure

# Generate the plots: 

# Plotting habitat type by month
p_habitatseason <- data.frame(ggeffect(m1, terms = c("season", "habitat")))
p_habitatseason <- p_habitatseason %>% 
  rename(season = x) %>% 
  mutate(season = factor(if_else(season == "May-Sep", "Dry (May-Sep)", "Wet (Oct-Apr)"), 
                         levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")),
         habitat = factor(case_when(group  == "Drie Doring" ~ "Drie doring", 
                                    group  == "Grassland" ~ "Grassland", 
                                    group  == "Red Sand" ~ "Red sand", 
                                    group == "White Sand" ~ "White sand"), 
                          levels = c("Grassland", "Red sand", "White sand", "Drie doring")))

# Plotting habitat type by season
p_habitatseason <- ggplot(data = p_habitatseason, 
                          aes(x = season, y = predicted, colour = habitat, fill = habitat)) + 
  ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.18, ymax = 0.52, 
                    fill = "lightblue", alpha = 0.3) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size = 0.8, width = 0, 
                position = position_dodge(width = 0.65), 
                show.legend = FALSE) + 
  geom_point(shape = 21, colour = "black", size = 3, stroke = 0.8, 
             position = position_dodge(width = 0.65)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 9.5),
        axis.text.y = element_text(colour = "black", size = 9.5),
        axis.title = element_text(colour = "black", size = 10.5), 
        legend.text = element_text(size = 9.5), 
        legend.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood", "forestgreen")) +
  scale_fill_manual(values = c("chocolate", "sandybrown", "burlywood", "forestgreen")) +
  labs(fill = "Habitat", x = NULL, y =  "Probabilty of burrow switch") + 
  scale_y_continuous(breaks = seq(0.2, 0.5, 0.05), 
                     labels = c(0.2, "", 0.3, "", 0.4, "", 0.5)) + 
  coord_cartesian(ylim = c(0.2, 0.5)) 
#saveRDS(p_habitatseason, "plot_burrowswitching_habitat.RDS")

# Plot the other variables
p1a <- plot(ggeffect(m1, terms = "GroupSize_z"))
p1b <- plot(ggeffect(m1, terms = c("PupPresence", "season")))
p1c <- plot(ggeffect(m1, terms = "breedingyear"))

p1a + p1b + p1c +  
  plot_layout(nrow = 2)

# Amend slightly so as un-scaled etc... 
plot_theme <-  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5),
        axis.title = element_text(colour = "black", size = 10)) 

# Group size
p1a_dat <- data.frame(ggeffect(m1, terms = "GroupSize_z [all]")) %>%  
  rename(GroupSize = x) %>% 
  mutate(GroupSize = GroupSize*sd(burrow_sequences$GroupSize) + mean(burrow_sequences$GroupSize))

p1a <- ggplot(p1a_dat, aes(x = GroupSize, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) + 
  geom_line(linewidth = 0.6) + 
  plot_theme + 
  labs(x = "Group size", y =  "Probability of burrow switch", tag = "A") + 
  scale_y_continuous(breaks = seq(0.1, 0.5, 0.05), 
                     labels = c(0.1, "", 0.2, "", 0.3, "", 0.4, "", 0.5)) + 
  coord_cartesian(ylim = c(0.1, 0.52)) 

# Presence of pups size
p1b_dat <- data.frame(ggeffect(m1, terms = c("PupPresence [all]", "season [all]"))) %>% 
  rename(PupPresence = x, 
         season = group) %>% 
  mutate(PupPresence = factor(case_when(PupPresence == "babysitting" ~ "Babysitting", 
                                        PupPresence == "pups_foraging" ~ "Foraging", 
                                        PupPresence == "neither" ~ "Neither"), 
                              levels = c("Babysitting", "Foraging", "Neither")), 
         season = if_else(season == "May-Sep", "Dry", "Wet"))

p1b <- ggplot(p1b_dat, aes(x = PupPresence, y = predicted, group = season, fill = season)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.8, 
                position = position_dodge(width = 0.4)) + 
  geom_point(shape = 21, colour = "black", size = 2.5, stroke = 0.8, 
             position = position_dodge(width = 0.4)) + 
  plot_theme + 
  theme(axis.text.x = element_text(size = 9), 
        legend.position = c(0.8, 0.2), 
        legend.box.background = element_rect(color = "black", linewidth = 0.5), 
        legend.margin = margin(2, 4, 2, 4)) +
  labs(x = "Pup presence", y = "Probability of burrow switch", 
       tag = "B", fill = NULL) + 
  scale_y_continuous(breaks = seq(0.1, 0.5, 0.05), 
                     labels = c(0.1, "", 0.2, "", 0.3, "", 0.4, "", 0.5)) + 
  coord_cartesian(ylim = c(0.1, 0.52)) +
  scale_fill_manual(values = c("Dry" = "white", "Wet" = "lightblue"))  

# Breeding season
p1c_dat <- data.frame(ggeffect(m1, terms = "breedingyear")) %>% 
  rename(breedingseason = x)

p1c <- ggplot(p1c_dat, aes(x = breedingseason, y = predicted)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.8) + 
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.8) + 
  plot_theme + 
  theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5)) +
  labs(x = "Breeding season", y =  "Probability of burrow switch", tag = "C") + 
  scale_y_continuous(breaks = seq(0.1, 0.5, 0.05), 
                     labels = c(0.1, "", 0.2, "", 0.3, "", 0.4, "", 0.5)) + 
  coord_cartesian(ylim = c(0.1, 0.55)) 

layout_matrix <- "AB
                  DD"

p1_final <- p1a + p1b + p1c + 
  plot_layout(design = layout_matrix)
#saveRDS(p1_final, "plot_burrowswitching_other.RDS")

########################## END OF SCRIPT ######################################