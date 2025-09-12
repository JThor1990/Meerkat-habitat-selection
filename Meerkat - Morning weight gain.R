########################################################

# R SCRIPT
# THE BIND OF THE BURROW: HABITAT SELECTION BY MEERKATS
# ANALYSIS OF MORNING WEIGHT GAIN 
# Author: Jack Thorley (jackthorley1@gmail.com)

########################################################

setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat habitat selection/Final Github scripts/")

# load in the packages
lapply(c("emmeans", "ggeffects", "lubridate", "nlme", "patchwork", "tidyverse"), FUN = library, character.only = TRUE)
select <- dplyr::select

# load in the data set
mwg_df <- read.csv("df_morningweightgain.csv", header = TRUE)

# Create a time variable which is the number of days from the first observation per individual
mwg_df <- mwg_df %>% 
  mutate(Date = dmy(Date), 
         IndividID = factor(IndividID), 
         breedingyear = factor(breedingyear)) %>% 
  arrange(IndividID, Date) %>% 
  group_by(IndividID) %>% 
  mutate(timeindex = as.numeric(Date - min(Date))+1) %>% 
  ungroup() %>% 
  mutate(GroupSize_z = as.numeric(scale(GroupSize)), 
         Age_years_z = as.numeric(scale(Age_years)))

mean(mwg_df$RateWeightGain_ghr) ; sd(mwg_df$RateWeightGain_ghr)

mwg_mod <- lme(RateWeightGain_ghr ~ habitat*season + 
                   GroupSize_z  + I(GroupSize_z^2) + 
                   Age_years_z  + I(Age_years_z^2) +
                   Sex + 
                   PupPresence*season + 
                   breedingyear, 
                  random = list(GroupName = ~1, IndividID = ~1), 
                 correlation = corCAR1(form = ~ timeindex | GroupName/IndividID),
                 data = mwg_df) 
summary(mwg_mod) 

# Check for any residual spatial autocorrelation here
# Can only do this using the burrow latitude and longitude filter but I want the full data set afterwards
# Also worth checking if there is any outstanding spatial structure in the residuals
# we'll randomly some of the data as it very large and computationally expensive on full dataset
#resid_df <- mutate(df1, resid = resid(mwg_mod))
#resid_df <- st_as_sf(resid_df, coords = c("BurrowLatitude", "BurrowLongitude"), crs = 4326)
#resid_df <- st_transform(resid_df, 32734) 
#set.seed(142)
#resid_sample <- resid_df[ sample(1:nrow(resid_df), 5000), ]
#coords <- st_coordinates(resid_sample)
#nb <- dnearneigh(coords, d1 = 0, d2 = 1000)  # neighbors within 1000 meters
#listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
#moran.test(resid_sample$resid, listw)
#Moran's I very close to 0 → very weak spatial autocorrelation remaining so will carry on without modelling spatial structure directly

# Extract the meginal means to compare the habitat contrasts per season
plot(emmeans(mwg_mod, ~ habitat + season)) # will be similar to marginal effect
mwg_contrast <- emmeans(mwg_mod, pairwise ~ habitat, by="season")
mwg_contrast <- data.frame(mwg_contrast$contrasts) %>%
  mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
         Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
  mutate(estimate = estimate*-1) # easier to compare the levels

# Get the marginal effects before "tidying" up the plots myself: 
plot(ggeffect(mwg_mod, term = "habitat"))
plot(ggeffect(mwg_mod, term = "season"))
plot(ggeffect(mwg_mod, terms = c("season", "habitat")))
plot(ggeffect(mwg_mod, term = "GroupSize_z"))
plot(ggeffect(mwg_mod, term = "Age_years_z"))
plot(ggeffect(mwg_mod, term = "PupPresence"))
plot(ggeffect(mwg_mod, term = c("PupPresence", "season")))
plot(ggeffect(mwg_mod, term = "breedingyear"))

# Plotting habitat type by season
p_habitatseason <- data.frame(ggeffect(mwg_mod, terms = c("season", "habitat")))
# If I want the residuals:
#p_habitat_resid <- plot(p_habitat, show_residuals = TRUE) 
p_habitatseason <- p_habitatseason %>% 
  rename(season = x, habitat = group) %>% 
  mutate(season = factor(if_else(season == "May-Sep", "Dry (May-Sep)", "Wet (Oct-Apr)"), 
                         levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")),
         habitat = factor(habitat, levels = c("Grassland", "Red sand", "White sand", "Drie doring")), 
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

# Plotting habitat type by month
p_habitatseason <- ggplot(data = p_habitatseason, 
                          aes(x = season, y = predicted, colour = habitat, fill = habitat)) + 
  ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 3.5, ymax = 9.5, 
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
  labs(fill = "Habitat", x = NULL, y =  "Rate of morning\nweight gain (g/hr)") + 
  scale_y_continuous(breaks = seq(4, 9, 0.5), labels = c(4, "", 5, "", 6, "", 7, "", 8, "", 9)) + 
  coord_cartesian(ylim = c(4, 9)) 
#saveRDS(p_habitatseason, "plot_mwg_habitat.RDS")

# Plot the other variables
p1a <- plot(ggeffect(mwg_mod, terms = "GroupSize_z"))
p1b <- plot(ggeffect(mwg_mod, terms = c("PupPresence", "season")))
p1c <- plot(ggeffect(mwg_mod, terms = c("Age_years_z")))
p1d <- plot(ggeffect(mwg_mod, terms = "breedingyear"))

p1a + p1b + p1c + p1a +
  plot_layout(nrow = 2)

# Amend slightly so as un-scaled etc... 
plot_theme <-  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5),
        axis.title = element_text(colour = "black", size = 10)) 

# Group size
p1a_dat <- data.frame(ggeffect(mwg_mod, terms = "GroupSize_z [all]")) %>% 
  rename(GroupSize = x) %>% 
  mutate(GroupSize = GroupSize*sd(mwg_df$GroupSize) + mean(mwg_df$GroupSize),
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

p1a <- ggplot(p1a_dat, aes(x = GroupSize, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) + 
  geom_line(linewidth = 0.6) + 
  plot_theme + 
  labs(x = "Group size", y = "Rate of morning\nweight gain (g/hr)", tag = "A") + 
  scale_y_continuous(breaks = seq(4, 9, 0.5), 
                     labels = c(4, "", 5, "", 6, "", 7, "", 8, "", 9), 
                     limits = c(4, 9))

# Presence of pups by season
p1b_dat <- data.frame(ggeffect(mwg_mod, terms = c("PupPresence [all]", "season [all]"))) %>% 
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
        legend.position = c(0.8, 0.85), 
        legend.box.background = element_rect(color = "black", linewidth = 0.5), 
        legend.margin = margin(2, 4, 2, 4)) +
  labs(x = "Pup presence", y = "Rate of morning\nweight gain (g/hr)", tag = "B", fill = NULL) + 
  scale_y_continuous(breaks = seq(4, 9.5, 0.5), 
                     labels = c(4, "", 5, "", 6, "", 7, "", 8, "", 9, ""), 
                     limits = c(4, 9.5)) +
  scale_fill_manual(values = c("Dry" = "white", "Wet" = "lightblue"))  

# Age 
p1c_dat <- data.frame(ggeffect(mwg_mod, terms = "Age_years_z [all]")) %>% 
  rename(Age = x) %>% 
  mutate(Age = Age*sd(mwg_df$Age_years) + mean(mwg_df$Age_years),
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high) %>% 
  filter(Age <= 10) # more realistic range

p1c <- ggplot(p1c_dat, aes(x = Age, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) + 
  geom_line(linewidth = 0.6) + 
  plot_theme + 
  labs(x = "Age (years)", y = "Rate of morning\nweight gain (g/hr)", tag = "C") + 
  scale_y_continuous(breaks = seq(4, 9, 0.5), 
                     labels = c(4, "", 5, "", 6, "", 7, "", 8, "", 9), 
                     limits = c(4, 9)) + 
  scale_x_continuous(breaks = 1:10)

# Breeding season
p1d_dat <- data.frame(ggeffect(mwg_mod, terms = "breedingyear")) %>% 
  rename(breedingyear = x) %>% 
  mutate(predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

p1d <- ggplot(p1d_dat, aes(x = breedingyear, y = predicted)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.8) + 
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.8) + 
  plot_theme + 
  theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5)) +
  labs(x = "Breeding season", y = "Rate of morning\nweight gain (g/hr)", tag = "D", fill = NULL) + 
  scale_y_continuous(breaks = seq(4, 9.5, 0.5), 
                     labels = c(4, "", 5, "", 6, "", 7, "", 8, "", 9, ""), 
                     limits = c(4, 9.5))

layout_matrix <- "AB
                  .C
                  DD"

p1_final <- p1c + p1a + p1b + plot_spacer() + p1d +
  plot_layout(design = layout_matrix)
#saveRDS(p1_final, "plot_mwg_other.RDS")

########################### END OF SCRIPT #####################################