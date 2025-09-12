########################################################

# R SCRIPT
# THE BIND OF THE BURROW: HABITAT SELECTION BY MEERKATS
# ANALYSIS OF MOVEMENT SPEEDS AND TRACK LENGTHS 
# Author: Jack Thorley (jackthorley1@gmail.com)

########################################################

setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat habitat selection/Final Github scripts/")

# Load packages 
lapply(c("tidyverse", "lubridate", "nlme", "ggeffects", "emmeans", "DHARMa"), 
       FUN = library, character.only = TRUE)

# Load in the data
tracks_df <- read.csv("df_morningtracks.csv", header = TRUE) 
  
# Rework some of the variables before modelling
tracks_df <- mutate(tracks_df, 
                    Date = as.Date(Date),
                    breedingyear = as.factor(breedingyear), 
                    GroupSize_z = as.numeric(scale(GroupSize)), 
                    TimeForaging_z = as.numeric(scale(TimeForaging)))
         
# add an Autocorr term for the date by each group
tracks_df <- tracks_df %>% 
  arrange(GroupRef, Time) %>% 
  group_by(GroupRef, Date) %>% 
  mutate(timeindex = 1:n()) %>% # TimeForaging is the count in minutes instead 
  data.frame()

#--------------------------------
# Modelling movement speeds  
#--------------------------------

# Look at the raw average speeds for habitat|season
tracks_df %>% 
  group_by(habitat, season) %>% 
  summarise(meanSpeed = mean(Speed, na.rm = TRUE)) %>% 
  dplyr::arrange(season, habitat)

mean(tracks_df$Speed) ; sd(tracks_df$Speed)  # m/min
mean(tracks_df$Speed)*60 ; sd(tracks_df$Speed)*60  # m/hour

# convert the speed to m/hour rather than m/minute
tracks_df$Speed <- tracks_df$Speed*60

mean(tracks_df$Speed) ; sd(tracks_df$Speed) ; range(tracks_df$Speed)

speed_mod <- lme(Speed ~ habitat*season + 
                   GroupSize_z  + I(GroupSize_z^2) + 
                   PupPresence*season + 
                   TimeForaging_z + I(TimeForaging_z^2) +
                   breedingyear, 
                   random = list(GroupName = ~1, Date = ~1), 
                   correlation = corAR1(form = ~ timeindex | GroupName/Date),  
                   data = tracks_df) 
summary(speed_mod) 
hist(resid(speed_mod), breaks = 50) 
# slight skew but pretty decent and produces similar results to a gamma model without the corCAR1

# For the gamma can't have non-zero 
# tracks_df <- filter(tracks_df, Speed != 0)
# speed_mod <- glmmTMB(Speed ~ HabType*period + 
#                         GroupSize_z  + I(GroupSize_z^2) + 
#                         PupPresence + 
#                         TimeForaging_z + I(TimeForaging_z^2) + 
#                         breedingyear + 
#                         (1 |GroupName/Date),
#                       family = Gamma(link = "log"), 
#                       data = tracks_df) 
# summary(speed_mod)
# plot(simulateResiduals(speed_mod))

# Also worth checking if there is any outstanding spatial structure in the residuals
# we'll randomly some of the data as it very large and computationally expensive on full dataset
#resid_df <- mutate(tracks_df, resid = resid(speed_mod))
#resid_df <- st_as_sf(resid_df, coords = c("GPS_Longitude", "GPS_Latitude"), crs = 4326)
#resid_df <- st_transform(resid_df, 32734) 
#set.seed(142)
#resid_sample <- resid_df[ sample(1:nrow(resid_df), 5000), ]
#coords <- st_coordinates(resid_sample)
#nb <- dnearneigh(coords, d1 = 0, d2 = 1000)  # neighbors within 1000 meters
#listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
#moran.test(resid_sample$resid, listw)
#Moran's I very close to 0 → very weak spatial autocorrelation remaining so will carry on without modelling spatial structure directly

# Extract the marginal means to compare the habitat contrasts per season
plot(emmeans(speed_mod,~ habitat + season)) # will be similar to marginal effect
speed_contrast <- emmeans(speed_mod, pairwise ~ habitat, by="season")
speed_contrast <- data.frame(speed_contrast$contrasts) %>%
  mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
         Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
  mutate(estimate = estimate*-1) # easier to compare the levels

# Get the marginal effects before "tidying" up the plots myself: 
#plot(ggeffect(speed_mod, term = "habitat"))
#plot(ggeffect(speed_mod, term = "season"))
#plot(ggeffect(speed_mod, terms = c("season", "habitat")))
#plot(ggeffect(speed_mod, term = "GroupSize_z"))
#plot(ggeffect(speed_mod, term = "PupPresence"))
#plot(ggeffect(speed_mod, term = c("PupPresence", "season")))
#plot(ggeffect(speed_mod, term = "TimeForaging_z"))
#plot(ggeffect(speed_mod, term = "breedingyear"))

# Plotting habitat type by season
p_habitatseason <- data.frame(ggeffect(speed_mod, terms = c("season", "habitat")))
# convert to a more meaningful speed (m/min to m/hour)
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
  ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 140, ymax = 280, 
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
  labs(fill = "Habitat", x = NULL, y =  "Movement speed (m/hr)") + 
  scale_y_continuous(breaks = seq(150, 255, 15), 
                     labels = c("150", "", "180", "", "210", "", "240", "")) + 
  coord_cartesian(ylim = c(150, 260)) 
#saveRDS(p_habitatseason, "plot_movespeed_habitat.RDS")

# Plot the other variables
#p1 <- plot(ggeffect(speed_mod, terms = c("period", "HabType")))
#p1a <- plot(ggeffect(speed_mod, terms = "GroupSize_z"))
#p1b <- plot(ggeffect(speed_mod, terms = "PupPresence"))
#p1c <- plot(ggeffect(speed_mod, terms = "TimeForaging_z"))
#p1d <- plot(ggeffect(speed_mod, terms = "breedingyear"))

#p1a + p1b + p1c + p1d + 
#  plot_layout(nrow = 2)

# Amend slightly so as un-scaled etc... 
plot_theme <-  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5),
        axis.title = element_text(colour = "black", size = 10)) 

# Group size
p1a_dat <- data.frame(ggeffect(speed_mod, terms = "GroupSize_z [all]")) %>% 
  rename(GroupSize = x) %>% 
  mutate(GroupSize = GroupSize*sd(tracks_df$GroupSize) + mean(tracks_df$GroupSize),
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

p1a <- ggplot(p1a_dat, aes(x = GroupSize, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) + 
  geom_line(linewidth = 0.6) + 
  plot_theme + 
  labs(x = "Group size", y =  "Movement speed (m/hr)", tag = "A") + 
  scale_y_continuous(breaks = seq(150, 300, 15), 
                     labels = c("150", "", "180", "", "210", "", "240", "", "270", "", "300"), 
                     limits = c(150, 300))

# Presence of pups by season
p1b_dat <- data.frame(ggeffect(speed_mod, terms = c("PupPresence [all]", "season [all]"))) %>% 
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
        legend.position = c(0.8, 0.8), 
        legend.box.background = element_rect(color = "black", linewidth = 0.5), 
        legend.margin = margin(2, 4, 2, 4)) +
  labs(x = "Pup presence", y =  "Movement speed (m/hr)", tag = "B", fill = NULL) + 
  scale_y_continuous(breaks = seq(150, 300, 15), 
                     labels = c("150", "", "180", "", "210", "", "240", "", "270", "", "300"), 
                     limits = c(150, 300)) +
  scale_fill_manual(values = c("Dry" = "white", "Wet" = "lightblue"))  

# Time foraging
p1c_dat <- data.frame(ggeffect(speed_mod, terms = "TimeForaging_z [all]")) %>% 
  rename(TimeForaging = x) %>% 
  mutate(TimeForaging = (TimeForaging*sd(tracks_df$TimeForaging) + mean(tracks_df$TimeForaging))/60,
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

p1c <- ggplot(p1c_dat, aes(x = TimeForaging, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) + 
  geom_line(linewidth = 0.6) + 
  plot_theme + 
  labs(x = "Hours of foraging", y =  "Movement speed (m/hr)", tag = "C") + 
  scale_y_continuous(breaks = seq(150, 300, 15), 
                     labels = c("150", "", "180", "", "210", "", "240", "", "270", "", "300"), 
                     limits = c(150, 300))

# Breeding season
p1d_dat <- data.frame(ggeffect(speed_mod, terms = "breedingyear")) %>% 
  rename(breedingyear = x) %>% 
  mutate(predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

p1d <- ggplot(p1d_dat, aes(x = breedingyear, y = predicted)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.8) + 
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.8) + 
  plot_theme + 
  theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5)) +
  labs(x = "Breeding season", y =  "Movement speed (m/hr)", tag = "D") + 
  scale_y_continuous(breaks = seq(150, 300, 15), 
                     labels = c("150", "", "180", "", "210", "", "240", "", "270", "", "300"), 
                     limits = c(141, 300))

layout_matrix <- "AB
                  .C
                  DD"

p1_final <- p1c + p1a + p1b + plot_spacer() + p1d +
  plot_layout(design = layout_matrix)
#saveRDS(p1_final, "plot_movespeed_other.RDS")


#--------------------------------
# Modelling track length  
#--------------------------------

# Calculate morning track length 
track_info <- tracks_df %>% 
  group_by(SessionID) %>% 
  summarise(totalTimeForaging = max(TimeForaging), 
            tracklength = sum(Distance_lead, na.rm = TRUE), 
            #maxdistance = if_else(any(is.na(distburrow)), NA, max(distburrow))
            ) %>% 
  data.frame()

# Extract the first habitat of the day (the burrow habitat)
firsthab <- tracks_df %>% 
  group_by(SessionID, BurrowLatitude, BurrowLongitude) %>% 
  slice(1) %>% 
  dplyr::select(SessionID, habitat, BurrowLatitude, BurrowLongitude) %>% 
  rename(burrow_habitat = habitat) %>% 
  data.frame()

# join the two together and add additional necessary info
track_info <- left_join(track_info, firsthab) %>% 
  mutate(BurrowHabitat = factor(case_when(burrow_habitat  == "DrieDoring" ~ "Drie doring",
                                          burrow_habitat  == "Grassland" ~ "Grassland",
                                          burrow_habitat  == "RedSandMix" ~ "Red sand",
                                   burrow_habitat  == "WhiteSandMix" ~ "White sand"),
                         levels = c("Drie doring", "Grassland", "Red sand", "White sand"))) %>% 
  left_join(dplyr::select(tracks_df, SessionID, GroupName, Date, season, GroupSize, PupPresence, breedingyear) %>% distinct())

hist(track_info$tracklength)

# Get the temporal autocorrelation term and set up the variables
track_info <- track_info %>% 
  arrange(GroupName, Date) %>% 
  group_by(GroupName) %>% 
  mutate(timeindex = as.numeric(Date - min(Date))+1) %>% 
  data.frame()

filter(track_info, is.na(timeindex))

tracklength_df <- track_info %>% 
  filter(!is.na(GroupSize), !is.na(tracklength), !is.na(timeindex)) %>% 
  mutate(GroupSize_z =  as.numeric(scale(GroupSize)),
         totalTimeForaging_z = as.numeric(scale(totalTimeForaging))) 

tracklength_mod <- lme(tracklength ~ burrow_habitat*season + 
                         GroupSize_z + I(GroupSize_z^2) + 
                         PupPresence*season + 
                         totalTimeForaging_z + 
                         breedingyear,
                       random = list(GroupName = ~1), 
                       correlation = corCAR1(form = ~ timeindex | GroupName),
                       data = tracklength_df)
#hist(residuals(tracklength_mod, type = "normalized"))
summary(tracklength_mod)

#plot(ggeffect(tracklength_mod, terms = c("season", "burrow_habitat")))
#plot(ggeffect(tracklength_mod, term = "GroupSize_z"))
#plot(ggeffect(tracklength_mod, term = c("PupPresence", "season")))
#plot(ggeffect(tracklength_mod, term = "breedingyear")) # Very similar to movement speeds 

# Extract the marginal means to compare the habitat contrasts per season
plot(emmeans(tracklength_mod, ~ burrow_habitat + season)) # will be similar to marginal effect
tracklength_contrast <- emmeans(tracklength_mod, pairwise ~ burrow_habitat, by="season")
tracklength_contrast <- data.frame(tracklength_contrast$contrasts) %>%
  mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
         Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
  mutate(estimate = estimate*-1)

# Plot the burrow habitat season interaction
mean(tracklength_df$totalTimeForaging)

p_habitatseason2 <- data.frame(ggeffect(tracklength_mod, terms = c("season", "burrow_habitat")))

p_habitatseason2 <- p_habitatseason2 %>% 
  rename(season = x, habitat = group) %>% 
  mutate(season = factor(if_else(season == "May-Sep", "Dry (May-Sep)", "Wet (Oct-Apr)"), 
                         levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")),
         habitat = factor(habitat, levels = c("Grassland", "Red sand", "White sand", "Drie doring")), 
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

p_habitatseason2 <- ggplot(data = p_habitatseason2, 
                           aes(x = season, y = predicted, colour = habitat, fill = habitat)) + 
  ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 380, ymax = 720, 
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
  labs(fill = "Habitat", x = NULL, y =  "Morning track length\n(metres)") + 
  scale_y_continuous(breaks = seq(450, 700, 50), 
                     labels = c("", 500, "", 600, "", 700)) + 
  coord_cartesian(ylim = c(450, 700)) 
#saveRDS(p_habitatseason2, "plot_track_lengthength_habitat.RDS")

# Plot the other variables
#p2 <- plot(ggeffect(tracklength_mod, terms = c("season", "burrow_habitat")))
#p2a <- plot(ggeffect(tracklength_mod, terms = "GroupSize_z"))
#p2b <- plot(ggeffect(tracklength_mod, terms = c("PupPresence", "season")))
#p2c <- plot(ggeffect(tracklength_mod, terms = "breedingyear"))

#p2a + p2b + p2c + 
#  plot_layout(nrow = 2)

# Amend the plots myself to improve aesthetics
# Group size
p2a_dat <- data.frame(ggeffect(tracklength_mod, terms = "GroupSize_z [all]")) %>% 
  rename(GroupSize_z  = x) %>% 
  mutate(GroupSize = (GroupSize_z*sd(tracklength_df$GroupSize)) + mean(tracklength_df$GroupSize),
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high) %>% 
  filter(GroupSize >= 2)

p2a <- ggplot(p2a_dat, aes(x = GroupSize, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) + 
  geom_line(linewidth = 0.6) + 
  plot_theme + 
  labs(x = "Group size", y =  "Morning track length\n(metres)", tag = "A") + 
  scale_y_continuous(breaks = seq(450, 750, 50), 
                     labels = c("", 500, "", 600, "", 700 ,""), 
                     limits = c(400, 770)) + 
  scale_x_continuous(breaks = seq(0, 40, 10), limits = c(0, 40))

# Presence of pups by season
p2b_dat <- data.frame(ggeffect(tracklength_mod, terms = c("PupPresence [all]", "season [all]"))) %>% 
  rename(PupPresence = x, 
         season = group) %>% 
  mutate(PupPresence = factor(case_when(PupPresence == "babysitting" ~ "Babysitting", 
                                        PupPresence == "pups_foraging" ~ "Foraging", 
                                        PupPresence == "neither" ~ "Neither"), 
                              levels = c("Babysitting", "Foraging", "Neither")), 
         season = if_else(season == "May-Sep", "Dry", "Wet"))

p2b <- ggplot(p2b_dat, aes(x = PupPresence, y = predicted, group = season, fill = season)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.8, 
                position = position_dodge(width = 0.4)) + 
  geom_point(shape = 21, colour = "black", size = 2.5, stroke = 0.8, 
             position = position_dodge(width = 0.4)) + 
  plot_theme + 
  theme(axis.text.x = element_text(size = 9), 
        legend.position = c(0.82, 0.86), 
        legend.box.background = element_rect(color = "black", linewidth = 0.5), 
        legend.margin = margin(2, 4, 2, 4)) +
  labs(x = "Pup presence", y =  "Morning track length\n(metres)", tag = "B", fill = NULL) + 
  scale_y_continuous(breaks = seq(450, 750, 50), 
                     labels = c("", 500, "", 600, "", 700 ,""), 
                     limits = c(400, 770)) + 
  scale_fill_manual(values = c("Dry" = "white", "Wet" = "lightblue"))  

# Breeding season
p2c_dat <- data.frame(ggeffect(tracklength_mod, terms = "breedingyear")) %>% 
  rename(breedingyear = x) %>% 
  mutate(predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

p2c <- ggplot(p2c_dat, aes(x = breedingyear, y = predicted)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.8) + 
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.8) + 
  plot_theme + 
  theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5)) +
  labs(x = "Breeding season", y =  "Morning track length\n(metres)", tag = "C") + 
  scale_y_continuous(breaks = seq(450, 750, 50), 
                     labels = c("", 500, "", 600, "", 700 ,""), 
                     limits = c(400, 770)) 

layout_matrix <- "AB
                  DD"

p2_final <- p2a + p2b + p2c +
  plot_layout(design = layout_matrix)
#saveRDS(p2_final, "plot_tracklength_other.RDS")

#########################  END OF SCRIPT ######################################