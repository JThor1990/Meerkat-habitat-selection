#########################################################################

# MODELLING THE MOVEMENT SPEEDS OF MEERKAT GROUPS ACCORDING TO HABITAT 

########################################################################

setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Intergroup Differences/")

# Load packages 
lapply(c("tidyverse", "lubridate", "RMySQL", "nlme", "ggeffects", "DHARMa", "emmeans", "sf", "spdep"), FUN = library, character.only = TRUE)

# Load in the data, either morning tracks interpolated or not: 
#tracks_df_15min <- read.csv("Habitat selection/Tracks_Morn_Filt_15min.csv", header = TRUE) 
tracks_df <- read.csv("Habitat selection/Tracks_Morn_Filt.csv", header = TRUE) %>% 
  filter(!is.na(Speed), 
         !is.na(GroupSize), 
         TimeForaging < 241) %>%  # consistent with SSF 
  filter(GroupSize > 1) %>% 
  filter(Date < as.Date("2024-01-01"))

# Note the speeds are in m/min. 
hist(tracks_df$Speed)
hist(tracks_df$Distance_lead)
filter(tracks_df, Speed > 50) 

# We will either model it as speed of movement per point, or the average speed in a 3 month period
# Then we can also consider to model it as the average speed in 3 month period
# For the latter we will want the home range data set which already defines each group according to the main habitat type for a 3-month period

# Now add some important variables to the pointwise data set
tracks_df <- tracks_df %>% 
  mutate(Date = ymd(Date), 
         Time = ymd_hms(Time),
         month = month(Date), 
         year = year(Date),
         breedingyear = case_when(Date >= as.Date(paste0(year,"-10-01")) ~ paste0(year,"/",year+1),
                                Date < as.Date(paste0(year,"-10-01")) ~ paste0(year-1,"/",year)),
       season = if_else(month(Date)%in%c(10,11,12,1,2,3,4), "Oct-Apr", "May-Sep")) %>%
  filter(year < 2024) %>% # we have no mass data for 2024
  mutate(year_f = as.factor(year),
         month_f = as.factor(month),
         SessionID = as.factor(SessionID))

# Add babysitting here and check the pups present 
#mkdb <- dbConnect(MySQL(),  username = "jack", password = "thor2015mr",
#                    dbname = 'Meerkatdatabase', 
#                    host = 'kalahariresearch.org')
#mkviews <- dbConnect(MySQL(),  username = "jack", password = "thor2015mr",
#                       dbname = 'MeerkatViews', 
#                       host = 'kalahariresearch.org')
#mkadlib <- dbConnect(MySQL(),  username = "jack", password = "thor2015mr",
#                       dbname = 'KMP_adlib_data', 
#                       host = 'kalahariresearch.org')

# Birth dates
#tblBirthdates <- dbReadTable(mkviews, "qry_BestBirthDates") %>% 
#  mutate(BirthDate = as.Date(BirthDate)) %>% 
#  right_join(tblIndividuals, by = "IndividID")

# Babysitting
babysitting_dates <- read.csv("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Home Ranges and Territory Overlap/Babysitting_GroupDates.csv", header = TRUE) %>% 
  mutate(Date = dmy(Date), Babysitting = "Yes")
# range(babysitting_dates$Date)

tracks_df <- left_join(tracks_df , babysitting_dates, by = c("GroupRef", "Date")) %>% 
  mutate(Babysitting = if_else(is.na(Babysitting), "N", "Y"), 
         anypups = factor(if_else(nPups > 0, "Y", "N"))) 

# Mutually exclude babysitting and then make a variable for PupPresence 
check <- tracks_df %>% 
  dplyr::select(GroupRef, Date, Babysitting, anypups) %>% 
  distinct()
table(check$Babysitting, check$anypups)

tracks_df <- tracks_df %>% 
  mutate(anypups = case_when(Babysitting == "Y" & anypups == "Y" ~ "N", 
                             Babysitting == "Y" & anypups == "N" ~ "N", 
                             Babysitting == "N" & anypups == "N" ~ "N", 
                             Babysitting == "N" & anypups == "Y" ~ "Y"), 
         PupPresence = factor(case_when(Babysitting == "Y" ~ "babysitting", 
                                 anypups == "Y" ~ "pups_foraging", 
                                 TRUE ~ "neither")))

#ggplot(tracks_df, aes(x = PupPresence, y = Speed)) + 
#  geom_boxplot(outliers = FALSE)

# Set up the three-monthly data set as well

#######################################

# Modelling movement speeds pointwise

########################################
 
# set up the variables
tracks_df <- tracks_df %>% 
  mutate(breedingyear = as.factor(breedingyear), 
         habitat = factor(case_when(HabType == "DrieDoring" ~ "Drie doring", 
                                    HabType == "Grassland" ~ "Grassland",
                                    HabType == "RedSandMix" ~ "Red sand",
                                    HabType == "WhiteSandMix" ~ "White sand"), 
                          levels= c("Grassland", "Red sand", "White sand", "Drie doring")))

# add an autocorr term for the date by each group
tracks_df <- tracks_df %>% 
  arrange(GroupRef, Time) %>% 
  group_by(GroupRef, Date) %>% 
  mutate(timeindex = 1:n()) %>% # TimeForaging is the count in minutes instead 
  data.frame()

# standardise the variables
tracks_df <- tracks_df %>% 
  mutate(GroupSize_z = as.numeric(scale(GroupSize)), 
         TimeForaging_z = as.numeric(scale(TimeForaging)))

# Look 
tracks_df %>% 
  group_by(habitat, season) %>% 
  summarise(meanSpeed = mean(Speed, na.rm = TRUE)) %>% 
  dplyr::arrange(season, habitat)

mean(tracks_df$Speed) ; sd(tracks_df$Speed)  # m/min
mean(tracks_df$Speed)*60 ; sd(tracks_df$Speed)*60  # m/hour

tracks_df$habitat <- as.character(tracks_df$habitat)
tracks_df$season <- as.character(tracks_df$season)
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
#tracks_df <- filter(tracks_df, Speed != 0)
#speed_mod <- glmmTMB(Speed ~ HabType*period + 
#                        GroupSize_z  + I(GroupSize_z^2) + 
#                        PupPresence + 
#                        TimeForaging_z + I(TimeForaging_z^2) + 
#                        breedingyear + 
#                        (1 |GroupName/Date),
#                      family = Gamma(link = "log"), 
#                      data = tracks_df) 
#summary(speed_mod)
#plot(simulateResiduals(speed_mod))

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
#saveRDS(p_habitatseason, "p_speed_habitat.RDS")
#ggsave("p_speed_habitat.pdf", p_habitatseason, width = 4.25, height = 3, dpi = 450, device = cairo_pdf)
#ggsave("p_speed_habitat.png", p_habitatseason, width = 4.25, height = 3, dpi = 450)

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
#saveRDS(p1_final, "p_speed_other.RDS")
#ggsave("p_speed_other.pdf", p1_final, width = 5.35, height = 8, dpi = 450, device = cairo_pdf)
#ggsave("p_speed_other.png", p1_final, width = 5.35, height = 8, dpi = 450)

#-----------------------------------------------------------------------------------------------------

# From the movement paths extract two other variables: 
# (1) The total daily travel distance 
# (2) The maximum distance from the burrow (a measure of displacement)

track_info <- tracks_df %>% 
  group_by(SessionID) %>% 
  summarise(totalTimeForaging = max(TimeForaging), 
            foraging_pl = sum(Distance_lead, na.rm = TRUE), 
            maxdistance = if_else(any(is.na(distburrow)), NA, max(distburrow))) %>% 
  data.frame()

firsthab <- tracks_df %>% 
  group_by(SessionID, BurrowLatitude, BurrowLongitude) %>% 
  slice(1) %>% 
  dplyr::select(SessionID, HabType, BurrowLatitude, BurrowLongitude) %>% 
  rename(BurrowHabitat = HabType) %>% 
  data.frame()

# join the two together and add additional necessary info
track_info <- left_join(track_info, firsthab) %>% 
  mutate(BurrowHabitat = factor(case_when(BurrowHabitat  == "DrieDoring" ~ "Drie doring",
                                   BurrowHabitat  == "Grassland" ~ "Grassland",
                                   BurrowHabitat  == "RedSandMix" ~ "Red sand",
                                   BurrowHabitat  == "WhiteSandMix" ~ "White sand"),
                         levels = c("Drie doring", "Grassland", "Red sand", "White sand"))) %>% 
  left_join(dplyr::select(tracks_df, SessionID, GroupName, Date, season, GroupSize, PupPresence, breedingyear) %>% distinct())

hist(track_info$foraging_pl)
hist(track_info$maxdistance)

#ggplot(track_info, aes(x = foraging_pl/totalTimeForaging)) + 
#  geom_density(aes(group = BurrowHabitat, fill = BurrowHabitat), alpha = 0.1) + 
#  facet_wrap(~season)

# Model the morning path length #-------------------------- 

track_info <- track_info %>% 
  group_by(GroupName) %>% 
  mutate(timeindex = as.numeric(Date - min(Date))+1)

pathlength_df <- track_info %>% 
  filter(!is.na(GroupSize), !is.na(foraging_pl), !is.na(timeindex)) %>% 
  mutate(GroupSize_z =  as.numeric(scale(GroupSize)),
         totalTimeForaging_z = as.numeric(scale(totalTimeForaging))) %>% 
  filter(!(SessionID %in% sessions_to_remove))

pathlength_mod <- lme(foraging_pl ~ BurrowHabitat*season + 
                         GroupSize_z + I(GroupSize_z^2) + 
                         PupPresence*season + 
                         totalTimeForaging_z + 
                         breedingyear,
                       random = list(GroupName = ~1), 
                       correlation = corCAR1(form = ~ timeindex | GroupName),
                       data = pathlength_df)
#hist(residuals(pathlength_mod, type = "normalized"))
summary(pathlength_mod)

#plot(ggeffect(pathlength_mod, terms = c("season", "BurrowHabitat")))
#plot(ggeffect(pathlength_mod, term = "GroupSize_z"))
#plot(ggeffect(pathlength_mod, term = c("PupPresence", "season")))
#plot(ggeffect(pathlength_mod, term = "breedingyear")) # Very similar to movement speeds 

# Extract the marginal means to compare the habitat contrasts per season
plot(emmeans(pathlength_mod, ~ BurrowHabitat + season)) # will be similar to marginal effect
pathlength_contrast <- emmeans(pathlength_mod, pairwise ~ BurrowHabitat, by="season")
pathlength_contrast <- data.frame(pathlength_contrast$contrasts) %>%
  mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
         Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
  mutate(estimate = estimate*-1)

# Plot the burrow habitat season interaction
mean(pathlength_df$totalTimeForaging)
p_habitatseason2 <- data.frame(ggeffect(pathlength_mod, terms = c("season", "BurrowHabitat")))

# convert to a more meaningful speed (m/min to m/hour)
# If I want the residuals:
#p_habitat_resid <- plot(p_habitat, show_residuals = TRUE) 
p_habitatseason2 <- p_habitatseason2 %>% 
  rename(season = x, habitat = group) %>% 
  mutate(season = factor(if_else(season == "May-Sep", "Dry (May-Sep)", "Wet (Oct-Apr)"), 
                         levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")),
         habitat = factor(habitat, levels = c("Grassland", "Red sand", "White sand", "Drie doring")), 
         predicted = predicted, 
         conf.low = conf.low, 
         conf.high = conf.high)

# Plotting habitat type by month
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
#saveRDS(p_habitatseason2, "p_pathlength_habitat.RDS")
#ggsave("p_pathlength_habitat.pdf", p_habitatseason2, width = 4.25, height = 3, dpi = 450, device = cairo_pdf)
#ggsave("p_pathlength_habitat.png", p_habitatseason2, width = 4.25, height = 3, dpi = 450)

# Plot the other variables
#p2 <- plot(ggeffect(pathlength_mod, terms = c("season", "BurrowHabitat")))
#p2a <- plot(ggeffect(pathlength_mod, terms = "GroupSize_z"))
#p2b <- plot(ggeffect(pathlength_mod, terms = c("PupPresence", "season")))
#p2c <- plot(ggeffect(pathlength_mod, terms = "breedingyear"))

#p2a + p2b + p2c + 
#  plot_layout(nrow = 2)

# Amend the plots myself to improve aesthetics
# Group size
p2a_dat <- data.frame(ggeffect(pathlength_mod, terms = "GroupSize_z [all]")) %>% 
  rename(GroupSize_z  = x) %>% 
  mutate(GroupSize = (GroupSize_z*sd(pathlength_df$GroupSize)) + mean(pathlength_df$GroupSize),
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
                     limits = c(400, 750)) + 
  scale_x_continuous(breaks = seq(0, 40, 10), limits = c(0, 40))

# Presence of pups by season
p2b_dat <- data.frame(ggeffect(pathlength_mod, terms = c("PupPresence [all]", "season [all]"))) %>% 
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
        legend.position = c(0.8, 0.85), 
        legend.box.background = element_rect(color = "black", linewidth = 0.5), 
        legend.margin = margin(2, 4, 2, 4)) +
  labs(x = "Pup presence", y =  "Morning track length\n(metres)", tag = "B", fill = NULL) + 
  scale_y_continuous(breaks = seq(450, 750, 50), 
                     labels = c("", 500, "", 600, "", 700 ,""), 
                     limits = c(400, 750)) + 
  scale_fill_manual(values = c("Dry" = "white", "Wet" = "lightblue"))  

# Breeding season
p2c_dat <- data.frame(ggeffect(pathlength_mod, terms = "breedingyear")) %>% 
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
                     limits = c(400, 750)) 

layout_matrix <- "AB
                  DD"

p2_final <- p2a + p2b + p2c +
  plot_layout(design = layout_matrix)
#saveRDS(p2_final, "p_pathlength_other.RDS")
#ggsave("p_pathlength_other.pdf", p2_final, width = 6, height = 6, dpi = 450, device = cairo_pdf)
#ggsave("p_pathlength_other.png", p2_final, width = 6, height = 6, dpi = 450)

#------------------------------------------------------------------------------------

# Model the maximum distance from the burrow #-------------------------- 

# Create a time variable which is the number of days from the first observation per group
#track_info <- track_info %>% 
#  group_by(GroupName) %>% 
#  mutate(timeindex = as.numeric(Date - min(Date))+1)

#track_info <- filter(track_info, !is.na(GroupSize), !is.na(maxdistance), !is.na(timeindex)) %>% 
#  mutate(GroupSize_z = as.numeric(scale(GroupSize)),
#         totalTimeForaging_z = as.numeric(totalTimeForaging)) %>% 
#  filter(!is.na(GroupSize_z))

#maxdistance_mod <- lme(maxdistance ~ BurrowHabitat*season + 
#                         GroupSize_z  + I(GroupSize_z^2) + 
#                         PupPresence*season + 
#                         totalTimeForaging_z + 
#                         breedingyear,
#                       random = list(GroupName = ~1), 
#                       correlation = corCAR1(form = ~ timeindex | GroupName),
#                       data = track_info)
#hist(residuals(pathlength_mod, type = "normalized"))

# check the residuals: 
#hist(residuals(maxdistance_mod, type = "normalized")) # right-skewed so may need to adjust
# They are a bit off

#plot(ggeffect(maxdistance_mod, terms = c("season")))
#plot(ggeffect(maxdistance_mod, terms = c("season", "BurrowHabitat")))
#plot(ggeffect(maxdistance_mod, term = "GroupSize"))
#plot(ggeffect(maxdistance_mod, term = "PupPresence"))
#plot(ggeffect(maxdistance_mod, term = c("PupPresence", "season")))
#plot(ggeffect(maxdistance_mod, term = "breedingyear"))
# Almost identical to movement speeds 

# Extract the marginal means to compare the habitat contrasts per season
#plot(emmeans(maxdistance_mod,~ BurrowHabitat + season)) # will be similar to marginal effect
#maxdist_contrast <- emmeans(maxdistance_mod, pairwise ~ BurrowHabitat, by="season")
#maxdist_contrast <- data.frame(maxdist_contrast$contrasts) %>%
#  mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
#         Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
#  mutate(estimate = estimate*-1) # easier to compare the levels

# Plot the burrow habitat season interaction
#p_habitatseason3 <- data.frame(ggeffect(maxdistance_mod, terms = c("season", "BurrowHabitat")))
# convert to a more meaningful speed (m/min to m/hour)
# If I want the residuals:
#p_habitat_resid <- plot(p_habitat, show_residuals = TRUE) 
#p_habitatseason3 <- p_habitatseason3 %>% 
#  rename(season = x, habitat = group) %>% 
#  mutate(season = factor(if_else(season == "May-Sep", "Dry (May-Sep)", "Wet (Oct-Apr)"), 
#                         levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")),
#         habitat = factor(habitat, levels = c("Grassland", "Red sand", "White sand", "Drie doring")), 
#         predicted = predicted, 
#         conf.low = conf.low, 
#         conf.high = conf.high)

# Plotting habitat type by month
#p_habitatseason3 <- ggplot(data = p_habitatseason3, 
#                          aes(x = season, y = predicted, colour = habitat, fill = habitat)) + 
#  ggplot2::annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 300, ymax = 560, 
#                    fill = "lightblue", alpha = 0.3) +  
#  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size = 0.8, width = 0, 
#                position = position_dodge(width = 0.65), 
#                show.legend = FALSE) + 
#  geom_point(shape = 21, colour = "black", size = 3, stroke = 0.8, 
#             position = position_dodge(width = 0.65)) + 
#  theme_bw() + 
#  theme(panel.grid = element_blank(), 
#        axis.text.x = element_text(colour = "black", size = 9.5),
#        axis.text.y = element_text(colour = "black", size = 9.5),
#        axis.title = element_text(colour = "black", size = 10.5), 
#        legend.text = element_text(size = 9.5), 
#        legend.title = element_text(hjust = 0.5)) + 
#  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood", "forestgreen")) +
#  scale_fill_manual(values = c("chocolate", "sandybrown", "burlywood", "forestgreen")) +
#  labs(fill = "Habitat", x = NULL, y =  "Maximum distance from\nsleeping burrow (metres)") + 
#  scale_y_continuous(breaks = seq(325, 550, 25), 
#                     labels = c("", 350, "", 400, "", 450, "", 500, "", 550)) + 
#  coord_cartesian(ylim = c(325, 550)) 
#saveRDS(p_habitatseason3, "p_maxdist_habitat.RDS")
#ggsave("p_maxdist_habitat3.pdf", p_habitatseason, width = 4.25, height = 3, dpi = 450, device = cairo_pdf)
#ggsave("p_maxdist_habitat3.png", p_habitatseason, width = 4.25, height = 3, dpi = 450)


######################  END OF SCRIPT #####################