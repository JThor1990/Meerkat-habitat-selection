# first load up the home range dynamics modelling script and run up until the model so that I have the basic home range data set with it's 
setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat habitat selection/Morning weight gain/")

# This produces a dataset called hr
# Then we also then need to link in the body wight data for each group row of the data

# Load in the body mass cleaned data: 

mass_fd <- "C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Body Mass Processing/Body mass cleaned data/"
mass_files <- list.files(mass_fd)
l1 <- list() 
for (j in 1:length(unique(mass_files))) {
    l1[[j]] <- read.csv(paste0(mass_fd, mass_files[j]) , header = TRUE)
}
bodymass <- do.call(rbind, l1)

MWG <- bodymass %>% 
  filter(Age_years > 1 & WeightSession != "E") %>% 
  group_by(IndividID, Date, WeightSession) %>%    # In case for some reason there are multiple wts/ind/session/day
  slice(1) %>% 
  group_by(IndividID) %>% 
  arrange(WeightTime) %>% 
  mutate(leadDate = as.Date(lead(Date)),  # easier than conditional statements
         leadSession = lead(WeightSession), 
         leadWeight = lead(Weight),
         leadWeightTime = lead(WeightTime)) %>% 
  filter(WeightSession == "M" &  leadSession == "L" & leadDate == Date) %>% 
  mutate(WeightGain_g = leadWeight - Weight, 
         TimeDifference = as.numeric(difftime(leadWeightTime, WeightTime, units = "hours")), 
         RateWeightGain_ghr = WeightGain_g/TimeDifference) %>% 
  dplyr::select(-RowRef, -WeightSession, -WeightQuality, -leadSession, -leadDate) %>% 
  rename(MorningWeight = Weight, 
         LunchWeight = leadWeight, 
         MorningWeightTime = WeightTime, 
         LunchWeightTime = leadWeightTime) %>% 
  dplyr::select(IndividID, Code, Sex, GroupRef, Date, BirthDate, Age_years, MorningWeightTime, 
                LunchWeightTime, MorningWeight, LunchWeight, WeightGain_g:RateWeightGain_ghr, FirstPregnant:PregnantLactating) %>% 
  data.frame()

# Filter any dodgy ones
hist(MWG$TimeDifference)
hist(MWG$RateWeightGain_ghr[MWG$RateWeightGain_ghr < 100])
MWG <- filter(MWG, between(TimeDifference, 2, 4.5)) %>% 
  filter(between(RateWeightGain_ghr, -10, 50))

# I will now approach this in two ways. The per-day approach.
# Firstly, taking the dominant habitat type used for foraging period that morning
# Need to load in the tracks first: 
tracks_df <- read.csv("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat habitat selection/Habitat selection Analyses/Tracks_Morn_Filt.csv", header = TRUE) %>% 
  filter(!is.na(GroupSize), 
         TimeForaging < 241) %>%  # consistent with SSF 
  filter(GroupSize > 1) %>% 
  filter(Date < as.Date("2024-01-01")) %>% 
  group_by(GroupRef, Date) %>% 
  mutate(MaxTimeForaging = max(TimeForaging)) %>% 
  data.frame()

# Get the dominant habitat type
domhabs <- tracks_df %>% 
  group_by(SessionID, Date, GroupRef, GroupName, GroupSize, nPups) %>% 
  rename(Pups = nPups) %>% 
  summarise(nGrassland = sum(HabType == "Grassland"), 
            nRedSandMix = sum(HabType == "RedSandMix"), 
            nWhiteSandMix = sum(HabType == "WhiteSandMix"), 
            nDrieDoring = sum(HabType == "DrieDoring")) %>% 
  pivot_longer(cols = starts_with("n"),  # or: nGrassland:nDrieDoring
    names_to = "Habitat",
    values_to = "Count") %>% 
  group_by(SessionID, Date, GroupRef, GroupName, GroupSize, Pups) %>%
  slice_max(order_by = Count, n = 1, with_ties = FALSE) %>%
  rename(DomHabitatType = Habitat) %>% 
  mutate(DomHabitatType = case_when(DomHabitatType == "nGrassland" ~ "Grassland",
                                    DomHabitatType == "nRedSandMix" ~ "RedSandMix", 
                                    DomHabitatType == "nWhiteSandMix" ~ "WhiteSandMix", 
                                    DomHabitatType == "nDrieDoring" ~ "DrieDoring")) %>%   ungroup()

# And get the burrow habitat in there as well (i.e. the first point)
firsthab <- tracks_df %>% 
  group_by(SessionID, BurrowLatitude, BurrowLongitude) %>% 
  slice(1) %>% 
  dplyr::select(SessionID, HabType, BurrowLatitude, BurrowLongitude) %>% 
  rename(BurrowHabitat = HabType) %>% 
  data.frame()

# join the two together
domhabs <- left_join(domhabs, firsthab)

# Now add some important variables to the daily data set 
domhabs <- domhabs %>% 
  mutate(Date = ymd(Date), 
         month = month(Date), 
         year = year(Date),
         breedingyear = case_when(Date >= as.Date(paste0(year,"-10-01")) ~ paste0(year,"/",year+1),
                                  Date < as.Date(paste0(year,"-10-01")) ~ paste0(year-1,"/",year)),
         season = if_else(month(Date)%in%c(10,11,12,1,2,3,4), "Oct-Apr", "May-Sep")) 

# Babysitting
babysitting_dates <- read.csv("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Home Ranges and Territory Overlap/Babysitting_GroupDates.csv", header = TRUE) %>% 
  mutate(Date = dmy(Date), Babysitting = "Yes")
# range(babysitting_dates$Date)

domhabs <- left_join(domhabs , babysitting_dates, by = c("GroupRef", "Date")) %>% 
  mutate(Babysitting = if_else(is.na(Babysitting), "N", "Y"), 
         anypups = factor(if_else(Pups > 0, "Y", "N"))) 

# Mutually exclude babysitting and then make a variable for PupPresence 
check <- domhabs %>% 
  dplyr::select(GroupRef, Date, Babysitting, anypups) %>% 
  distinct()
table(check$Babysitting, check$anypups)

domhabs <- domhabs %>% 
  mutate(anypups = case_when(Babysitting == "Y" & anypups == "Y" ~ "N", 
                             Babysitting == "Y" & anypups == "N" ~ "N", 
                             Babysitting == "N" & anypups == "N" ~ "N", 
                             Babysitting == "N" & anypups == "Y" ~ "Y"), 
         PupPresence = factor(case_when(Babysitting == "Y" ~ "babysitting", 
                                        anypups == "Y" ~ "pups_foraging", 
                                        TRUE ~ "neither")))

MWG$Date <- as.Date(MWG$Date)

df1 <- left_join(select(MWG, IndividID, GroupRef, Date, Age_years, Sex, WeightGain_g, RateWeightGain_ghr), 
                 domhabs, by = c("GroupRef", "Date")) %>% 
  filter(!is.na(DomHabitatType), Sex != "U") %>% 
  # set up the variables
  mutate(breedingyear = as.factor(breedingyear), 
         IndividID = factor(IndividID),
         season = as.character(season), 
         GroupSize_z = as.numeric(scale(GroupSize)), 
         Age_years_z = as.numeric(scale(Age_years))) %>% 
  rename(habitat = DomHabitatType) %>% 
  mutate(habitat = factor(case_when(habitat == "DrieDoring" ~ "Drie doring",
                             habitat == "Grassland" ~ "Grassland",
                             habitat == "RedSandMix" ~ "Red sand",
                             habitat == "WhiteSandMix" ~ "White sand"),
         levels = c("Drie doring", "Grassland", "Red sand", "White sand")), 
         BurrowHabitat = factor(case_when(BurrowHabitat  == "DrieDoring" ~ "Drie doring",
                                          BurrowHabitat  == "Grassland" ~ "Grassland",
                                          BurrowHabitat  == "RedSandMix" ~ "Red sand",
                                          BurrowHabitat  == "WhiteSandMix" ~ "White sand"),
                                levels = c("Drie doring", "Grassland", "Red sand", "White sand")))

# Check the agreement between burrow habitat and dominant foraging habitat
# We can compared the actual vs expected matches via a permutations test: 
#set.seed(123)
#habitat_match_rate <- mean(df1$BurrowHabitat == df1$habitat) # 56.7%
# Simulate expected match rate under independence
#null_match_rates <- replicate(1000, {
#  shuffled <- sample(df1$BurrowHabitat)
#  mean(shuffled == df1$habitat)
#})
#mean(null_match_rates >= habitat_match_rate) # how often was null as extreme <0.001
#hist(null_match_rates)


# Create a time variable which is the number of days from the first observation per individual
df1 <- df1 %>% 
  group_by(IndividID) %>% 
  mutate(timeindex = as.numeric(Date - min(Date))+1)

mean(df1$RateWeightGain_ghr) ; sd(df1$RateWeightGain_ghr)

mwg_mod <- lme(RateWeightGain_ghr ~ habitat*season + 
                   GroupSize_z  + I(GroupSize_z^2) + 
                   Age_years_z  + I(Age_years_z^2) +
                   Sex + 
                   PupPresence*season + 
                   breedingyear, 
                  random = list(GroupName = ~1, IndividID = ~1), 
                 correlation = corCAR1(form = ~ timeindex | GroupName/IndividID),
                 data = df1) 
summary(mwg_mod) 

# What about if we use burrow habitat instead 
#mwg_mod2 <- lme(RateWeightGain_ghr ~ BurrowHabitat*season + 
#                 GroupSize_z  + I(GroupSize_z^2) + 
#                 Age_years_z  + I(Age_years_z^2) +
#                 Sex + 
#                 PupPresence*season + 
#                 breedingyear, 
#                random = list(GroupName = ~1, IndividID = ~1), 
#                correlation = corCAR1(form = ~ timeindex | GroupName/IndividID),
#               data = df1) 
#summary(mwg_mod2) 
#plot(ggeffect(mwg_mod2, term = "BurrowHabitat"))
#plot(ggeffect(mwg_mod2, term = "season"))
#plot(ggeffect(mwg_mod2, terms = c("season", "BurrowHabitat")))

# Check for any residual spatial autocorrelation here in the first model
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
saveRDS(p_habitatseason, "p_mwg_habitat.RDS")
#ggsave("p_mwg_habitat.pdf", p_habitatseason, width = 4.25, height = 3, dpi = 450, device = cairo_pdf)
#ggsave("p_mwg_habitat.png", p_habitatseason, width = 4.25, height = 3, dpi = 450)

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
  mutate(GroupSize = GroupSize*sd(df1$GroupSize) + mean(df1$GroupSize),
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
  mutate(Age = Age*sd(df1$Age_years) + mean(df1$Age_years),
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
#saveRDS(p1_final, "p_mwg_other.RDS")
#ggsave("p_mwg_other.pdf", p1_final, width = 5.35, height = 8, dpi = 450, device = cairo_pdf)
#ggsave("p_mwg_other.png", p1_final, width = 5.35, height = 8, dpi = 450)

#=============================================================


########################### END OF SCRIPT #####################################