########################################################

# R SCRIPT
# THE BIND OF THE BURROW: HABITAT SELECTION BY MEERKATS
# STEP SELECTION ANALYSIS (HABITAT SELECTION AT LOCAL SCALE) 
# Author: Jack Thorley (jackthorley1@gmail.com) & Christopher Duncan 

########################################################

setwd("INSERT FILE PATH")

# load the packages
lapply(c("adehabitatLT", "amt", "emmeans", "geosphere", "ggplot2", "glmmTMB", "job", "lubridate", "patchwork", "sf", "terra", "tidyverse", "broom.mixed"), FUN = library, character.only = TRUE)

# load the data: 
tracks_df <- read.csv("df_morningtracks.csv", header = TRUE) # morning tracks
d <- paste0("Habitat map/habClPanRiver.shp")  # Habitat Map

# The script has three broad sections. In the first section we interpolate the morning tracks (i). In the second, we create the randomised movement steps (ii), and in the third, we fit the various step selection analyses (iii). 
# You can run through all sections, but to replicate the results exactly, you would need to load the model data sets for part iii, which starts on line 250. You can also load in the model outputs here to save the run time. 

#---------------------------------------------
# (i) Get the interpolated morning tracks (15 minute intervals)
#----------------------------------------------

# Rework some of the variables:
tracks_df <- tracks_df %>% 
  mutate(Date = dmy(Date), 
         BurrowLeaveTime = dmy_hms(BurrowLeaveTime), 
         Time = dmy_hms(Time)) 
  
# Look at the track data before interpolation
length(unique(tracks_df$SessionID)) # for main text need this after interpolating
length(unique(tracks_df$GroupRef))  # ditto 
  
# What is the distribution of step lengths before interpolation
ggplot(tracks_df, aes(x = TimeDiff_lead)) + 
  geom_histogram(binwidth = 1, fill = "blue", colour = "black") + 
  labs(title = "Histogram of Time Foraging",
       x = "Time Steps (minutes)",
       y = "Frequency") +
  theme_minimal()
mean(tracks_df$TimeDiff_lead, na.rm = TRUE)
sd(tracks_df$TimeDiff_lead, na.rm = TRUE)
  
# Number of foraging steps
distinct(tracks_df, SessionID, nPoints) %>%
  .$nPoints %>% 
  table()
  
#Total time spent foraging
tracks_df %>%
    distinct(SessionID, TimeForaging) %>%
    mutate(TimeForaging = TimeForaging / 60) %>%
    ggplot(aes(x = TimeForaging)) +
    geom_histogram(binwidth = 1, fill = "blue", colour = "black") +
    labs(title = "Histogram of Time Foraging",
         x = "Total Time Foraging (hours)", y = "Frequency") +
    theme_minimal()
  
# Need to interpolate at 15 minute resolution across all samples
# First convert to UTM and keep only the necessary information
tracks_df <- tracks_df %>%
    st_as_sf(coords = c("GPS_Longitude", "GPS_Latitude"), crs = 4326) %>% # Set CRS as WGS84
    st_transform(crs = "+proj=utm +zone=34 +south +datum=WGS84") %>% 
    mutate(Easting = st_coordinates(.)[, 1],  
           Northing = st_coordinates(.)[, 2])  %>%
    st_drop_geometry()  %>% 
    dplyr::select(-habitat, -Distance_lead, -TimeDiff_lead, -Speed) %>% 
  # also provide an additional unique identifier for the track 
    arrange(Date) %>% 
    group_by(SessionID) %>% 
    mutate(id = cur_group_id()) %>% 
    arrange(id) %>% 
    data.frame()
  
# Now set up as ltraj object
ltraj <- as.ltraj(xy = tracks_df[, c("Easting", "Northing")],  
                  date = tracks_df$Time,                  
                  id = as.factor(tracks_df$GroupRef),
                  burst = as.factor(tracks_df$SessionID),
                  typeII = TRUE)  # Time-explicit
  
# Interpolate the trajectory at 15-minute intervals
job({ ltraj_interp <- redisltraj(ltraj, u = 15*60, type = "time") }) # ~ 10 minutes
  
# Visualise an interpolated track to check the method 
# e.g. of a higher resolution track: 
#unique(filter(tracks_df, SessionID == "10 2004-06-29")$id)
#plot(ltraj[81], perani=FALSE)
#plot(ltraj_interp[81], perani=FALSE) # doing a good job it seems. 
  
# Convert the trajectories back to a dataframe
track_sessions <- distinct(select(read.csv("df_morningtracks.csv", header = TRUE),  SessionID, GroupName))
tracks_df <- ld(ltraj_interp) 
tracks_df <- tracks_df %>% 
  rename(Easting = x, Northing = y, GroupRef = id, SessionID = burst, Time = date) %>% 
  dplyr::select(GroupRef, SessionID, Easting, Northing, Time) %>% 
  left_join(track_sessions, by = "SessionID") %>%
  group_by(SessionID) %>% 
  mutate(TrackID = cur_group_id()) %>%   # get a unique TrackID
  ungroup() %>% 
  arrange(TrackID) %>% 
  dplyr::select(TrackID, GroupRef, GroupName, SessionID, Easting, Northing, Time) %>% 
  data.frame()
      
# Check the final interpolated data set for foraging track points: 
head(tracks_df)
  
#get the relevant information about the tracks used for SSF
length(unique(tracks_df$TrackID)) ; length(unique(tracks_df$GroupName))
  
# Export this interpolate data set for use 
#write.csv(tracks_df, "df_morningtracks15min", row.names = FALSE)
  
# Clean up
#rm(ltraj, ltraj_interp, tracks)

#---------------------------------
# Append the necessary habitat data to the interpolate tracks
tracks_df <- read.csv("df_morningtracks15min.csv", header = TRUE) %>% 
  mutate(time = ymd_hms(time), date = as.Date(time)) %>% 
  data.frame()

# rework the habitat data a bit
o <- terra::vect(d) %>% st_as_sf()
o <- st_set_crs(o, "+proj=utm +zone=34 +south +datum=WGS84")

# Based on the % of the different habitats merge some categories
o <- o %>%
  mutate(Class_Name = case_when( Class_Name == 'DD' ~ 'Drie Doring',
                                 Class_Name == 'grassland' ~ 'Grassland',
                                 Class_Name == 'RS mix' ~ 'Red Sand', 
                                 Class_Name %in% c('WS mix', "pan", "Riverbed") ~ 'White Sand'))

# To get the function to work I need to convert the veg cover map into a spatial raster
# For this is makes sense to to check the resolution of the features in the polygon data
st_bbox(o)  # Get the bounding box
min_feature_size <- sqrt(min(st_area(o)))  # Minimum feature size is small 
# so 25 metre resolution seems fine

# Convert sf to a SpatVector
vect_o <- vect(o)
rast_o <- rast(ext(vect_o), resolution = 25)
raster_o <- rasterize(vect_o, rast_o, field = "Class_Name")
#plot(raster_o, colNA = "red")
raster_o <- mask(raster_o, mask = !is.na(raster_o))
#plot(raster_o)

#---------------------------------------------
# (ii) Get the randomised steps (i.e., set up the step selection analysis)
#----------------------------------------------

# Generate "track" types using the amt package 
# given the data structure, we should expect ~ 130,000 steps
job({ alltracks <- tracks_df %>% 
   mutate(TrackID = as.numeric(TrackID), 
          TrackID2 = TrackID) %>% 
   arrange(time) %>% 
   nest(data = -TrackID) %>% 
   mutate(track = lapply(data, function(d) {
     amt::make_track(d, .x = Easting, .y = Northing, .t = time, burst_ = TrackID2,
                     crs = crs("+proj=utm +zone=34 +south +datum=WGS84"))
   }))
})  # ~ 3 minutes # n = 12886 tracks

# Need to add burst_ id to each xyt_track object so that later steps run
alltracks <- alltracks %>%
  mutate(track = map2(track, TrackID, ~ mutate(.x, burst_ = .y)))  
  
# based on the sample size per track, we should expect 132922 steps.
alltracks %>% 
  unnest(track) %>% 
  group_by(burst_) %>% 
  summarise(n = n()) %>% 
  mutate(n = n-1) %>% 
  summarise(sum(n)) #

alltracks <- bind_rows(alltracks$track)
alltracks_burst <- arrange(steps_by_burst(alltracks), burst_, t1_) # 132922 steps

# The sample size will drop slightly because n-1 steps per burst can provide a random step:
alltracks_burst %>% 
  group_by(burst_) %>% 
  summarise(n = n()) %>% 
  mutate(n = n - 1) %>% 
  summarise(sum(n)) # so 120056 steps can provide a random step

# and on this basis, we should expect 2401120 overall locations with 1:19 ratio per step
set.seed(1066)
alltracks_df <- random_steps(alltracks_burst, n_control = 19) #--> 2401120 rows. 

# Extract habitat variables
alltracks_df <- alltracks_df %>% 
  extract_covariates(raster_o, where = "both") %>% 
  rename(habitat_start = Class_Name_start, 
         habitat_end = Class_Name_end) %>% 
  # I won't be analysing step lengths within this framework (would make it an iSSA)
  # mutate(cos_ta = cos(ta_), 
  #       log_sl_ = log(sl_)) %>% 
  data.frame()

alltracks_df <- data.frame(alltracks_df)
length(unique(alltracks_df$burst_)) # 12866 tracks/bursts still
length(unique(alltracks_df$step_id_)) # 120056 steps (a small number have NA turn angles)

# left_join relevant information back on: 
alltracks_df <- alltracks_df %>% 
  rename(TrackID = burst_, Stratum = step_id_) %>% 
  left_join(distinct(select(tracks_df, date, TrackID, month:season, GroupName, GroupRef, GroupSize, nPups))) %>% 
  mutate(SessionID = paste0(GroupRef, " ", date)) %>% 
  arrange(TrackID, Stratum)

 # filter out a small number of steps (strata) where the habitat_end was an NA
 NA_steps <- unique(alltracks_df$Stratum[is.na(alltracks_df$habitat_end)]) # n = 27 steps
 alltracks_df <- filter(alltracks_df, !(Stratum %in% NA_steps)) 
 #alltracks_df %>% filter(if_any(everything(), is.na))
  
 # and where the turning angle is NA. 
 NA_steps2 <- unique(alltracks_df$Stratum[is.na(alltracks_df$cos_ta)]) 
 alltracks_df <- filter(alltracks_df, !(Stratum %in% NA_steps2)) 
 #saveRDS(alltracks_df, "df_SSA_data.RDS")
 
#------------------------------------------
# (iii) Run the step selection analsis (SSA) via glmmTMB
#---------------------------------------------

alltracks_df <- readRDS("df_SSA_data.rds")
#mkat_ssf_mod1 <- readRDS("model_SSA_dry.rds")
#mkat_ssf_mod2 <- readRDS("model_SSA_wet.rds")

# get the relevant information about the tracks used for SSF
length(unique(alltracks_df$TrackID)) ; 
length(unique(alltracks_df$GroupName)) ; 
length(unique(alltracks_df$Stratum))

#---------------------------------------------------- 
# Set up a dry-season model (SSA MODEL 1) : May-Sep
#    and a wet-season model (SSA MODEL 2) : Oct-Apr
# ----------------------------------------------------
 
alltracks_df$Stratum <- as.factor(alltracks_df$Stratum)
alltracks_df$GroupName <- as.factor(alltracks_df$GroupName)
alltracks_df$habitat_end <- as.factor(alltracks_df$habitat_end)
alltracks_df <- alltracks_df %>% 
  group_by(SessionID) %>% 
  mutate(habitat_end = factor(habitat_end, 
                              levels = c("Drie Doring", "Grassland", "Red Sand", "White Sand"))) %>% 
  ungroup()  

# Dry season model 
alltracks_df$season
alltracks_dry <- filter(alltracks_df, season == "May-Sep") %>% 
  mutate(GroupName = droplevels(GroupName), 
         case_ = if_else(case_ == "FALSE", 0, 1)) 

alltracks_wet <- filter(alltracks_df, season == "Oct-Apr") %>% 
  mutate(GroupName = droplevels(GroupName),
         case_ = if_else(case_ == "FALSE", 0, 1)) 

# Set up following Muff et al. 2020 using the "poisson" trick 
# Dry season model
mkat_ssf_mod1_config <- glmmTMB(case_ ~ habitat_end +
                                 (1 | Stratum) +
                                 (0 + habitat_end || GroupName), # uncorr random slopes
                               REML = TRUE, 
                               family = poisson, 
                               data = alltracks_dry, 
                               doFit = FALSE, 
                               map = list(theta=factor(c(NA, 1:4))),
                               start = list(theta=c(log(1e3), rep(0, 4))))

job( { mkat_ssf_mod1 <- glmmTMB:::fitTMB(mkat_ssf_mod1_config) })  # takes 2 mins
summary(mkat_ssf_mod1)
#saveRDS(mkat_ssf_mod1 , "model_SSA_dry.rds")

# Wet-season model
mkat_ssf_mod2_config <- glmmTMB(case_ ~ habitat_end +
                                  (1 | Stratum) +
                                  (0 + habitat_end || GroupName), # uncorr random slopes
                                REML = TRUE, 
                                family = poisson, 
                                data = alltracks_wet, 
                                doFit = FALSE, 
                                map = list(theta=factor(c(NA, 1:4))),
                                start = list(theta=c(log(1e3), rep(0, 4))))

job( { mkat_ssf_mod2 <- glmmTMB:::fitTMB(mkat_ssf_mod2_config) })  # takes two minutes 
summary(mkat_ssf_mod2)  
#saveRDS(mkat_ssf_mod2, "model_SSA_wet.rds")

#------------------------------------------------------------------
# Plot and interpret habitat-selection parameters for each model 
#------------------------------------------------------------------

# The coefficients for categorical predictors reflect use:availability ratios for each level of the predictor relative to the use:availability ratio for the reference class. 

# Drie Doring is the reference habitat level. Qualitatively, for this season, we can see that the relative selection strength of the different habitat classes is grassland < drie doring < white sand < red sand. 

# We can quantify the relative selection strength assuming all habitat types are equally available using the model coefficients. To generate a relative, we need to exponentiate use intensity ("Selection intensity"), we need to exponentiate the coefficients. 

fixef_habitat_dry <- tidy(mkat_ssf_mod1, effects = "fixed") %>% 
  select(term, estimate, std.error) %>% 
  rename(fe_estimate = estimate) %>%
  mutate(fe_abs_logintensity = if_else(term == "(Intercept)", 
                                       fe_estimate,  
                                       fe_estimate + fe_estimate[term == "(Intercept)"]), 
         term = case_when(term == "(Intercept)" ~ "Drie doring", 
                          term == "habitat_endWhite Sand" ~ "White sand",
                          term == "habitat_endGrassland" ~ "Grassland", 
                          term == "habitat_endRed Sand" ~ "Red sand"), 
         season = "Dry (May-Sep)") %>% 
  rename(habitat = term)  %>% 
  data.frame()

fixef_habitat_wet <- tidy(mkat_ssf_mod2, effects = "fixed") %>% 
  select(term, estimate, std.error) %>% 
  rename(fe_estimate = estimate) %>%
  mutate(fe_abs_logintensity = if_else(term == "(Intercept)", 
                                       fe_estimate,  
                                       fe_estimate + fe_estimate[term == "(Intercept)"]), 
         term = case_when(term == "(Intercept)" ~ "Drie doring", 
                          term == "habitat_endWhite Sand" ~ "White sand",
                          term == "habitat_endGrassland" ~ "Grassland", 
                          term == "habitat_endRed Sand" ~ "Red sand"), 
         season = "Wet (Oct-Apr)") %>% 
  rename(habitat = term) %>% 
  data.frame()

fixef_habitat <- bind_rows(fixef_habitat_dry, fixef_habitat_wet) %>% 
  mutate(season = factor(season, levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")))

# Plot the relative selection intensity in each season, ignoring group effects
p1 <- ggplot(filter(fixef_habitat, habitat != "Drie doring"), aes(x = habitat, y = exp(fe_estimate))) + 
  geom_hline(yintercept = 1, linetype = 2, colour = "forestgreen") +
  geom_errorbar(aes(ymin = exp(fe_estimate - 1.96*std.error), 
                    ymax = exp(fe_estimate + 1.96*std.error), 
                    colour = habitat), width = 0, linewidth = 0.8) +
  geom_point(size = 4.5, aes(colour = habitat)) +
  geom_point(size = 4.5, shape = 1, colour = "black") +
  facet_wrap(~season) +
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]})), 
       tag= "(a)") +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text = element_text(colour = "black", size = 9.5), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11), 
        strip.background = element_blank(), 
        plot.tag.position = c(0, 1),   
        plot.tag = element_text(hjust = 0, vjust = 1)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) +   
  scale_y_continuous(breaks = seq(0.5, 1.5, 0.1), 
                     labels = c("", 0.6, "", 0.8,  0.9, 1, 1.1, 1.2, 1.3, 1.4, ""))
p1

# Now work with plotting the group differences 
# The random effect for each GroupName reflects the deviation of that group's response from the overall population-level effect of habitat_end.
# So to also get selection intensity as above, we need to make all group-level estimates relative to overall White sand.

fe_dry <- fixef(mkat_ssf_mod1)$cond
re_dry <- ranef(mkat_ssf_mod1)$cond$GroupName
groupe_dry <- sweep(re_dry, 2, fe_dry[names(re_dry)], FUN = "+")
ranef_habitat_dry <- as.data.frame(groupe_dry) %>%
  tibble::rownames_to_column("GroupName") %>%
  pivot_longer(cols = -GroupName, names_to = "habitat", values_to = "effect") %>% 
  filter(habitat != "habitat_endDrie Doring") %>% 
  mutate(habitat = case_when(habitat == "habitat_endWhite Sand" ~ "White sand",
                             habitat == "habitat_endGrassland" ~ "Grassland", 
                             habitat == "habitat_endRed Sand" ~ "Red sand"), 
         season = "Dry (May-Sep)")

fe_wet <- fixef(mkat_ssf_mod2)$cond
re_wet <- ranef(mkat_ssf_mod2)$cond$GroupName
groupe_wet <- sweep(re_wet, 2, fe_wet[names(re_wet)], FUN = "+")
ranef_habitat_wet <- as.data.frame(groupe_wet) %>%
  tibble::rownames_to_column("GroupName") %>%
  pivot_longer(cols = -GroupName, names_to = "habitat", values_to = "effect") %>% 
  filter(habitat != "habitat_endDrie Doring") %>% 
  mutate(habitat = case_when(habitat == "habitat_endWhite Sand" ~ "White sand",
                             habitat == "habitat_endGrassland" ~ "Grassland", 
                             habitat == "habitat_endRed Sand" ~ "Red sand"), 
         season = "Wet (Oct-Apr)")

ranef_habitat <- bind_rows(ranef_habitat_dry, ranef_habitat_wet) %>% 
  mutate(season = factor(season, levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")))

p2 <- ggplot(ranef_habitat) + 
  geom_boxplot(aes(x = habitat, y = exp(effect)), outliers = FALSE) +
  geom_hline(yintercept = 1, linetype = 2, colour = "forestgreen") +
  geom_point(aes(x = habitat, y = exp(effect)),
             position = position_jitter(width = 0.2), shape = 1) +
  geom_errorbar(data = filter(fixef_habitat, habitat != "Drie doring"), 
                aes(x = habitat, ymin = exp(fe_estimate - 1.96*std.error), 
                    ymax = exp(fe_estimate + 1.96*std.error), 
                    colour = habitat), width = 0, linewidth = 0.8, alpha = 0.6) +
  geom_point(data = filter(fixef_habitat, habitat != "Drie doring"), 
             aes(x = habitat, y = exp(fe_estimate), colour = habitat), size = 4.5, alpha = 0.6) +
  geom_point(data = filter(fixef_habitat, habitat != "Drie doring"), 
             aes(x = habitat, y = exp(fe_estimate), colour = habitat), size = 4.5, 
             shape = 1, alpha = 0.6, colour = "black") +
  facet_wrap(~season) +
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]})), 
       tag = "(b)") +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text = element_text(colour = "black", size = 9.5), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.tag.position = c(0, 1),   
        plot.tag = element_text(hjust = 0, vjust = 1)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) + 
  scale_y_continuous(breaks = seq(0.5, 2, 0.1), 
                     labels = c("", 0.6, "", 0.8,  "", 1, "", 1.2, "",1.4, "", 
                                1.6, "", 1.8, "", 2.0))
p2 

#---------------------

# We can also look at all two-way comparisons to test the significance in the trends in the different seasons 
# dry season comparisons 

habitat_means_dry <- emmeans(mkat_ssf_mod1, ~ habitat_end)
habitat_contrasts_dry <- data.frame(pairs(habitat_means_dry, adjust = "tukey")) %>% 
  mutate(useratio = exp(estimate), 
         habitat1 = c("Drie doring", "Drie doring", "Drie doring", "Grassland", 
                      "Grassland", "Red sand"), 
         habitat2 = c("Grassland", "Red sand", "White sand", "Red sand", 
                      "White sand", "White sand")) %>% 
  select(contrast, habitat1, habitat2, estimate:useratio) %>% 
  mutate(estimate = -1*estimate)

# wet season comparisons 
habitat_means_wet <- emmeans(mkat_ssf_mod2, ~ habitat_end)
habitat_contrasts_wet <- data.frame(pairs(habitat_means_wet, adjust = "tukey")) %>% 
  mutate(useratio = exp(estimate), 
         habitat1 = c("Drie doring", "Drie doring", "Drie doring", "Grassland", 
                      "Grassland", "Red sand"), 
         habitat2 = c("Grassland", "Red sand", "White sand", "Red sand", 
                      "White sand", "White sand")) %>% 
  select(contrast, habitat1, habitat2, estimate:useratio) %>% 
  mutate(estimate = -1*estimate)

#---------------------

# We also want to calculate the probability of use

# To do this, we need to correct for the availability of the habitat types in the data
  
 # Starting with the dry season
 # first we get the absolute intensity of use in each season
 fixef_habitat_dry <- mutate(fixef_habitat_dry, fe_abs_intensity = exp(fe_abs_logintensity))
 
 # then we get the per-habitat availability
 dryseason_availability <- alltracks_dry %>% 
   filter(case_ == 0) %>% 
   mutate(habitat_end = as.character(habitat_end) ,
          habitat_end = case_when(habitat_end == "White Sand" ~ "White sand", 
                                  habitat_end == "Drie Doring" ~ "Drie doring",
                                  habitat_end == "Grassland" ~ "Grassland", 
                                  habitat_end == "Red Sand" ~ "Red sand")) %>% 
   group_by(habitat_end) %>% 
   summarise(availability = n()) %>% 
   rename(habitat = habitat_end)
 fixef_habitat_dry <- left_join(mutate(fixef_habitat_dry, habitat = as.character(habitat)), 
                                dryseason_availability)
 
 # Compute the probability of use: 
 fixef_habitat_dry <- fixef_habitat_dry %>% 
   mutate(weighted_intensity = fe_abs_intensity * availability,  # Multiply selection intensity by availability
   total_weighted_intensity = sum(weighted_intensity),             # Compute total sum
   probability_of_use = weighted_intensity / total_weighted_intensity)  # Normalize to sum to 1

# Repeat for the wet season
 # first we get the absolute intensity of use in each season
 fixef_habitat_wet <- mutate(fixef_habitat_wet, fe_abs_intensity = exp(fe_abs_logintensity))
 
 # then we get the per-habitat availability
 wetseason_availability <- alltracks_wet %>% 
   filter(case_ == 0) %>% 
   group_by(habitat_end) %>% 
   summarise(availability = n()) %>% 
   rename(habitat = habitat_end)
 fixef_habitat_wet <- left_join(mutate(fixef_habitat_wet, habitat = as.character(habitat)),
                                       dryseason_availability)
 
 # Compute the probability of use: 
 fixef_habitat_wet <- fixef_habitat_wet %>% 
   mutate(weighted_intensity = fe_abs_intensity * availability,  # Multiply selection intensity by availability
          total_weighted_intensity = sum(weighted_intensity),             # Compute total sum
          probability_of_use = weighted_intensity / total_weighted_intensity)  # Normalize to sum to 1

 # Ideally would want confidence intervals around this but I think it's fine as is for now as a descriptive
 bind_rows(select(fixef_habitat_dry, habitat, probability_of_use) %>% 
   mutate(season = "Dry"), select(fixef_habitat_wet, habitat, probability_of_use) %>% 
     mutate(season = "Wet")) %>% 
   mutate(probability_of_use = round(probability_of_use*100, 1)) %>% 
   pivot_wider(id_cols = habitat, names_from = season, values_from = probability_of_use) %>% 
   data.frame()

#---------------------------------------------------------
# Collate the SSA plots 
#--------------------------------------------------------

# Population-level effects (5.25in x 8in)
p_final <- p1/p2
#ggsave("FigureS3.pdf", p_final, device="pdf", dpi = 450, units="in", height = 6.25, width = 5.5)
#ggsave("FigureS3.png", p_final, device="png", dpi = 450, units="in", height = 6.25, width = 5.5)

###################### END OF SCRIPT ###################