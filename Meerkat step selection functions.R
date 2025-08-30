################################################
#  STEP SELECTION FUNCTIONS TO EXPLORE HABITAT USE IN THE MEERKATS AT THE MOVEMENT SCALE
#
###############################################

lapply(c("adehabitatLT", "amt", "elevatr", "emmeans", "geosphere", "ggplot2", "job", "lubridate", "patchwork", "sf", "terra", "tidyverse", "broom.mixed"), FUN = library, character.only = TRUE)

# Load in everything I need: 

# MOVEMENT/GPS DATA -----------------------------------------------------------------

# Of more relevance, load in the tracks of interest
# "Added the movement paths csv to the group folder as Tracks_Morn_Filt. It's about 162,000 GPS points which is 13,000 sessions. It's filtered to morning sessions where every GPS point was taken 10-20 minutes apart, and there are at least 8 points in the session (usually 12-13). The HabType variable describes what Habitat type that specific GPS point is in -- currently its still split into 4 habitat types (DrieDoring, WhiteSandMix, RedSandMix, and Grassland) so you may want to clump those together into 3 categories instead. PixelID describes which Pixel that GPS point is in. Distance_lead describes the distance (meters) from this GPS point to the next one in the session, and TimeDiff_lead describes the time (minutes) between this GPS point and the next one in the session -- so both of these values will be NA for the last point in the session. Speed is just Distance_lead/TimeDiff_lead.  nPoints is the number of GPS points in the session"

#tracks <- read.csv("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Intergroup Differences/Habitat selection/Tracks_Morn_Filt.csv", 
#                  header = TRUE) %>% 
#  mutate(Date = ymd(Date), 
#         Month = month(Date),
#         Year = year(Date),
#         Time = ymd_hms(Time), 
#         BurrowLeaveTime = ymd_hms(BurrowLeaveTime)) %>% 
#  group_by(SessionID) %>% 
#   mutate(TimeForaging = as.numeric(difftime(Time, first(Time), units = "mins")), # Time spent foraging
#         breeding_season = case_when(Date >= as.Date(paste0(Year,"-10-01")) ~ paste0(Year,"/",Year+1),
#                                   Date < as.Date(paste0(Year,"-10-01")) ~ paste0(Year-1,"/",Year)),
#         season = if_else(month(Date)%in%c(10,11,12,1,2,3,4), "Oct-Apr", "May-Sep")) %>% # breeding season 
#  data.frame()

  # Remove those > 4 hours n = 38
#  tracks <- tracks %>% 
#    group_by(SessionID) %>% 
#    mutate(TotalTimeForaging = max(TimeForaging, na.rm = TRUE)/60) %>% 
#    filter(TotalTimeForaging <= 4) %>% 
 ##   data.frame()

  # Look at the track data before interpolation
#  length(unique(tracks$SessionID)) # for main text need this after interpolating
#  length(unique(tracks$GroupRef))  # ditto 
  
#  # What is the distribution of step lengths before interpolation
#  ggplot(tracks, aes(x = TimeDiff_lead)) + 
#    geom_histogram(binwidth = 1, fill = "blue", colour = "black") + 
#    labs(title = "Histogram of Time Foraging",
#         x = "Time Steps (minutes)",
#         y = "Frequency") +
#    theme_minimal()
#  mean(tracks$TimeDiff_lead, na.rm = TRUE)
#  sd(tracks$TimeDiff_lead, na.rm = TRUE)
  
  # Number of foraging steps
#  distinct(tracks, SessionID, nPoints) %>%
#    .$nPoints %>% 
#    table()
  
  # Total time spent foraging
#  tracks %>%
#    distinct(SessionID, TimeForaging) %>%
#    mutate(TimeForaging = TimeForaging / 60) %>%
#    ggplot(aes(x = TimeForaging)) +
#    geom_histogram(binwidth = 1, fill = "blue", colour = "black") +
#    labs(title = "Histogram of Time Foraging",
#         x = "Total Time Foraging (hours)", y = "Frequency") +
#    theme_minimal()
  
# Need to interpolate at 15 minute resolution across all samples
  # First convert to UTM and keep only the necessary information
#  tracks <- tracks %>%
#    st_as_sf(coords = c("GPS_Longitude", "GPS_Latitude"), crs = 4326) %>% # Set CRS as WGS84
#    st_transform(crs = "+proj=utm +zone=34 +south +datum=WGS84") %>% 
#    mutate(Easting = st_coordinates(.)[, 1],  
#           Northing = st_coordinates(.)[, 2])  %>%
#    st_drop_geometry()  %>% 
#    dplyr::select(-HabType, -PixelID, -Distance_lead, -TimeDiff_lead, -Speed) %>% 
    # also provide an additional unique identifier for the track 
#    arrange(Date) %>% 
#    group_by(SessionID) %>% 
#    mutate(id = cur_group_id()) %>% 
#    arrange(id) %>% 
#    data.frame()
  
  # Now set up as ltraj object
#  ltraj <- as.ltraj(xy = tracks[, c("Easting", "Northing")],  
#                    date = tracks$Time,                  
#                    id = as.factor(tracks$GroupRef),
#                    burst = as.factor(tracks$SessionID),
#                    typeII = TRUE)  # Time-explicit
  
   # Interpolate the trajectory at 15-minute intervals
 # job({ ltraj_interp <- redisltraj(ltraj, u = 15*60, type = "time") }) # ~ 10 minutes
  
  # Visualise an interpolated track to feel confident in the method 
  # e.g. of a higher resolution track: 
#  unique(filter(tracks, SessionID == "10 2004-06-29")$id)
#  plot(ltraj[81], perani=FALSE)
#  plot(ltraj_interp[81], perani=FALSE) # doing a good job it seems. 
  
  # Convert the trajectories back to a dataframe
#  tracks_df <- ld(ltraj_interp) 
#  tracks_SSF_ids <- unique()
#  tracks_df <- tracks_df %>% 
#    rename(Easting = x, Northing = y, GroupRef = id, SessionID = burst, Time = date) %>% 
#    dplyr::select(GroupRef, SessionID, Easting, Northing, Time) %>% 
#    left_join(distinct(dplyr::select(tracks, SessionID, GroupName)), by = "SessionID") %>%
#    group_by(SessionID) %>% 
#    mutate(TrackID = cur_group_id()) %>%   # get a unique TrackID
#    ungroup() %>% 
#    arrange(TrackID) %>% 
#    dplyr::select(TrackID, GroupRef, GroupName, SessionID, Easting, Northing, Time) %>% 
#    data.frame()
      
  # Check the final data frame for foraging track points: 
  #head(tracks_df)
  
  # get the relevant information about the tracks used for SSF
  #length(unique(tracks_df$TrackID)) ; length(unique(tracks_df$GroupName))
  
  # Export this data set
  #write.csv(tracks_df, "Tracks_Morn_Filt_15min.csv", row.names = FALSE)
  
  # Clean up
 # rm(ltraj, ltraj_interp, tracks)
  
# LANDCOVER DATA --------------------------------------------------------------

# Now load in the various habitat and spatial information I need
# Load in the landcover map 
#d <- paste0("C://Users//Jack//OneDrive//Documents//Kalahari//Cambridge LARG//Cambridge PostDoc2//Meerkat//Kalahari Habitat Map//habClPanRiver.shp") 
#o <- terra::vect(d) %>% st_as_sf()
#o <- st_set_crs(o, "+proj=utm +zone=34 +south +datum=WGS84")
#FH <- data.frame(longitude = 21.83264, latitude = -26.97850)

# based on the % of the different habitats merge some categories
#o <- o %>%
#  mutate(Class_Name = case_when(Class_Name == 'DD' ~ 'Drie Doring',
#                                Class_Name == 'grassland' ~ 'Grassland',
#                                Class_Name == 'RS mix' ~ 'Red Sand', 
#                                Class_Name %in% c('WS mix', "pan", "Riverbed") ~ 'White Sand'))

# To get the function to work I need to convert the veg cover map into a spatial raster
# For this is makes sense to to check the resolution of the features in the polygon data
#st_bbox(o)  # Get the bounding box
#min_feature_size <- sqrt(min(st_area(o)))  # Minimum feature size is small 
# so 25 metre resolution seems fine

# Convert sf to a SpatVector
#vect_o <- vect(o)
#rast_o <- rast(ext(vect_o), resolution = 25)
#raster_o <- rasterize(vect_o, rast_o, field = "Class_Name")
#plot(raster_o, colNA = "red")
#raster_o <- mask(raster_o, mask = !is.na(raster_o))
#plot(raster_o)

# Estimate an edge of some sort and see how it looks
# First re-categorise as 2 things 
#o2 <- o %>%
#  mutate(Class_Name = case_when(Class_Name == 'Drie Doring' ~ 'White Sand',
#                                Class_Name == 'Grassland' ~ 'Red Sand',
#                                TRUE ~ Class_Name)) %>% 
#  vect() 
#rast_o2 <- rast(ext(o2), resolution = 25)
#raster2 <- rasterize(o2, rast_o2, field = "Class_Name")
#plot(raster2)
#raster2 <- mask(raster2, mask = !is.na(raster2))
#plot(raster2)

#edge_map <- boundaries(raster2, inner = TRUE, directions = 4, classes = TRUE)
#plot(edge_map, main = "Edge Map Using boundaries()")

# Compare the queen's case others
#edge_map2 <- boundaries(raster2, inner = TRUE, directions = 8, classes = TRUE)

# Get the distance from the boundary in this fashion
# And compute the distance from each pixel to the nearest boundary
#boundary_raster <- ifel(edge_map == 1, 1, NA)
#distance_map <- distance(boundary_raster)
#distance_map <- mask(distance_map, edge_map)
#plot(distance_map, main = "Distance to Boundary Pixels (Value = 1)")

# Or to do it in such as way as to get a  use a similar method to Bateman

# Lastly, it would be nice to also get a Digital elevation map for the Kalahari 
#terra::crs(raster2) <- "+proj=utm +zone=34 +south +datum=WGS84"
#dem_krc <- get_elev_raster(locations = raster2, z = 14, prj = "+proj=utm +zone=34 +south +datum=WGS84")
#plot(dem_krc)
# Resample the DEM to match the resolution and extent of raster2
#dem_krc <- rast(dem_krc)
#dem_krc <- resample(dem_krc, raster2, method = "bilinear")
#compareGeom(dem_krc, raster2)
#plot(dem_krc, main = "DEM")

# Compute slope in degrees
#slope_krc <- terrain(dem_krc, v = "slope", unit = "degrees")
#plot(slope_krc, main = "Slope (Degrees)")

# Export all the rasters
#saveRDS(raster_o, "HabitatRaster.rds")  # Habitat 
#saveRDS(edge_map, "HabitatEdgeRaster.rds") # Edge habitat
#saveRDS(edge_map2, "HabitatEdgeRaster2.rds") # Edge habitat 2
#saveRDS(distance_map, "HabitatEdgeDist.rds") # Distance to edge
#saveRDS(dem_krc, "ElevationRaster.rds") # DEM
#saveRDS(slope_krc, "SlopeRaster.rds") # Slope

########################################

### STEP SELECTION FUNCTION WITH MEERKATS (AT THE LEVEL OF THE FORAGING TRACK)

#########################################

setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Intergroup Differences/Habitat selection/")

# Working with tracks_df 
tracks_df <- read.csv("Tracks_Morn_Filt_15min.csv", header = TRUE) %>% 
  mutate(time = ymd_hms(time),  
         date = as.Date(time))
#write.csv(tracks_df, "Tracks_Morn_Filt_15min.csv", row.names = FALSE)
 
# And the following three rasters 
raster_o <- readRDS("HabitatRaster.rds")  # Habitat 
dem_krc <- readRDS("ElevationRaster.rds")  # Elevation
slope_krc <- readRDS("SlopeRaster.rds")  # Slope 
edge_map <- readRDS("HabitatEdgeRaster.rds") # Edge habitat
edge_map2 <- readRDS("HabitatEdgeRaster2.rds")  # Edge habitat more coarse
distance_map <- readRDS ("HabitatEdgeDist.rds") # distance from edge habitat

# get the relevant information about the tracks used for SSF
length(unique(tracks_df$TrackID)) ; length(unique(tracks_df$GroupName))

#----------------------------------------

# STEP SELECTION FUNCTION 

#----------------------------------------

# Generate "track" types using the amt package 
 # given the data structure, we should expect ~ 130,000 steps

 #alltracks <- tracks_df %>% 
 #   mutate(TrackID = as.numeric(TrackID)) %>% 
 #   arrange(time) %>% 
 #   make_track(.x = Easting, .y = Northing, .t = time, burst_ = TrackID, 
 #              SessionID = SessionID, GroupRef = GroupRef, GroupName = GroupName, 
 #              month = month, period = period, crs = crs("+proj=utm +zone=34 +south +datum=WGS84")) 
 
job({ alltracks <- tracks_df %>% 
   mutate(TrackID = as.numeric(TrackID), 
          TrackID2 = TrackID) %>% 
   arrange(time) %>% 
   nest(-TrackID) %>% 
  # filter(TrackID %in% 1:1000) %>% 
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
alltracks_df <- random_steps(alltracks_burst, n_control = 19) #--> 2401120 rows. # a lot of rows...

# Extract habitat variables
alltracks_df <- alltracks_df %>% 
  extract_covariates(raster_o, where = "both") %>% 
  rename(habitat_start = Class_Name_start, 
         habitat_end = Class_Name_end) %>% 
  extract_covariates(distance_map, where = "both") %>% 
  rename(edgedist_start = Class_Name_start, 
         edgedist_end = Class_Name_end) %>% 
  # I won't be analysing step lengths within this framework
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
 NA_steps <- unique(alltracks_df$Stratum[is.na(alltracks_df$habitat_end)]) # n = 26 steps
 alltracks_df <- filter(alltracks_df, !(Stratum %in% NA_steps)) 
 #alltracks_df %>% filter(if_any(everything(), is.na))
  
 # and where the turning angle is NA. 
 NA_steps2 <- unique(alltracks_df$Stratum[is.na(alltracks_df$cos_ta)]) # n = 19
 alltracks_df <- filter(alltracks_df, !(Stratum %in% NA_steps2)) 
 #saveRDS(alltracks_df, "SSFModelData.rds")
 
# Now for the track characteristics, we want the information before resampling but on those groups where the steps provide the info
 # Average track duration  
 tracks %>% 
   filter(SessionID %in% unique(alltracks_df$SessionID)) %>% 
   arrange(SessionID , Time) %>% 
   group_by(SessionID ) %>% 
   mutate(TimeDiff = as.numeric(difftime(max(Time), min(Time), units = "mins"))) %>% 
   ungroup() %>% 
   dplyr::select(SessionID, TimeDiff) %>% 
   distinct() %>% 
   summarise(mean(TimeDiff), sd(TimeDiff), range(TimeDiff)) %>% 
   data.frame()
 
 # Interval/step characteristics
 tracks %>% 
   filter(SessionID %in% unique(alltracks_df$SessionID)) %>% 
   arrange(SessionID, Time) %>% 
   group_by(SessionID) %>% 
   mutate(TimeDiff = as.numeric(difftime(lead(Time), Time, units = "mins"))) %>% 
   ungroup() %>% 
   summarise(mean(TimeDiff, na.rm = T), sd(TimeDiff, na.rm = T), range(TimeDiff, na.rm = T)) %>% 
   data.frame()
 
#------------------------------------------
 
# Step selection function (SSF) via glmmTMB
 
#------------------------------------------
 
# load in the bits to save re-doing all the above steps 
lapply(c("ggplot2", "tidyverse", "glmmTMB", "broom.mixed", "emmeans", "patchwork", "job"), FUN = library, character.only = TRUE)
setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Intergroup Differences/Habitat Selection/Step Selection Functions")

alltracks_df <- readRDS("SSFModelData.rds")
#mkat_ssf_mod1 <- readRDS("SSF_mod_dry.rds")
#mkat_ssf_mod2 <- readRDS("SSF_mod_wet.rds")

# get the relevant information about the tracks used for SSF
length(unique(alltracks_df$TrackID)) ; 
length(unique(alltracks_df$GroupName)) ; 
length(unique(alltracks_df$Stratum))

#---------------------------------------------------- 
# Set up a dry-season model (SSF MODEL 1) : May-Oct
#     and a wet-season model (SSF MODEL 2) : Nov-Apr
# ----------------------------------------------------
 
 # In this model, I include all of the data. I don't care about differences in habitat selection or movement parameters according to season.  
alltracks_df$Stratum <- as.factor(alltracks_df$Stratum)
alltracks_df$GroupName <- as.factor(alltracks_df$GroupName)
alltracks_df$habitat_end <- as.factor(alltracks_df$habitat_end)
#alltracks_df$habitat_start <- as.factor(alltracks_df$habitat_start)
#alltracks_df$year <- as.factor(year(alltracks_df$date))
#alltracks_df$breeding_season <- as.factor(alltracks_df$breeding_season)
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
#saveRDS(mkat_ssf_mod1 , "SSF_mod_dry.rds")

# Or use a slightly different specification where you encode the habitat types as dummy variables
#alltracks_df <- mutate(alltracks_df, 
#                       "Drie" = if_else(habitat_end == "Drie Doring", 1, 0), 
#                       "Grassland" = if_else(habitat_end == "Grassland", 1, 0), 
#                       "RedSand" = if_else(habitat_end == "Red Sand", 1, 0), 
#                       "WhiteSand" = if_else(habitat_end == "White Sand", 1, 0))

#mkat_ssf_mod1_config_alt <- glmmTMB(case_ ~ -1 + Drie.Doring + Grassland + RedSand +
#          (0 + Drie.Doring | GroupName) + 
#          (0 + Grassland | GroupName) + 
#          (0 + RedSand | GroupName) + 
#          (1 | Stratum),
#        REML = TRUE, 
#        family = poisson, 
#        data = alltracks_dry, 
#        doFit = FALSE, 
#        map = list(theta=factor(c(1:3, NA))),
#        start = list(theta=c(0,0,0,log(1e3))))

# wet-season model
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
#saveRDS(mkat_ssf_mod2, "SSF_mod_wet.rds")

#------------------------------------------------------------------
# Plot and interpret habitat-selection parameters for each model #
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
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]}))) +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text = element_text(colour = "black", size = 9.5), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) +   scale_y_continuous(breaks = seq(0.5, 1.5, 0.1), 
                                                                                                                labels = c("", 0.6, "", 0.8,  0.9, 1, 1.1, 1.2, 1.3, 1.4, ""))
p1

# An alternative plot 
p1_alt <- ggplot(filter(fixef_habitat, habitat != "Drie doring"), aes(x = season, y = exp(fe_estimate))) + 
  geom_hline(yintercept = 1, linetype = 2, colour = "forestgreen") +
  geom_errorbar(aes(ymin = exp(fe_estimate - 1.96*std.error), 
                    ymax = exp(fe_estimate + 1.96*std.error), 
                    colour = habitat), width = 0, linewidth = 0.8) +
  geom_point(size = 4.5, aes(colour = habitat)) +
  geom_point(size = 4.5, shape = 1, colour = "black") +
  facet_wrap(~habitat) +
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]}))) +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text = element_text(colour = "black", size = 9.5), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) +   scale_y_continuous(breaks = seq(0.5, 1.5, 0.1), 
                                                                                                 labels = c("", 0.6, "", 0.8,  0.9, 1, 1.1, 1.2, 1.3, 1.4, ""))
p1_alt

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
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]}))) +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text = element_text(colour = "black", size = 9.5), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) + 
  scale_y_continuous(breaks = seq(0.5, 1.5, 0.1), 
                     labels = c("", 0.6, "", 0.8,  "", 1, "", 1.2, "",1.4, ""))
p2 

# An alternative plot: 
p2_alt <- ggplot(ranef_habitat) + 
  geom_boxplot(aes(x = season, y = exp(effect)), outliers = FALSE) +
  geom_hline(yintercept = 1, linetype = 2, colour = "forestgreen") +
  geom_point(aes(x = season, y = exp(effect)),
             position = position_jitter(width = 0.2), shape = 1) +
  geom_errorbar(data = filter(fixef_habitat, habitat != "Drie doring"), 
                aes(x = season, ymin = exp(fe_estimate - 1.96*std.error), 
                    ymax = exp(fe_estimate + 1.96*std.error), 
                    colour = habitat), width = 0, linewidth = 0.8, alpha = 0.6) +
  geom_point(data = filter(fixef_habitat, habitat != "Drie doring"), 
             aes(x = season, y = exp(fe_estimate), colour = habitat), size = 4.5, alpha = 0.6) +
  geom_point(data = filter(fixef_habitat, habitat != "Drie doring"), 
             aes(x = season, y = exp(fe_estimate), colour = habitat), size = 4.5, 
             shape = 1, alpha = 0.6, colour = "black") +
  facet_wrap(~habitat) +
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]}))) +
  theme_bw() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text = element_text(colour = "black", size = 9.5), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) + 
  scale_y_continuous(breaks = seq(0.5, 1.5, 0.1), 
                     labels = c("", 0.6, "", 0.8,  "", 1, "", 1.2, "",1.4, ""))
p2_alt

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
# Collate the plots 
#--------------------------------------------------------

# Population-level effects (5.25in x 8in)
p1_final <- p1 + labs(tag = "A")
p2_final <- p2 + labs(tag = "B")
p_final <- p1_final/p2_final
ggsave("SSA_wetdry.pdf", p_final, width = 5.75, height = 5.75, device = "pdf", dpi = 600)
ggsave("SSA_wetdry.png", p_final, width = 5.75, height = 5.75, device = "png", dpi = 600)

#p1_alt_final <- p1_alt + labs(tag = "A")
#p2_alt_final <- p2_alt + labs(tag = "B")
#p_alt_final <- p1_alt_final/p2_alt_final
#ggsave("iSSF_wetdry_alt.pdf", p_alt_final, width = 5.75, height = 5.75, device = "pdf", dpi = 600)
#ggsave("iSSF_wetdry_alt.png", p_alt_final, width = 5.75, height = 5.75, device = "png", dpi = 600)

###################### END OF SCRIPT ###################