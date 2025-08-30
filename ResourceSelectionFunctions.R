# MODELLING RESOURCE SELECTION FOR FORAGING AND BURROW HABITAT #
# 29th May 2025
# I have planned it so that it works with two seasons, wet (Oct-Apr) and dry (May-Sep)

# Create season-specific meshes and models and plot them out
lapply(c("terra", "tidyverse", "emmeans", "ggplot2", "sf", "sdmTMB", "fmesher", "patchwork"), FUN = library, character.only = TRUE)

## Import and format polygon  of habitat types
# Load in the landcover map 
d <- paste0("C:/Users/Jack/Dropbox/MeerkatEnvironmentWork/Territory quality and group size/Kalahari Habitat Map/habClPanRiver.shp") 

o <- terra::vect(d) %>% st_as_sf()
o <- st_set_crs(o, "+proj=utm +zone=34 +south +datum=WGS84")
FH <- data.frame(longitude = 21.83264, latitude = -26.97850)

# Based on the % of the different habitats merge some categories
o <- o %>%
  mutate(Class_Name = case_when( Class_Name == 'DD' ~ 'Drie Doring',
    Class_Name == 'grassland' ~ 'Grassland',
    Class_Name == 'RS mix' ~ 'Red Sand', 
    Class_Name %in% c('WS mix', "pan", "Riverbed") ~ 'White Sand'))

# Convert the polygon coords to km scale
o_km <- o %>% mutate(geometry=geometry/1000)
o_km <- st_set_crs(o_km, 32734)

# Read in the foraging tracks (switch based on user)
morningtracks <- read_csv("C:/Users/Jack/Dropbox/MeerkatEnvironmentWork/Territory quality and group size/Tracks_Morn_Filt.csv")

# Add on covariate data to the foraging tracks
modDF <-select(morningtracks, SessionID, GroupRef, Date, Time, HabType, GPS_Longitude, GPS_Latitude)

# Convert gps points to LAT/LONG
modDF <- add_utm_columns(modDF,c("GPS_Longitude","GPS_Latitude"),units="km")

# Set up new seasonal breakdown
modDF <- modDF %>%
  mutate(month = month(Date),
         year = year(Date),
         GroupRef = GroupRef) %>%
  mutate(breedingyear = case_when(Date >= as.Date(paste0(year,"-10-01")) ~ paste0(year,"/",year+1),
                                  Date < as.Date(paste0(year,"-10-01")) ~ paste0(year-1,"/",year)),
         season = if_else(month(Date)%in%c(10,11,12,1,2,3,4), "Oct-Apr", "May-Sep")) %>%
  filter(year < 2024) %>% # we have no mass data for 2024
  mutate(year_f = as.factor(year),
         month_f = as.factor(month),
         SessionID = as.factor(SessionID))

# Assign within session ID to track points
modDF <- modDF %>% 
  ungroup() %>% 
  group_by(SessionID) %>% arrange(Time) %>% 
  mutate(PointOrder = row_number(),
         ForTime = difftime(Time,min(Time), units="mins"))

# Edit the labels slightly
modDF <- mutate(modDF, HabType = case_when(HabType == "DrieDoring" ~ "Drie doring", 
                                           HabType == "Grassland" ~ "Grassland", 
                                           HabType == "RedSandMix" ~ "Red sand", 
                                           HabType == "WhiteSandMix" ~ "White sand"))


modDF$season[modDF$season == "May-Sep"] <- "Dry (May-Sep)"
modDF$season[modDF$season == "Oct-Apr"] <- "Wet (Oct-Apr)"

p <- ggplot(filter(modDF, ForTime <= 180)) +
  geom_histogram(aes(x=ForTime, fill = HabType), position="fill", 
                 binwidth = 15, boundary = 0) +
  geom_hline(yintercept = 0.5,lty=2) +
  geom_hline(yintercept = 0.25,lty=2) +
  geom_hline(yintercept = 0.75,lty=2) +
  facet_wrap(~season) +
  coord_cartesian(expand = F) +
  scale_x_continuous(breaks = seq(0, 180, 30)) + 
  scale_fill_manual(values = c("forestgreen", "chocolate", "sandybrown", "burlywood")) + 
  theme_bw() + 
  theme(legend.title = element_blank(), 
        axis.title = element_text(size = 11),
        axis.text = element_text(colour = "black", size = 9.5), 
        panel.grid = element_blank(), 
        strip.text = element_text(size = 10)) + 
  labs(x = "Foraging time (minutes)", 
       y = "Proportion")

# Extract the max point of all bars for Grassland, Red Sand, and White Sand
histlines <- ggplot_build(p)$data[[1]] %>% 
  filter(fill != "forestgreen") %>% 
  select(fill, PANEL, xmin, xmax, ymax) %>% 
  rowwise() %>%  # Ensure operations are row-specific
  mutate(x = list(seq(xmin, xmax, by = 1))) %>%  # Create sequence from xmin to xmax
  unnest(x) %>%  # Expand rows for each x value
  select(fill, PANEL, x, ymax) %>% 
  mutate(season = case_when(PANEL == 1 ~ "Dry (May-Sep)",
                            PANEL == 2 ~ "Wet (Oct-Apr)")) %>%  
  data.frame()

p + 
  geom_path(data = histlines, aes(group = fill, x = x, y = ymax), linewidth = 0.5)

# Were they babysitting on a given morning/day?
#babysitting_dates <- read.csv("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Home Ranges and Territory Overlap/Babysitting_GroupDates.csv", header = TRUE) %>% 
#  mutate(Date = dmy(Date), Babysitting = "Yes")

#modDF <- left_join(modDF , babysitting_dates, by = c("GroupRef", "Date")) %>% 
#  mutate(Babysitting = if_else(is.na(Babysitting), "N", "Y")) %>% 
#  data.frame()

# what times of year was babysitting
#modDF  %>% 
#  distinct(Date, month, GroupRef, Babysitting) %>% 
#  group_by(month, Babysitting) %>% 
#  summarise(n = n()) %>% 
#  data.frame()

#modDF  %>% 
#  distinct(Date, season, GroupRef, Babysitting) %>% 
#  group_by(season, Babysitting) %>% 
#  summarise(n = n()) %>% 
#  data.frame()

# Rework the plot
#p2 <- ggplot(filter(modDF, ForTime <= 180)) +
#  geom_histogram(aes(x=ForTime, fill = HabType), position="fill", 
#                 binwidth = 15, boundary = 0) +
#  geom_hline(yintercept = 0.5,lty=2) +
#  geom_hline(yintercept = 0.25,lty=2) +
#  geom_hline(yintercept = 0.75,lty=2) +
#  facet_grid(Babysitting~season) +
#  coord_cartesian(expand = F) +
#  scale_x_continuous(breaks = seq(0, 180, 30)) + 
#  scale_fill_manual(values = c("forestgreen", "chocolate", "sandybrown", "burlywood")) + 
#  theme_bw() + 
#  theme(legend.title = element_blank(), 
#        axis.title = element_text(size = 11),
#        axis.text = element_text(colour = "black", size = 9.5), 
#        panel.grid = element_blank(), 
#        strip.text = element_text(size = 10)) + 
#  labs(x = "Foraging time (minutes)", 
#       y = "Proportion")
#p2 # Little difference so I will not segregate the analysis by breeding season

#--------------------------
# Sub sample each session to a randomly chosen data point
set.seed(1066)
PerSession_DF <- modDF %>% 
  group_by(SessionID, GroupRef, Date) %>%
  slice_sample(n=1)

# Sub Sample Burrow and Foraging Period points
  # Burrow sample, assuming the first point on the track is the burrow, or nearby
  BurrowDF <- modDF %>% filter(PointOrder==1)    
    
# For the foraging track we sample between 1 & 4 hours from the first burrow point
  # this should allow a buffer large enough to ensure we are sampling points where the group has settled down to forage
  ForageDF <- modDF %>% 
    filter(ForTime >= 60 & ForTime <= 240) %>% 
    group_by(SessionID, GroupRef, Date) %>%
    slice_sample(n=1)  
  
  ggplot(BurrowDF,aes(x = HabType, fill = HabType)) +
    geom_bar(stat = "count") 
  ggplot(ForageDF, aes(x = HabType, fill = HabType)) +
        geom_bar(stat="count")
      
# Model data prep      
  GrpSeason_Summary <- PerSession_DF %>% 
    group_by(GroupRef, season) %>%
    summarise(n=n()) %>%
    filter(n >= 20)

# Presence points
  PresentPoints <- PerSession_DF %>% 
    inner_join(GrpSeason_Summary) %>% 
    st_as_sf(coords=c("X","Y"), crs = 32734) %>%
    select(GroupRef, season, x=geometry) %>%
    mutate(Present=1)

# Burrow presences 
  PresentPoints_Burrow <- BurrowDF %>%
    inner_join(GrpSeason_Summary) %>%
    st_as_sf(coords=c("X","Y"),crs = 32734) %>%
    select(GroupRef, season, x=geometry) %>%
    mutate(Present=1)
  
# Foraging presences
  PresentPoints_Forage <- ForageDF %>% 
    inner_join(GrpSeason_Summary) %>% 
    st_as_sf(coords=c("X","Y"),crs = 32734) %>%
    select(GroupRef, season,x=geometry) %>%
    mutate(Present=1)
  
# Pseudo-absences (available locations)
GroupSeason_Coord_List <- vector("list",nrow(GrpSeason_Summary))
GroupSeason_Boundaries_list <- vector("list",nrow(GrpSeason_Summary))

for(i in 1:nrow(GrpSeason_Summary)){
  print(i)
  GrpRef <- GrpSeason_Summary[i,]$GroupRef
  #BreedingYear <-  GrpSeason_Summary[i,]$breedingyear
  Season <- GrpSeason_Summary[i,]$season
  N <- GrpSeason_Summary[i,]$n
  
  BoundPoints <- st_as_sf(PerSession_DF %>% filter(GroupRef==GrpRef) ,
                          coords=c("X","Y"),crs = 32734)
  
  GroupSeason_Boundaries_list[[i]] <- fm_nonconvex_hull(BoundPoints, 
                                                        convex = 0.5, 
                                                        concave = -0.5) %>% 
    st_as_sf() %>% 
    mutate(GroupRef=GrpRef, season=Season)
  
  GroupSeason_Coord_List[[i]] <- st_sample(GroupSeason_Boundaries_list[[i]], size=N*19) %>%
    st_as_sf() %>% mutate(GroupRef=GrpRef,
                          #breedingyear = BreedingYear,
                          season=Season) 
  }

# convert from list to data.frames
GroupSeason_Boundaries <- do.call(rbind, GroupSeason_Boundaries_list)
GroupSeasonCoord_Absence <- do.call(rbind, GroupSeason_Coord_List) %>% 
  mutate(Present=0)

#####################################################
# Fit the foraging data resource selection function #
#####################################################

pointMerge_Forage <- rbind(GroupSeasonCoord_Absence, PresentPoints_Forage)

## Assign habitat values to each coordinate  
# Convert sf_polygon to a SpatVector
vect_o <- terra::vect(o_km) ## Convert from sf to vector
rast_o <- terra::rast(terra::ext(vect_o), resolution = 0.025) ## generate raster
raster_o <- terra::rasterize(vect_o, rast_o, field = "Class_Name") ## carry over values associated with vector polygon

points_sf <- terra::vect(pointMerge_Forage)      # Convert coordinates to SpatVector

# Extract raster values at point locations
extracted_values <- terra::extract(raster_o, points_sf)

## assign values to coordinates
points_sf <- cbind(points_sf, extracted_values)

# convert back to a sf object and flag NA hab types
pointMerge_Forage_HAB <- st_as_sf(points_sf) %>% 
  mutate(HabType=ifelse(is.na(Class_Name)==T, "OutOfBounds", as.character(Class_Name)))

table(pointMerge_Forage_HAB$HabType[pointMerge_Forage_HAB$Present==1])
table(pointMerge_Forage_HAB$HabType[pointMerge_Forage_HAB$Present==0])

# create mesh for each
  # produce boundary with more flexible perimeter using convex/concave functions
  # for foraging when pups at burrow
  Boundary <- fm_nonconvex_hull(pointMerge_Forage_HAB %>% 
                                     filter(HabType!="OutOfBounds"), 
                                 convex = 0.1, 
                                 concave = -0.1)
  plot(Boundary)
  
# Generate mesh using inla based on the boundary  
inla_mesh <- fm_mesh_2d(loc = pointMerge_Forage_HAB %>% 
                          filter(HabType!="OutOfBounds"), # coordinates
  max.edge = c(0.6, 1), # max triangle edge length; inner and outer meshes
  offset = c(0.7,2),  # inner and outer border widths
  cutoff = 0.4, # minimum triangle edge length
  boundary = Boundary)
plot(inla_mesh)

# Generate XY coords for sdmTMB for each data subset
modDF_Forage <- data.frame(pointMerge_Forage_HAB) %>% 
  mutate(X = st_coordinates(geometry)[, 1], # Extract x-coordinates
         Y = st_coordinates(geometry)[, 2]) %>%   # Extract y-coordinates 
  filter(HabType!="OutOfBounds")

# generate zero weight values
modDF_Forage <- modDF_Forage %>% 
  mutate(wt= case_when(Present == 1 ~ 1e-6,
                       Present == 0 ~ as.numeric((st_area(Boundary))/nrow(modDF_Forage %>% filter(Present==0)))))

# Ensure categorical random effects for each model
modDF_Forage <- mutate(modDF_Forage, 
                       GroupRef = as.factor(GroupRef),
                       HabType = as.factor(HabType))

#######################################
## Model Foraging habitat selection  ##
#######################################

mesh_FOR <- make_mesh(modDF_Forage, c("X","Y"), mesh = inla_mesh)
#plot(mesh_FOR)

# Base model
ForageSDM_base <- sdmTMB(Present/wt ~ HabType*season,
                    data = modDF_Forage,
                    family=poisson(link="log"),
                    mesh=mesh_FOR,
                    weights=modDF_Forage$wt,
                    spatial="off")
summary(ForageSDM_base)
plot(emmeans(ForageSDM_base, pairwise ~ HabType, by="season"))
  
# Random Effects
ForageSDM_RF <- sdmTMB(Present/wt ~ HabType*season + (1|GroupRef),
                    data = modDF_Forage,
                    family=poisson(link="log"),
                    mesh=mesh_FOR,
                    weights=modDF_Forage$wt,
                    spatial="off")
summary(ForageSDM_RF)
plot(emmeans(ForageSDM_RF, pairwise ~ HabType, by="season"))
  
# Random effects + Spatial Variation
ForageSDM_spatio <- sdmTMB(Present/wt ~ HabType*season + (1|GroupRef),
                        data = modDF_Forage,
                        family=poisson(link="log"),
                        mesh=mesh_FOR,
                        weights =modDF_Forage$wt,
                        spatial="on")
summary(ForageSDM_spatio)
plot(emmeans(ForageSDM_spatio,~ HabType + season))
ForContrast <- emmeans(ForageSDM_spatio, pairwise ~ HabType, by="season")
#AIC(ForageSDM_base, ForageSDM_RF, ForageSDM_spatio)

# extract contrasts for the different hab types using emmeans
Contrasts_Forage <- data.frame(ForContrast$contrasts) %>%
            mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
                   Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
          mutate(season=factor(season,levels=c("Wet (Oct-Apr)", "Dry (May-Sep)")),
                 Group2 = case_when(Group2 == "Red Sand" ~ "Red sand", 
                                    Group2 == "White Sand" ~ "White sand", 
                                    TRUE ~ Group2)) %>%
  mutate(estimate = estimate*-1)

# Plot the relative intensity of use
plot1 <- ggplot(Contrasts_Forage %>% filter(Group1 == "Drie Doring"),
       aes(x=Group2,y=exp(estimate))) +
  geom_hline(yintercept = 1, linetype = 2, colour = "forestgreen",lwd=1) +
  geom_errorbar(aes(ymin = exp(estimate - 1.96*SE), 
                    ymax = exp(estimate + 1.96*SE), 
                    colour = Group2), width = 0, linewidth = 0.8) +
  geom_point(size = 3.5, aes(fill = Group2),shape=21,stroke=0.5) +
  #geom_point(size = 3.5, aes(colour = Group2), shape=4,stroke= 2) +
  facet_wrap(~season, ncol=2) +
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]})), 
       tag = "D",
       subtitle = "Landscape-scale: foraging area selection") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(colour = "black", size = 9), 
        axis.text.y = element_text(colour = "black", size = 9.5),
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) +
  scale_fill_manual(values = c("chocolate", "sandybrown", "burlywood"))
  
    ### Conditional Binomial Model (Muff et al., 2020)
    
      #library(glmmTMB)
      ## apply a weight of 1000 to the pseudoabsences
      #modDF_Forage_BS$weight <- 1000^(1-modDF_Forage_BS$Present)
      
      ## fit base model without accounting for spatial autocorrelation
      #Forage_CondMod <- glmmTMB(Present ~ HabType*season + (1|GroupRef),
      #                      data = modDF_Forage_BS,
      #                      family="binomial",
      #                      weights = weight)
      #summary(Forage_CondMod)
      #plot(emmeans(Forage_CondMod,pairwise ~ HabType,by="breedingquarters"))
      
      # Run conditional logistic regression with spatial autocorrelation accounted for
      #Forage_CondMod_spatio <- sdmTMB(Present ~ HabType*season + (1|GroupRef),
      #                           data = modDF_Forage_BS,
      #                           family=binomial(link = "logit"),
      #                           mesh=mesh_FOR_BS,
      #                           weights=modDF_Forage_BS$weight,
      #                           spatial="on")
      
      ## plot of effects
      #summary(Forage_CondMod_spatio)
      #plot(emmeans(Forage_CondMod_spatio,pairwise ~ HabType,by="breedingquarters"))
    
# Extract the probability of use
forage_means <- data.frame(emmeans(ForageSDM_spatio, ~ HabType + season)) %>% 
  mutate(selection_intensity = exp(emmean)) %>% 
  select(HabType, season, selection_intensity)
  
forage_availability <- filter(modDF_Forage, Present == 0) %>% 
  group_by(HabType, season) %>% 
  summarise(availability = n())

forage_means <- left_join(forage_means, forage_availability) %>% 
  group_by(season) %>% 
  # calculate probability of use 
  mutate(weighted_intensity = selection_intensity * availability,  # Multiply selection intensity by availability
         total_weighted_intensity = sum(weighted_intensity),             # Compute total sum
         probability_of_use = (weighted_intensity / total_weighted_intensity)*100) %>%  # Normalize to sum to 1
  data.frame()

#-------------------------------------------------------------------------------
# Fit the burrow site resource selection function 
#-------------------------------------------------------------------------------

pointMerge_Burrow <- rbind(GroupSeasonCoord_Absence,PresentPoints_Burrow)

## Assign habitat values to each coordinate  
# Convert sf_polygon to a SpatVector
vect_o <- terra::vect(o_km) ## Convert from sf to vector
rast_o <- terra::rast(terra::ext(vect_o), resolution = 0.025) ## generate raster
raster_o <- terra::rasterize(vect_o, rast_o, field = "Class_Name") ## carry over values associated with vector polygon

points_sf <- terra::vect(pointMerge_Burrow) # Convert coordinates to SpatVector

# Extract raster values at point locations
extracted_values <- terra::extract(raster_o, points_sf)

## assign values to coordinates
points_sf <- cbind(points_sf, extracted_values)

## convert back to a sf object and flag NA hab types
pointMerge_Burrow_HAB <- st_as_sf(points_sf) %>% 
  mutate(HabType=ifelse(is.na(Class_Name)==T,"OutOfBounds",as.character(Class_Name)))

# create mesh
modDF_Burrow <- data.frame(pointMerge_Burrow_HAB) %>% 
  mutate(X = st_coordinates(geometry)[, 1], # Extract x-coordinates
         Y = st_coordinates(geometry)[, 2]) %>%  # Extract y-coordinates
  filter(HabType!="OutOfBounds")

Boundary2 <- fm_nonconvex_hull(pointMerge_Burrow_HAB %>% 
                                 filter(HabType!="OutOfBounds"), 
                               convex = 0.1, 
                               concave = -0.1)
plot(Boundary2)

inla_mesh2 <- fm_mesh_2d(
  loc = pointMerge_Burrow_HAB %>% filter(HabType!="OutOfBounds"), # coordinates
  max.edge = c(0.6, 1), # max triangle edge length; inner and outer meshes
  offset = c(0.7,2),  # inner and outer border widths
  cutoff = 0.4, # minimum triangle edge length
  boundary=Boundary2)
plot(inla_mesh2)

# generate zero weight values
modDF_Burrow <- modDF_Burrow %>% mutate(wt=case_when(
  Present==1 ~ 1e-6,
  Present==0 ~ as.numeric((st_area(Boundary2))/nrow(modDF_Burrow %>% filter(Present==0)))))

# Convert to sdmTMB mesh 
modDF_Burrow <- mutate(modDF_Burrow, 
                       GroupRef = as.factor(GroupRef), 
                       HabType = as.factor(HabType))

mesh_Burrow <- make_mesh(modDF_Burrow, c("X","Y"), mesh = inla_mesh2)

# Fit the models
# Burrow SDM just random-effects
BurrowSDM_RF <- sdmTMB(Present/wt ~ HabType * season + (1|GroupRef),
                    data = modDF_Burrow,
                    family = poisson(link="log"),
                    mesh = mesh_Burrow,
                    weights = modDF_Burrow$wt,
                    spatial="off")

  # summarise model outputs
  summary(BurrowSDM_RF)
  plot(emmeans(BurrowSDM_RF,pairwise~HabType, by="season"))

# Burrow SDM with spatial term 
BurrowSDM_spatio <- sdmTMB(Present/wt ~ HabType * season + (1|GroupRef),
              data= modDF_Burrow,
              family=poisson(link="log"),
              mesh=mesh_Burrow,
              weights= modDF_Burrow$wt,
              spatial="on")
  # summarise model outputs
  summary(BurrowSDM_spatio)
  plot(emmeans(BurrowSDM_spatio,pairwise~HabType, by="season"))
  
# Extract pairwise contrast from the model using emmeans
BurrowContrast <- emmeans(BurrowSDM_spatio, pairwise ~ HabType, by="season")

# get metadata to allow specific contrasts to be subset
Contrasts_Burrow <- data.frame(BurrowContrast$contrasts) %>%
  mutate(Group1=trimws(str_split_fixed(contrast,"-",n=2))[,1],
         Group2=trimws(str_split_fixed(contrast,"-",n=2))[,2]) %>%
  mutate(season = factor(season, levels=c("Wet (Oct-Apr)", "Dry (May-Sep)")), 
         Group2 = case_when(Group2 == "Red Sand" ~ "Red sand", 
                            Group2 == "White Sand" ~ "White sand", 
                            TRUE ~ Group2))   %>% 
  mutate(estimate = estimate*-1)

plot2 <- ggplot(Contrasts_Burrow %>% filter(Group1=="Drie Doring"),
       aes(x=Group2,y=exp(estimate))) +
  geom_hline(yintercept = 1, linetype = 2, colour = "forestgreen",lwd=1) +
  geom_errorbar(aes(ymin = exp(estimate - 1.96*SE), 
                    ymax = exp(estimate + 1.96*SE), 
                    colour = Group2), width = 0, linewidth = 0.8) +
  geom_point(size = 3.5, aes(fill = Group2), shape=21,stroke= 0.5) +
  facet_wrap(~season, ncol=2) +
  labs(y = expression("Selection intensity " ~ (italic(e)^{italic(beta)[hab]})), 
       tag = "C",
       subtitle = "Landscape-scale: burrow habitat selection") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(colour = "black", size = 9), 
        axis.text.y = element_text(colour = "black", size = 9.5),
        panel.grid = element_blank(), 
        strip.text = element_text(size = 11)) +
  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood")) +
  scale_fill_manual(values = c("chocolate", "sandybrown", "burlywood"))

plot2 / plot1

# Extract the probability of use for burrowing habitat 
burrow_means <- data.frame(emmeans(BurrowSDM_spatio,~ HabType + season)) %>% 
  mutate(selection_intensity = exp(emmean)) %>% 
  select(HabType, season, selection_intensity)

burrow_availability <- filter(modDF_Burrow, Present == 0) %>% 
  group_by(HabType, season) %>% 
  summarise(availability = n())

burrow_means <- left_join(burrow_means, burrow_availability) %>% 
  group_by(season) %>% 
  # calculate probability of use 
  mutate(weighted_intensity = selection_intensity * availability,  # Multiply selection intensity by availability
         total_weighted_intensity = sum(weighted_intensity),             # Compute total sum
         probability_of_use = (weighted_intensity / total_weighted_intensity)*100) %>%  # Normalize to sum to 1
  data.frame()

###############################################################

# Finalise the plots and save them
plot_final <- plot2 / plot1
saveRDS(plot_final, "plot_RSF_season.RDS")
ggsave("plot_RSF_season.pdf", plot_final, device = "pdf", units = "in", dpi = 600, height = 6.25, width = 6)
ggsave("plot_RSF_season.png", plot_final, device = "png", units = "in", dpi = 600, height = 6.25, width = 6)

#saveRDS(plot2, "plot_RSFburrow_season.RDS")
#saveRDS(plot1, "plot_RSFforaging_season.RDS")

#################### END OF SCRIPT #######################