########################################################

# ANALYSING BURROW DENSITY AND DYNAMICS 
# Jack T adapting and extending Annika's code: 3rd June 2025

#########################################################

# Start by modelling the burrow switching probability
# I will first model as nlme
# before then looking into modelling spatial effects explicitly via sdmTMB 

# Load in and set up the data set
setwd("C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Meerkat Intergroup Differences/Burrow density and switching rates")

lapply(c("tidyverse", "nlme", "glmmTMB", "sdmTMB", "ggeffects", "patchwork", "sf", "terra", "fmesher", "emmeans"), FUN = library, character.only = TRUE)
select <- dplyr::select

burrow_sequences <- read.csv("burrow_sequences.csv", header = TRUE) %>% 
  filter(GroupSize <= 40, 
         distance_m < 4000) %>% 
  select(-breeding_season, -period) %>% 
  mutate(Date = as.Date(Date), 
         breedingyear = case_when(Date >= as.Date(paste0(year,"-10-01")) ~ paste0(year,"/",year+1),
                                  Date < as.Date(paste0(year,"-10-01")) ~ paste0(year-1,"/",year)),
         season = if_else(month(Date)%in%c(10,11,12,1,2,3,4), "Oct-Apr", "May-Sep"), 
         GroupRef = as.factor(GroupRef)) %>% 
  filter(between(Date, as.Date("2002-05-01"), as.Date("2023-12-31")))
range(burrow_sequences$Date)

# Move probability
nrow(burrow_sequences)
table(burrow_sequences$BurrowChange)
13194/41654 # Raw switch rate = 32%

table(burrow_sequences$BurrowChange[burrow_sequences$Babysitting == "N"])
11826/(11826 + 17938) # closer to 40% otherwise

# What proprtion of observations involved babysitting in wet vs dry
burrow_sequences %>% 
  group_by(season, Babysitting) %>% 
  summarise(n = n(), 
            PercentBurrowSwitching = sum(BurrowChange)/n())
8485/( 8485 + 16241)   # wet season babysitting 34% of the time
3323 /(3323  +  13523)  # dry season 19.7%

table(burrow_sequences$BurrowChange[burrow_sequences$season == "Oct"])
11826/(11826 + 17938) # closer to 40% otherwise
  
# Recap
table(burrow_sequences$habitat)
# So grassland is heavily under-utilised as burrowing habitat. 

#-----------------------------------
# Fit intial glms on P|burrow switch and move distance given a switch 

# We will model breeding season as a factor to allow an easy look at per-year effects
m1 <- glm(BurrowChange ~ habitat*season + GroupSize + I(GroupSize^2) + PupPresence*season + breedingyear, data = burrow_sequences, family = "binomial")
summary(m1)

#plot(ggeffect(m1, term = "habitat"))
#plot(ggeffect(m1, term = "season"))
#plot(ggeffect(m1, terms = c("season", "habitat")))
#plot(ggeffect(m1, term = "GroupSize"))
#plot(ggeffect(m1, term = c("PupPresence", "season")))
#plot(ggeffect(m1, term = "breedingyear"))

# Move distance 
#hist(filter(burrow_sequences, BurrowChange == 1)$distance_m)
#mean(filter(burrow_sequences, BurrowChange == 1)$distance_m)
#sd(filter(burrow_sequences, BurrowChange == 1)$distance_m)

# Repeat this for the breeding distance 
#m2 <- glm(distance_m ~ habitat*season + GroupSize + I(GroupSize^2) + 
#            PupPresence + breedingyear, 
#          data = filter(burrow_sequences, BurrowChange == 1))

#plot(ggeffect(m2, term = "habitat")
#plot(ggeffect(m2, term = "season"))
#plot(ggeffect(m2, terms = c("season", "habitat")))
#plot(ggeffect(m2, term = "GroupSize"))
#plot(ggeffect(m2, term = "PupPresence"))
#plot(ggeffect(m2, term = "breedingyear"))

#-----------------------------------
# Translate the burrow swithcing into an LMMs with an appropriate random effects structure
burrow_sequences$GroupSize_z <- as.numeric(scale(burrow_sequences$GroupSize))
burrow_sequences$season <- as.character(burrow_sequences$season)
burrow_sequences$PupPresence <- as.character(burrow_sequences$PupPresence)

m1 <- glmmTMB(BurrowChange ~ habitat*season + GroupSize_z + I(GroupSize_z^2) + 
            PupPresence*season + breedingyear + (1|GroupRef) + (1|BurrowRef_New), 
            data = burrow_sequences, 
            family = "binomial")
summary(m1)

# Marginal effects
#plot(ggeffect(m1, term = "habitat"))
#plot(ggeffect(m1, term = "season"))
#plot(ggeffect(m1, terms = c("season", "habitat")))
#plot(ggeffect(m1, term = "GroupSize_z"))
#plot(ggeffect(m1, term = "PupPresence"))
#plot(ggeffect(m1, term = c("PupPresence", "season")))
#plot(ggeffect(m1, term = "breedingyear"))

# Extract the meginal means to compare the habitat contrasts per season
plot(emmeans(m1,~ habitat + season)) # will be similar to marginal effect
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
# nothing obvious, supporting the lack of further modelling down.

# Generate the plots I want
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
#saveRDS(p_habitatseason, "p_burrow_season.RDS")
#ggsave("p_burrow_habitat.pdf", p_habitatseason, width = 4.25, height = 3, dpi = 450, device = cairo_pdf)
#ggsave("p_burrow_habitat.png", p_habitatseason, width = 4.25, height = 3, dpi = 450)

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
#saveRDS(p1_final, "p_burrow_other.RDS")
#ggsave("p_burrow_other.pdf", p1_final, width = 5.35, height = 6, dpi = 450, device = cairo_pdf)
#ggsave("p_burrow_other.png", p1_final, width = 5.35, height = 6, dpi = 450)


# Model distance of burrow switch #----------------------------

# Model distance of burrow switching
#m2 <- glmmTMB(distance_m ~ habitat*season + GroupSize + I(GroupSize^2) + 
#                PupPresence + breedingyear + (1|GroupRef) + (1|BurrowRef_New), 
#              data = filter(burrow_sequences, BurrowChange == 1))
#summary(m2)
#plot(ggeffect(m2, terms = c("season", "habitat")))

# right-skew so need 
#nrow(filter(burrow_sequences, BurrowChange == 1))
#hist(log(filter(burrow_sequences, BurrowChange == 1)$distance_m))
#m2b <- glmmTMB(log(distance_m) ~ habitat*period + GroupSize + I(GroupSize^2) + 
#                PupPresence + breeding_season + (1|GroupRef), 
#              data = filter(burrow_sequences, BurrowChange == 1))
#summary(m2b)
#plot(simulateResiduals(m2b))

# doesn't affect how far they move should they move

#plot(ggeffect(m2b, term = "habitat"))
#plot(ggeffect(m2b, term = "period"))
#plot(ggeffect(m2b, terms = c("period", "habitat")))
#plot(ggeffect(m2b, term = "GroupSize"))
#plot(ggeffect(m2b, term = "PupPresence"))
#plot(ggeffect(m2b, term = "breeding_season"))


#--------------------------------------------------
# Now extend to plot a spatial random field via sdmTMB 

# THIS CODE IS ONLY AN EXAMPLE... NO RESIDUAL AUTOCORRELATION

# First need to create a mesh over all the unique burrow points. 
burrow_unique <- burrow_sequences %>% 
  dplyr::select(BurrowRef, Longitude_New, Latitude_New) %>% 
  distinct()

burrow_unique <- vect(burrow_unique, geom = c("Longitude_New", "Latitude_New"), crs = "EPSG:4326")
# Reproject to UTM Zone 34S with WGS84 datum
burrow_unique_utm <- project(burrow_unique, "+proj=utm +zone=34 +south +datum=WGS84") %>% 
  sf::st_as_sf() %>% 
  sf::st_geometry(.)/1000  ; #plot(burrow_unique_utm)

# Create non-convex hull boundary
burrow_boundary <- fm_nonconvex_hull(burrow_unique_utm, convex = 0.1, concave = -0.1)
par(mfrow = c(1,2)) ; plot(burrow_boundary) ; points(burrow_unique_utm)

# Generate mesh using inla based on the boundary  
inla_mesh <- fm_mesh_2d(loc = burrow_unique_utm, # coordinates
                        max.edge = c(0.6, 2), # max triangle edge length; inner and outer meshes
                        offset = c(0.3, 0.4),  # inner and outer border widths
                        cutoff = 0.3, # minimum triangle edge length
                        boundary = burrow_boundary)
plot(inla_mesh) ; par(mfrow = c(1,1)) # we have no information on outer boundary so this is good. Also recovers a good density within the plotting area. 

# Convert original model data to sf with same projection so that the mesh can be created
burrow_sequences_sf <- burrow_sequences %>%
  sf::st_as_sf(coords = c("Longitude_New", "Latitude_New"), crs = 4326) %>%
  sf::st_transform("+proj=utm +zone=34 +south +datum=WGS84")

# Extract coordinates and convert to kilometers
coords_km <- sf::st_coordinates(burrow_sequences_sf) / 1000
burrow_sequences$X <- coords_km[,1]
burrow_sequences$Y <- coords_km[,2]

mesh_burrows <- make_mesh(burrow_sequences, xy_cols= c("X","Y"), mesh = inla_mesh)

# Now we can model it 
burrow_sequences$GroupRef <- as.factor(burrow_sequences$GroupRef)
burrow_sequences$BurrowRef_New <- as.factor(burrow_sequences$BurrowRef_New)
burrow_sequences$breedingyear <- as.factor(burrow_sequences$breedingyear)

# First model without a spatial random field
m_base <- sdmTMB(BurrowChange ~ habitat*season + breedingyear + PupPresence +
                   GroupSize_z + I(GroupSize_z^2) + (1|GroupRef) + (1|BurrowRef_New), 
                 data = burrow_sequences, 
                 family = binomial(),
                 #mesh = mesh_burrows,
                 spatial="off")
summary(m_base)

# Now include the spatial random field: 
m_sprf <- sdmTMB(BurrowChange ~ habitat*season + breedingyear + PupPresence +
                   GroupSize_z + I(GroupSize_z^2) + (1|GroupRef) + (1|BurrowRef_New),  
                 data = burrow_sequences, 
                 family = binomial(),
                 mesh = mesh_burrows,
                 spatial="on")
summary(m_sprf)

AIC(m_base, m_sprf) # No benefit to having the spatial effect in there really
# Need a pretty fine-grained spatial effect

# We now want to plot the spatial effect. 

# First need to generate a grid over which to plot the predictions: 
grid <- sf::st_make_grid(burrow_boundary, cellsize = 0.3, what = "centers", square = TRUE) ; #plot(grid)
# Convert to sf data frame for prediction
pred_grid <- sf::st_as_sf(grid)
pred_grid$X <- sf::st_coordinates(pred_grid)[, 1]
pred_grid$Y <- sf::st_coordinates(pred_grid)[, 2]

# Link the coordinates to the habitat type 
habitat_fd <- "C:/Users/Jack/OneDrive/Documents/Kalahari/Cambridge LARG/Cambridge PostDoc2/Meerkat/Kalahari Habitat map/"
habitat_shp <- paste0(habitat_fd, "/habClPanRiver.shp") 
habitat <- terra::vect(habitat_shp) %>% st_as_sf()
habitat <- st_set_crs(habitat, "+proj=utm +zone=34 +south +datum=WGS84")
habitat <- habitat%>%
  mutate(Class_Name = case_when(Class_Name == 'DD' ~ 'Drie Doring',
                                Class_Name == 'grassland' ~ 'Grassland',
                                Class_Name == 'RS mix' ~ 'Red Sand', 
                                Class_Name %in% c('WS mix', "pan", "Riverbed") ~ 'White Sand'))
# check the validity of the habitat polygons
st_is_valid(habitat, reason = TRUE)
habitat <- st_make_valid(habitat)

# Group by class and merge polygons within each class
sf_data <- do.call(rbind, lapply(1:nrow(habitat), function(i) {
  st_cast(habitat[i, ], "POLYGON") }))

habitat_final <- sf_data %>%
  group_by(Class_Name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") 

habitat_final <- habitat_final %>% 
  st_transform(crs = st_crs(habitat)) %>%  
  dplyr::mutate(geometry = geometry / 1000) 

st_crs(habitat_final) <- st_crs(habitat)

# Include the fencelines and water holes
KMP_spatial <- setwd("C:\\Users\\Jack\\OneDrive\\Documents\\Kalahari\\Cambridge LARG\\Cambridge PostDoc2\\Meerkat\\Archive and Other\\KMP Reserve waypoints\\")
st_layers("KurumanRiverReserve.kml")
# Fences
reserve_fences <- st_read("KurumanRiverReserve.kml", layer = "Farm_Boundaries")
reserve_fences <- st_make_valid(reserve_fences)
reserve_fences_utm <- st_transform(reserve_fences, crs = st_crs(habitat_final))
st_geometry(reserve_fences_utm) <- st_geometry(reserve_fences_utm) / 1000
st_crs(reserve_fences_utm) <- st_crs(habitat_final)
reserve_clipped <- st_intersection(reserve_fences_utm, st_union(habitat_final))
reserve_clipped <- filter(reserve_clipped, !(Name %in% c("KRR639", "Meerkat gat")))

# Now we can link pred_grid to the habitats ready for spatial plotting
st_crs(pred_grid) <- st_crs(habitat_final)
pred_grid <- st_join(pred_grid, habitat_final) %>% 
  rename(habitat = Class_Name) %>% 
  dplyr::filter(!is.na(habitat))
plot(pred_grid)

st_crs(burrow_boundary) <- st_crs(pred_grid)
pred_grid <- sf::st_intersection(pred_grid, burrow_boundary)

# Generate two grids here to predict in space for the wet and dry season: 
pred_data <- bind_rows(pred_grid %>% mutate(season = "May-Sep"),
                       pred_grid %>% mutate(season = "Oct-Apr"))
pred_data <- st_drop_geometry(pred_data)

# We can now expand to add the other effects before predicting
pred_data <- pred_data %>% 
  mutate(breedingyear = as.factor("2001/2002"), 
         PupPresence = as.factor("neither"), 
         GroupSize_z = 0, 
         BurrowRef_New = as.factor(burrow_sequences$BurrowRef_New[1]), 
         GroupRef = as.factor(burrow_sequences$GroupRef[1]))

# Generate the predictions 
predictions <- predict(m_sprf, newdata = pred_data, re_form_iid = NA) %>% 
  mutate(X = X*1000, Y = Y*1000)

# Plot the spatial variation
inv.logit <- function(x) {1 / (1 + exp(-x))}

reserve_clipped_m <- reserve_clipped %>%
  st_set_crs(st_crs(32634)) %>%  # make sure it's explicitly UTM 34S
  mutate(geometry = geometry * 1000)

# Fixed and random effects
ggplot(filter(predictions, season == "Oct-Apr")) + 
  geom_raster( aes(x = X, y = Y, fill = inv.logit(est))) + 
  geom_sf(data = reserve_clipped_m, fill = NA, colour = "black", linewidth = 0.5) +
  facet_wrap(~ season ) + 
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects + all random effects)") + 
  xlim(c(575000, 590000)) + 
  ylim(c(7009000, 7023000))

#ggplot(filter(predictions, season == "Oct-Apr")) + 
#  geom_raster( aes(x = X, y = Y, fill = inv.logit(est_non_rf))) + 
#  geom_sf(data = reserve_clipped_m, fill = NA, colour = "black", linewidth = 0.5) +
#  facet_wrap(~ season ) + 
#  scale_fill_viridis_c() +
#  ggtitle("Prediction (fixed effects only)") + 
#  xlim(c(575000, 590000)) + 
#  ylim(c(7009000, 7023000))

#ggplot(filter(predictions, season == "Oct-Apr")) + 
#  geom_raster( aes(x = X, y = Y, fill = omega_s)) + 
#  geom_sf(data = reserve_clipped_m, fill = NA, colour = "black", linewidth = 0.5) +
#  facet_wrap(~ season ) + 
#  scale_fill_viridis_c() +
#  ggtitle("Prediction (spatial random field only)") + 
#  xlim(c(575000, 590000)) + 
#  ylim(c(7009000, 7023000))

# Now tt's worth predicting the other variables separately
#p <- predict(m_sprf, newdata = pred_data, se_fit = TRUE, re_form = NA, type = "link") 
  
#p <- mutate(p, l95ci = inv.logit(est - est_se), 
#            u95ci = inv.logit(est + est_se),
#            est = inv.logit(est)) %>% 
#  mutate(season = factor(if_else(season == "May-Sep", "Dry (May-Sep)", "Wet (Oct-Apr)"),# 
  #                       levels = c("Wet (Oct-Apr)", "Dry (May-Sep)")),
  #       habitat = factor(case_when(habitat  == "Drie Doring" ~ "Drie doring", 
  #                                  habitat  == "Grassland" ~ "Grassland", 
  #                                  habitat  == "Red Sand" ~ "Red sand", 
  #                                  habitat == "White Sand" ~ "White sand"), 
  #                        levels = c("Grassland", "Red sand", "White sand", "Drie doring")))
  
#p1 <- ggplot(data = p, aes(x = season, y = est, colour = habitat, fill = habitat)) + 
#  geom_errorbar(aes(ymin = l95ci, ymax = u95ci), size = 0.8, width = 0, 
#                position = position_dodge(width = 0.65), show.legend = FALSE) + 
#  geom_point(shape = 21, colour = "black", size = 3, stroke = 0.8, 
#             position = position_dodge(width = 0.65)) + 
#  theme_bw() + 
#  theme(panel.grid = element_blank(), 
#        axis.text.x = element_text(colour = "black", size = 9.5, angle = 45, hjust = 1),
#        axis.text.y = element_text(colour = "black", size = 9.5),
#        axis.title = element_text(colour = "black", size = 10.5), 
#        legend.text = element_text(size = 9.5), 
#        legend.title = element_text(hjust = 0.5)) + 
#  scale_colour_manual(values = c("chocolate", "sandybrown", "burlywood", "forestgreen")) +
#  scale_fill_manual(values = c("chocolate", "sandybrown", "burlywood", "forestgreen")) +
#  labs(fill = "Habitat", x = "Season", y =  "Probabilty of burrow switch") 
#p1

############################ END OF SCRIPT #################