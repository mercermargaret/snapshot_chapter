# spatial analysis
# Margaret Mercer
# May 29, 2024

# load packages ####
library(tidyverse)
library(sf)
library(pscl)

# import data ####
data <- read_csv("../data_too_big/all_years.csv")
range <- st_read('data/subset_shape_files/Puma')

# wrangle data ####

sf_use_s2(FALSE)

# cut down so it's only cameras in range of animal
# Convert df to an sf object
points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(range)))
st_is_valid(points_sf, reason=TRUE)
sf_use_s2(TRUE)
# Perform the point-in-polygon test
inside <- st_within(points_sf, range, sparse = FALSE)
# Extract rows from df that are inside the polygon
df_inside <- data[which(inside[,1]),]
# df_inside now contains only the rows where coordinates fall inside the polygon

# now make df_inside spatial
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(range)))

ggplot() +
  geom_sf(data = range[1,], size = 1.5, color = "black", fill = "#690375") +
  geom_sf(data = spatial_inside) +
  coord_sf()

# turn df_inside into "data"
data <- df_inside

# for each species, create a new column that has "rate of detection of animal" (just like the Humans_per_camera_per_day)
data$Species <- ifelse(data$Species_Name == "Puma concolor", 1, 0)
Species_Per_Camera <- data %>% filter(Species == 1) %>%
  group_by(Site_Name) %>%
  summarise(Species_Per_Camera = n())
data <- left_join(data, Species_Per_Camera, by = "Site_Name") %>%
  mutate(Species_Per_Camera = tidyr::replace_na(Species_Per_Camera, 0)) # replace nas with 0
data$Species_Per_Camera_Per_Day <- data$Species_Per_Camera/data$Survey_Days
data <- subset(data, select = c(-Species))

# now get dataframe with one row per camera
cameras <- data %>% distinct(Site_Name, .keep_all = TRUE)
cameras <- dplyr::select(cameras, Array:Longitude, Time_Zone, Year, Humans_Per_Camera, Humans_Per_Camera_Per_Day, Species_Per_Camera, Species_Per_Camera_Per_Day)


# analysis ####
# # visualize first
# plot(cameras$Species_Per_Camera_Per_Day ~ cameras$Humans_Per_Camera_Per_Day)
# plot(log(cameras$Species_Per_Camera_Per_Day + 1) ~ log(cameras$Humans_Per_Camera_Per_Day + 1))
# ggplot(data = cameras, aes(x = Humans_Per_Camera_Per_Day, y = Species_Per_Camera_Per_Day)) +
#   geom_point() +
#   geom_smooth()
# ggplot(data = cameras, aes(x = log(Humans_Per_Camera_Per_Day + 1), y = log(Species_Per_Camera_Per_Day + 1))) +
#   geom_point() +
#   geom_smooth()
# # hm, i don't like this. its super 0 inflated, even when log transformed
# # lets see how each variable is distributed
# hist(cameras$Species_Per_Camera_Per_Day)
# hist(cameras$Humans_Per_Camera_Per_Day)
# # both are EXTREMEMLY 0 inflated
# # log transformed?
# hist(log(cameras$Species_Per_Camera_Per_Day + 1))
# hist(log(cameras$Humans_Per_Camera_Per_Day + 1))
# # still ugly
# 
# # can you DOUBLE log transform it?
# hist(log(log(cameras$Species_Per_Camera_Per_Day + 1)))
# hist(log(log(cameras$Humans_Per_Camera_Per_Day + 1)))
# # well now it looks normal lollll
# # but is it even allowed?
# 
# # lets do a ggplot on it
# ggplot(data = cameras, aes(x = (log(log(Humans_Per_Camera_Per_Day + 1) + 1)), y = (log(log(Species_Per_Camera_Per_Day + 1) + 1)))) +
#   geom_point() +
#   geom_smooth()
# # still nah


# poisson with offset
model <- glm(Species_Per_Camera ~ Humans_Per_Camera_Per_Day + offset(log(Survey_Days)), data = cameras, family = poisson)
summary(model)
plot(model)

# puma:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -5.51135    0.06523  -84.49  < 2e-16 ***
#   Humans_Per_Camera_Per_Day  0.08584    0.03191    2.69  0.00714 **


# wolf:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -4.95470    0.06497 -76.262  < 2e-16 ***
#   Humans_Per_Camera_Per_Day  0.13766    0.05282   2.606  0.00915 **


# ZERO inflated poisson with offset
zip_model <- zeroinfl(Species_Per_Camera ~ Humans_Per_Camera_Per_Day + offset(log(Survey_Days)) | Humans_Per_Camera_Per_Day, data = cameras)
summary(zip_model)

# puma:
# Count model coefficients (poisson with log link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -3.30483    0.08309 -39.776  < 2e-16 ***
#   Humans_Per_Camera_Per_Day  0.14537    0.05606   2.593  0.00951 ** 
#   
#   Zero-inflation model coefficients (binomial with logit link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.046783   0.108561  18.854   <2e-16 ***
#   Humans_Per_Camera_Per_Day -0.009576   0.074269  -0.129    0.897 


# wolf:
# Count model coefficients (poisson with log link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                -3.2047     0.0806 -39.761   <2e-16 ***
#   Humans_Per_Camera_Per_Day   0.2286     0.1015   2.252   0.0243 *  
#   
#   Zero-inflation model coefficients (binomial with logit link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 1.5459     0.1170  13.208   <2e-16 ***
#   Humans_Per_Camera_Per_Day  -0.1523     0.1258  -1.211    0.226



# cameras with predator vs cameras without predator ~ cameras with humans vs cameras without humans
cameras$Human_Present <- ifelse(cameras$Humans_Per_Camera > 0, 1, 0)
cameras$Species_Present <- ifelse(cameras$Species_Per_Camera > 0, 1, 0)

model2 <- glm(cameras$Species_Present ~ cameras$Human_Present, family = "binomial")
summary(model2)

# puma:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -3.3810     0.4548  -7.435 1.05e-13 ***
#   cameras$Human_Present   1.1838     0.4660   2.540   0.0111 *  

# wolf:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -1.5581     0.3890  -4.006 6.19e-05 ***
#   cameras$Human_Present  -0.1665     0.4046  -0.411    0.681
