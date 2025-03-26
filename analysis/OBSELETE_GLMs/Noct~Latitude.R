# Nocturnality as a function of latitude
# Margaret Mercer
# May 23, 2024

data <- read_csv("../data_too_big/all_years.csv")
cameras <- read.csv("data/cameras.csv")


binomial <- data %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(Is_Night)) # Calculate binomial proportions for each site
counts <- data %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(Is_Night == 1),
            Total_Obs = n()) # Create columns for #nightobservations/site and total#observations/site.
props <- merge(counts, binomial, by = "Site_Name") # merge together. now we have a dataframe with each site, the total night observations, total observations, and the proportion of the two.

unique_data <- data %>% distinct(Site_Name, .keep_all = TRUE)
unique_data <- unique_data %>% dplyr::select(Site_Name, Latitude)
props_updated <- props %>% 
  left_join(unique_data, by = "Site_Name")

summary(glm(data = props_updated, Prop_Noct ~ Latitude))

plot(data = props_updated, Prop_Noct ~ Latitude)
