# Preliminary Nocturnality as a Function of Human Presence
# Margaret Mercer
# May 1, 2024

library(tidyverse)
library(lme4)

# Whitetail ####

# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "White-tailed Deer")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day 0.14628    0.00663  22.063   <2e-16 ***


# Mule Deer ####

# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Mule Deer")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  0.04101    0.02146   1.910   0.0561 .


# Moose ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Moose")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  -0.6612     0.2461  -2.686  0.00723 ** 

# Elk ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Elk")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  -0.4168     0.1443  -2.888  0.00388 **

# Snowshoe Hare ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Snowshoe Hare")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day   3.0819     1.5896   1.939   0.0525 .

# Whitetailed Jackrabbit ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "White-tailed Jackrabbit")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day   11.895     28.486   0.418   0.6763

# Blacktailed Jackrabbit ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Black-tailed Jackrabbit")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  -1.9264     0.5994  -3.214  0.00131 **

# Cottontail ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, data$Species_Name == 'Sylvilagus aquaticus' |
                    data$Species_Name == 'Sylvilagus audubonii'|
                    data$Species_Name == 'Sylvilagus bachmani' |
                    data$Species_Name == 'Sylvilagus floridanus' |
                    data$Species_Name == 'Sylvilagus nuttallii' |
                    data$Species_Name == 'Sylvilagus obscurus' |
                    data$Species_Name == 'Sylvilagus palustris')

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day -0.008013   0.011950  -0.671    0.502 

# Puma ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Puma")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

plot(species$Is_Night~log(species$Humans_Per_Camera_Per_Day))

# Humans_Per_Camera_Per_Day     0.8771     0.4779   1.835  0.06649 .

# Wolf ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Grey Wolf")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  -0.1251     0.3445  -0.363 0.716438 

# Black Bear ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "American Black Bear")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  0.17213    0.01785   9.646   <2e-16 ***

# Grizzly Bear ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Brown Bear")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  -1.5014     0.5693  -2.637  0.00835 **

# Coyote ####
# import data
data <- read_csv("../data_too_big/all_years.csv")

# subset species

species <- subset(data, Common_Name == "Coyote")

summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = species, family = binomial))

# Humans_Per_Camera_Per_Day  0.09828    0.01373   7.156 8.28e-13 ***