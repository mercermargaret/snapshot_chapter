# Javaan's model (takes into account pred presence AND pred nocturnality)
# Margaret Mercer
# January 2024

# Prey Nocturnality (probability) ~ -1 + Predator_Absence*beta_A + Predator_Presence * beta_P + Predator_Prop:Predator_Presence * beta_P_prop + Disturbance * beta_D + Disturbance:Predator_Presence * beta_D_P + Disturbance:Predator_Prop:Predator_Presence * beta_D_P_prop 

# add column to prey dataframe with pred noct by site
prey <- subset(raw2019, Species_Name == "Alces alces")
pred <- subset(raw2019, Species_Name == "Canis lupus") # subset
pred_noct_by_site <- pred %>%
  group_by(Site_Name) %>%
  summarize(
    Pred_Noct_By_Site = mean(IsNight == 1)
  ) # get proportion of predator nocturnality for each site name
pred <- merge(pred, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Merge proportions back to the original dataframe, keeping all rows from the original dataframe
prey <- merge(prey, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Now I add a column to the "prey" dataframe with the proportion of predator nocturnality

# add pred present and pred absent columns to prey dataframe
prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array)
prey$Pred_Absent <- ifelse(prey$Pred_Present == 0, 1, 0)

summary(glm(data = prey, IsNight ~ -1 + Pred_Absent + Pred_Present + Pred_Noct_By_Site:Pred_Present + Disturbance + Disturbance:Pred_Present + Disturbance:Pred_Noct_By_Site:Pred_Present))




