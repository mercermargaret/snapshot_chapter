# Add Column to Prey Dataframe with Pred Noct Proportion
# Margaret Mercer
# January 2024


prey <- subset(raw2019, Species_Name == "Alces alces")
pred <- subset(raw2019, Species_Name == "Canis lupus") # subset

# get proportion of predator nocturnality for each site name
pred_noct_by_site <- pred %>%
  group_by(Site_Name) %>%
  summarize(
    Pred_Noct_By_Site = mean(IsNight == 1)
  )

pred <- merge(pred, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Merge proportions back to the original dataframe, keeping all rows from the original dataframe

prey <- merge(prey, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Now I add a column to the "prey" dataframe with the proportion of predator nocturnality

