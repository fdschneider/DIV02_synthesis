# get community weighted means

## apply transformations 

#load("data/arthropod_trait_matrix.rData")

arthropod_trait_matrix <- mutate(arthropod_trait_matrix, Body_Size = log(Body_Size))

# par(mfrow = c(2,3))
# for(i in colnames(arthropod_trait_matrix)[-c(1:5)]) {
#   hist(arthropod_trait_matrix[,i], xlab = i, col = "black", breaks = 32)
# }

#load("data/plant_trait_matrix.rData")

plant_trait_matrix <- mutate(plant_trait_matrix, 
                             leaf_area = log(leaf_area),
                             #SLA = log(SLA),
                             leaf_drymass = log(leaf_drymass),
                             #LDMC = log(LDMC),
                             #leaf_N = log(leaf_N),
                             #leaf_P = log(leaf_P),
                             #leaf_thickness = log(leaf_thickness),
                             height = log(height),
                             #root_depth = log(root_depth),
                             seedmass = log(seedmass),
                             stem_drymass = log(stem_drymass)
)

# par(mfrow = c(3,4))
# for(i in colnames(plant_trait_matrix[,-1])) {
#   hist(plant_trait_matrix[,i], xlab = i, col = "black", main = NA, breaks = 32)
# }
# 



## calculate plant CWM values per plot per year. 

#workerlist <- c(rep("localhost", times = 3))
#cl <- makeSOCKcluster(workerlist)
#registerDoSNOW(cl)

#plants_annual <- ddply(plants_full[,c("EP_PlotId", "Species","Year","cover")], .(EP_PlotId, Species, Year), summarize, mean = round(mean(cover, na.rm = TRUE),2), .drop = TRUE )

#stopCluster(cl)


cwm_plants <- ddply(plants_full, .(EP_PlotId,Year), cwm, trait_table = plant_trait_matrix, traits = c("SLA", "leaf_P", "leaf_N", "LMA", "height", "leaf_area", "seedmass","stem_drymass", "leaf_thickness", "Nfixation", "palatability", "root_depth"), abund_label = "cover", spec_label = "SpeciesID", trait_spec_label= "AccSpeciesID")

cwm_plants$Region <- as.factor(substr(cwm_plants$EP_PlotId, 1,3))

#cwm_plants[,3:11] <- log10(cwm_plants[,3:11])

# 
# {
# p <- "HEG12"
# y <- "2008"
# s <- "Orius niger" "
# x <- subset(arthropods_core, PlotID == p & CollectionYear == y & Species == s & selector == TRUE, .drop = TRUE)
# 
# 
# out <- data.frame(
#   Species = arthropod_species,
#   NumberAdults = rep(0, length(arthropod_species))
# )
# 
# out$NumberAdults[match(x$Species ,  arthropod_species)] <- x$NumberAdults
# arthropod_species[!arthropod_species %in% heg12$Species]
# }

cwm_predators <- ddply(predators_core, .(EP,Year), cwm, trait_table = arthropod_trait_matrix, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"), abund_label = "Abundance", spec_label = "SpeciesID")

names(cwm_predators)[1] <- "EP_PlotId"
cwm_predators$Year <- as.factor(cwm_predators$Year)
cwm_predators$Region <- as.factor(substr(cwm_predators$EP_PlotId, 1,3))


cwm_herbivores <- ddply(herbivores_core, .(EP,Year), cwm, trait_table = arthropod_trait_matrix, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric", "Feeding_suckers",  "Feeding_specialization_numeric" ), abund_label = "Abundance", spec_label = "SpeciesID")

names(cwm_herbivores)[1] <- "EP_PlotId"
cwm_herbivores$Year <- as.factor(cwm_herbivores$Year)
cwm_herbivores$Region <- as.factor(substr(cwm_herbivores$EP_PlotId, 1,3))

#pairs(cwm_herbivores[,c(3:7)], pch = 20, upper.panel = panel.lm, lower.panel = panel.lm, col = cwm_herbivores$Region)

# 
# herbivores_annual <- ddply(herbivores_core[herbivores$selector == TRUE,c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, Species, CollectionYear), summarize, mean = round(sum(NumberAdults, na.rm = TRUE)/2 ,4), .drop = TRUE )
# 
# cwm_herbivores <- ddply(herbivores_annual, .(PlotID,CollectionYear), cwm, trait_table = arthropod_traits, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric", "Feeding_mode_numeric"))
# 
# pairs(cwm_herbivores[3:6], pch = 20)


# 
# 
# consumers_annual <- ddply(consumers[consumers$selector == TRUE,c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, Species, CollectionYear), summarize, mean = round(sum(NumberAdults, na.rm = TRUE)/2 ,4), .drop = TRUE )
# 
# cwm_consumers <- ddply(consumers_annual, .(PlotID,CollectionYear), cwm, trait_table = arthropod_traits, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"))
# 
# pairs(cwm_consumers[3:5], pch = 20)


# backtransform

cwm_herbivores <- mutate(cwm_herbivores, Body_Size = exp(Body_Size))
cwm_predators <- mutate(cwm_predators, Body_Size = exp(Body_Size))

cwm_plants <- mutate(cwm_plants, 
                     leaf_area = exp(leaf_area),
                     #SLA = log(SLA),
                     #leaf_drymass = log(leaf_drymass),
                     #LDMC = log(LDMC),
                     #leaf_N = log(leaf_N),
                     #leaf_P = log(leaf_P),
                     #leaf_thickness = log(leaf_thickness),
                     height = exp(height),
                     #root_depth = log(root_depth),
                     seedmass = exp(seedmass),
                     stem_drymass = exp(stem_drymass)
)

rm(arthropod_trait_matrix, plant_trait_matrix)

save(cwm_plants, file = "data/cwm_plants.rData")

save(cwm_herbivores, file = "data/cwm_herbivores.rData")

save(cwm_predators, file = "data/cwm_predators.rData")

