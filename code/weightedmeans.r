### get community weighted means

source("R/cwm.r")
source("code/data.r")
library(foreach)
library(doSNOW)
################## calculate plant CWM values per plot per year. 


#workerlist <- c(rep("localhost", times = 3))
#cl <- makeSOCKcluster(workerlist)
#registerDoSNOW(cl)

#plants_annual <- ddply(plants_full[,c("EP_PlotId", "Species","Year","cover")], .(EP_PlotId, Species, Year), summarize, mean = round(mean(cover, na.rm = TRUE),2), .drop = TRUE )

#stopCluster(cl)

cwm_plants <- ddply(plants_full, .(EP_PlotId,Year), cwm, trait_table = plant_trait_matrix, traits = c("SLA", "leaf_P", "leaf_N", "LDMC", "leaf_area", "seedmass","SSD"), abund_label = "cover", spec_label = "SpeciesID", trait_spec_label= "AccSpeciesID")

save(cwm_plants, file = "data/cwm_plants.rData")

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


## WARNING: Check calculation of annual means: species that have not been reported should have value 0 at each plot and not be treated as NA.  
arthropods_annual <- ddply(arthropods_core[arthropods_core$selector == TRUE,c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, Species, CollectionYear), summarize, mean = round(sum(NumberAdults, na.rm = TRUE)/2 ,4), .drop = TRUE )

cwm_arthropods <- ddply(arhropods_annual, .(PlotID,CollectionYear), cwm, trait_table = arthropod_traits, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"))

pairs(cwm_arthropods[3:5], pch = 20)



herbivores_annual <- ddply(herbivores[herbivores$selector == TRUE,c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, Species, CollectionYear), summarize, mean = round(sum(NumberAdults, na.rm = TRUE)/2 ,4), .drop = TRUE )

cwm_herbivores <- ddply(herbivores_annual, .(PlotID,CollectionYear), cwm, trait_table = arthropod_traits, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric", "Feeding_mode_numeric"))

pairs(cwm_herbivores[3:6], pch = 20)




consumers_annual <- ddply(consumers[consumers$selector == TRUE,c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, Species, CollectionYear), summarize, mean = round(sum(NumberAdults, na.rm = TRUE)/2 ,4), .drop = TRUE )

cwm_consumers <- ddply(consumers_annual, .(PlotID,CollectionYear), cwm, trait_table = arthropod_traits, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"))

pairs(cwm_consumers[3:5], pch = 20)

 
