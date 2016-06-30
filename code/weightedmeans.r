### get community weighted means
library(plyr)

source("code/data.r")

cwm <- function(abundances, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"), trait_table = arthropod_traits) {
  temp <- cbind(abundances[,c("Species", "mean")], trait_table[match(abundances$Species, trait_table$SpeciesID),traits])
  
  output <- colSums(temp[,traits]*temp$mean, na.rm = TRUE)/sum(temp$mean, na.rm = TRUE)
  
}

arhropods_annual <- ddply(arthropods_core[,c("PlotID", "Species","CollectionYear","NumberAdults")], .(PlotID, Species, CollectionYear), summarize, mean = round(mean(NumberAdults, na.rm = TRUE),2) )

cwm_arthropods <- ddply(arhropods_annual, .(PlotID,CollectionYear), cwm)

pairs(cwm_arthropods[3:5])



plants_annual <- ddply(plants_full[,c("Plotid", "Species","Year","cover")], .(Plotid, Species, Year), summarize, mean = round(mean(cover, na.rm = TRUE),2) )

cwm_plants <- ddply(plants_annual, .(Plotid,Year), cwm, trait_table = plant_traits, traits = c("SLA", "leafPpermass", "leafNpermass", LDMC) )

