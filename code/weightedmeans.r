
### get community weighted means

arhropods_annual <- ddply(arthropods_core[,c("PlotID", "SpeciesID","Adults")], .(PlotID, SpeciesID), summarize, mean = round(mean(Adults, na.rm = TRUE),2) )

AEG1 <- subset(arhropods_annual, PlotID == plots[1])

cbind(AEG1, arthropod_traits[match(AEG1$SpeciesID, arthropod_traits$SpeciesID),6:7])

weighted_means <- colSums(arthropod_traits[match(AEG1$SpeciesID, arthropod_traits$SpeciesID),6:7]*AEG1$mean, na.rm = TRUE)/sum(AEG1$mean, na.rm = TRUE)

weightedmeans <- function(abundances, traits, columns) {}