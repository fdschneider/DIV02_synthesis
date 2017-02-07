# create trait table per plant species

plant_traits_raw <- read.csv("data/plants_traits_TRY/2181.txt", sep = "\t" )

# original file: 2181.txt as downloaded on 01.08.2016 from TRY database following data request #2181. Traits requested: ... , plant IDs requested: ...)  

plant_trait_datasets <- unique(plant_traits_raw$Dataset)
plant_trait_authors <- unique(paste(plant_traits_raw$FirstName, plant_traits_raw$LastName))
plant_trait_species <- levels(plant_traits_raw$AccSpeciesName)

# preparation: remove unrealistic trait values

plant_traits_raw <- subset(plant_traits_raw, !is.na(TraitID))
plant_traits_raw$ID <- 1:length(plant_traits_raw$LastName)

#trait_observations <- ddply(plant_traits_raw[, c("AccSpeciesID","AccSpeciesName", "TraitID", "TraitName", "StdValue")], .(TraitID,AccSpeciesID), summarise,  AccSpeciesName = unique(AccSpeciesName)[1], observations = sum(StdValue/StdValue, na.rm = TRUE) )
#subset(trait_observations, observations > 100 )

#par(mfrow = c(2,1), mar = c(0,2,1,1), bty = "n")
#boxplot(log(subset(plant_traits_raw, TraitID == 1 & AccSpeciesName == "Dactylis glomerata")$StdValue), horizontal = TRUE, xaxt = "n") -> hdat
#par(mar = c(3,2,0,1))
#hist(subset(plant_traits_raw, TraitID == 1 & AccSpeciesName == "Dactylis glomerata")$StdValue, main = NA)
#lines(exp(hdat$stats[c(1,5)]), c(10,10), lwd = 1)
#lines(exp(hdat$stats[c(2,4)]), c(10,10), lwd = 5)
#lines(exp(hdat$stats[c(3,3)]), c(9,11), lwd = 3, col = "white")

## rectify data by excluding extreme observations (outliers outside 95% inner quantile on log-transformed values, Chambers et al 1983, based on function hist() in R)

## identify outliers
clean_out <- lapply(levels(plant_traits_raw$AccSpeciesName), function(x) {
  outlier <- boxplot.stats(log(subset(plant_traits_raw, TraitID == 1 & AccSpeciesName == x)$StdValue))$out 
  subset(plant_traits_raw, AccSpeciesName == x  & log(StdValue) %in% outlier)[,c("ID", "LastName","FirstName", "AccSpeciesName","StdValue")]
})
clean_out <- ldply(clean_out, data.frame)

## remove outliers

plant_traits_clean <- plant_traits_raw[-clean_out$ID,] 
rm(plant_traits_raw)

# crunching average trait values per plant

## step 1: take average value per author

plant_traits_perauthor <- ddply(plant_traits_clean[, c("LastName", "AccSpeciesName", "AccSpeciesID", "TraitID", "TraitName", "StdValue", "UnitName", "ErrorRisk")], .(LastName, AccSpeciesID, TraitID), summarise, StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), UnitName = unique(UnitName)[1], ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  TraitName = unique(TraitName)[1] ,  AccSpeciesName = unique(AccSpeciesName)[1] )
rm(plant_traits_clean)

## step 2: take average per plant species

plant_traits <- ddply(plant_traits_perauthor, .(AccSpeciesID, TraitID), summarise, StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), UnitName = unique(UnitName)[1], ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  TraitName = unique(TraitName)[1] ,  AccSpeciesName = unique(AccSpeciesName)[1] )
rm(plant_traits_perauthor)

plant_traits$TraitName_short <- plant_traits$TraitName
levels(plant_traits$TraitName_short) <- c("","leaf_area", "SLA","leaf_drymass","LDMC", "leaf_N",  "leaf_P", "leaf_thickness","height","Nfixation","palatability","pollination","root_depth","seedmass","stem_drymass")

## step 3: build plant -- trait matrix 

plant_trait_matrix <- dcast(plant_traits, AccSpeciesID ~ TraitName_short, value.var = "StdValue" )

save(plant_trait_matrix, file = "data/plant_trait_matrix.rData")

