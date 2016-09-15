## synthesis project: trait synchrony
## READ IN DATA FOR CORE ANALYSIS
## Florian D. Schneider
## 08.2016

library(reshape2)
library(plyr)

#########################################
# read in land use index 

lui <- read.csv2("data/land_use/LUI_reg_sep_12.07.2016+153401.txt", sep = "\t") 
lui$Year <-  lui$Std_procedure.year.
levels(lui$Year) <- 2008:2013
lui$Exploratory <-  lui$Std_procedure.exploratory.
levels(lui$Exploratory) <- c("ALB", "HAI", "SCH")

##########################################
# read in plant abundance data

plants_full <- read.csv2("data/plants_core/plantcover.csv") 
# original file: 19686.xlsx  downloaded from   BExis on 17.05.2016 as Bexis dataset 19686 vegetation relevés EP 2008-2015 1.2.5 ; Maintainer: Markus Fischer

plants_full$cover <- as.numeric(plants_full$cover)
levels(plants_full$Year) <- c(2008:2015, 2011)


# remove tree species from list (for extreme trait values assigned to seedlings/saplings)
trees <- c("Acer_sp", "Baumkeimling_sp", "Betula_pendula", "Fraxinus_excelsior", "Populus_tremula", "Prunus_avium", "Prunus_sp","Prunus_spinosa", "Quercus_robur", "Tilia_sp")

plants_full <- subset(plants_full, !Species %in% trees, drop = TRUE)
plants_full$Species <- factor(plants_full$Species)


## match species with TRY species names

try_species <- read.table("data/plants_traits_TRY/TryAccSpecies.txt", sep = "\t", header = TRUE)

plants_full$AccSpeciesNames <- plants_full$Species
levels(plants_full$AccSpeciesNames) <- sub("_aggr.","",levels(plants_full$AccSpeciesNames))
levels(plants_full$AccSpeciesNames)  <- sub("_agg.","",levels(plants_full$AccSpeciesNames) )
levels(plants_full$AccSpeciesNames)  <- sub("(incl_B_commutatus)","",levels(plants_full$AccSpeciesNames) )
levels(plants_full$AccSpeciesNames)  <- sub("_"," ",levels(plants_full$AccSpeciesNames) )

# translate species names into TRY species IDs
## fuzzy matching
#try_ids <- sapply(plant_species, function(x) agrep(x, try_species$AccSpeciesName))

## exact matching
plants_full <- mutate(plants_full, SpeciesID = match(AccSpeciesNames, try_species$AccSpeciesName) )

plants_pooledyears <- ddply(plants_full, .(EP_PlotId), summarise, Year = unique(Year), Species = unique(Species), cover = mean(cover), AccSpecies  )
  
save(plants_full, file = "data/plants_full.rData")

###################################
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


##################################################

# read in arthropod  data

#IDs <- c(16871, 16893, 16894, 16895, 16897, 16908, 17006, 17007, 17008, 17009, 17010, 17011, 17012, 17013, 17014, 17015, 17266, 17246, 17247, 17267)

#lapply(IDs, function(x) colnames(read.csv2(paste0("data/arthropods_core/", x, ".txt"), sep = "", header = TRUE, nrows = 1 )))

#ddf <- lapply(IDs, function(ID) read.csv2(paste0("data/arthropods_core/", ID, ".txt"), sep = "\t") )
# original files: ______,____  downloaded from   BExis on ____ as Bexis dataset ________ ; Maintainer: Martin Goßner


#arthropods_core <- ddf[[1]]
#for(i in 2:length(IDs)) {
#  arthropods_core <- rbind(arthropods_core, ddf[[i]])
#}

#arthropods_core <- subset(arthropods_core, Species != "<NA>")

#plots <- unique(plants_full$EP_PlotId)
#levels(arthropods_core$PlotID) <- plots
#arthropod_species <- levels(arthropods_core$SpeciesID)

# select replicatess for analysis: chose replicates of June and August sampling, except for Schorfheide sampling in 2009, where sampling took place in June and August. 
#arthropods_core$selector <- with(arthropods_core, (CollectionMonth %in% c("Jun", "Aug") & !(CollectionYear == 2009 & Exploratory == "SCH") | CollectionMonth %in% c("Jul", "Sep") & CollectionYear == 2009 & Exploratory == "SCH" ))

#remove replicates without a second counterpart
#replicates <- ddply(arthropods_core[arthropods_core$selector == TRUE, c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, CollectionYear), summarize, n = length(unique(CollectionMonth)), .drop = TRUE )
#arthropods_core$selector[paste(arthropods_core$PlotID, arthropods_core$CollectionYear) %in% paste(replicates$PlotID, replicates$CollectionYear)[replicates$n != 2]] <- FALSE

## WARNING: Check calculation of annual means: species that have not been reported should have value 0 at each plot and not be treated as NA.  
#arthropods_annual <- ddply(arthropods_core[arthropods_core$selector == TRUE,c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, Species, CollectionYear), summarize, mean = round(sum(NumberAdults, na.rm = TRUE)/2 ,4), .drop = TRUE )


arthropods_core <- read.csv2("data/arthropods_core/Arthropods_GL_0813.csv")
predators_core <- subset(arthropods_core, Feeding_guild == "c")
herbivores_core <- subset(arthropods_core, Feeding_guild %in% c("h","o"))


#######################################

arthropod_traits <- read.csv("data/arthropod_traits/ArthropodSpeciesTraits.csv", sep = ";")
# Original file: ArthropodSpeciesTraits.csv   from   Gossner MM, Simons NK, Achtziger R, Blick T, Dorow WHO, Dziock F, Köhler F, Rabitsch W, Weisser WW (2015) Data from: A summary of eight traits of Coleoptera, Hemiptera, Orthoptera and Araneae, occurring in grasslands in Germany. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.53ds2

lepidoptera_traits <- read.csv("data/arthropod_traits_c_westphal/butterfly_traits.csv", sep = ";")
cicadomorpha_traits <- read.csv("data/arthropod_traits_c_westphal/cicadomorpha_traits.csv", sep = ";")
coleoptera_traits <- read.csv("data/arthropod_traits_c_westphal/coleoptera_traits.csv", sep = ";")


arthropod_traits$Stratum_use_numeric <- c(1,2,3,4,2.5,NA)[match(arthropod_traits$Stratum_use_short, c("s", "g", "h", "t", "u", "w"))]
arthropod_traits$Feeding_mode_numeric <- c(1,2,NA)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]

herbivores <- subset(arthropods_core, Species %in% arthropod_traits$Species[arthropod_traits$Feeding_guild_short %in% c("h")])
consumers <- subset(arthropods_core, Species %in% arthropod_traits$Species[arthropod_traits$Feeding_guild_short %in% c("c")])
