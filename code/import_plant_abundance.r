#########################################
#     import plant abundance data       #
#########################################

plants_full <- read.csv2("data/plants_core/plantcover.csv") 

# original file: 19686.xlsx  downloaded from BExis on 17.05.2016 as Bexis
# dataset 19686 vegetation relevés EP 2008-2015 1.2.5 ; Maintainer: Markus
# Fischer

plants_full$cover <- as.numeric(plants_full$cover)  # define cover as numeric
levels(plants_full$Year) <- c(2008:2015, 2011)  # fix levels in bexis table

plants_full <- subset(plants_full, Year %in% 2008:2013)  # select years

# remove tree species from list (for extreme trait values assigned to
# seedlings/saplings):

trees <- c("Acer_sp", "Baumkeimling_sp", "Betula_pendula", "Fraxinus_excelsior", 
           "Populus_tremula", "Prunus_avium", "Prunus_sp","Prunus_spinosa", 
           "Quercus_robur", "Tilia_sp")

plants_full <- subset(plants_full, !Species %in% trees, drop = TRUE)
plants_full$Species <- factor(plants_full$Species)  # define species as factorial

# make species names compatible with TRY species names

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

save(plants_full, file = "data/plants_full.rData")

rm(try_species, trees, trees)

#plants_pooledyears <- ddply(plants_full, .(EP_PlotId), summarise, Year = unique(Year), Species = unique(Species), cover = mean(cover), AccSpecies  )
