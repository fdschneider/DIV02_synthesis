
source("code/data.r")


# match plant species names from bexis to try database IDs

try_species <- read.table("data/plants_traits_TRY/TryAccSpecies.txt", sep = "\t", header = TRUE)

plant_species <- unique(subset(plants_full, cover != 0)$Species)
plant_species <- sub("_aggr.","",plant_species)
plant_species <- sub("_agg.","",plant_species)
plant_species <- sub("_"," ",plant_species)

## fuzzy matching
#try_ids <- sapply(plant_species, function(x) agrep(x, try_species$AccSpeciesName))

## exact matching
try_ids <- sapply(plant_species, function(x) match(x, try_species$AccSpeciesName))

## review matching:
names(try_ids) <- plant_species
lapply(try_ids, function(x) try_species$AccSpeciesName[x])

## replace indices for accepted TRY ID
try_ids <- lapply(try_ids, function(x) try_species$AccSpeciesID[x])

## return list of species for TRY database request
cat(na.exclude(unlist(try_ids)), sep = ", ")




# select trait IDs for TRY database request

