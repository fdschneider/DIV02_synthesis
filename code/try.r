
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
try_names <- names(try_ids)

## replace indices for accepted TRY ID
try_ids <- lapply(try_ids, function(x) try_species$AccSpeciesID[x])

## return list of species for TRY database request
#cat(na.exclude(unlist(try_ids)), sep = ", ")




# select trait IDs for TRY database request

try_traits <- read.table("data/plants_traits_TRY/Try_traits.txt", sep = "\t", header = TRUE, skip = 3)

# Diaz et al 2016: 
# plant height, stem specific density, leaf area, leaf mass per area, leaf nitrogen content per unit mass, diaspore mass 
#ids <- c(18, 4, 1, 55 ,14, 26)

# Petes data:
# Mycorrhizal.intensity,	SLA, leafPpermass, 	leafNpermass, 	LDMC  lead dry matter content
# ids <- c(11,55,14,) 

# Sagliero Gomez

# Request:

request_id <- c(
1, # - Specific leaf area  [1] 
4, # - stem specific density [4]
55, # - leaf mass per area [55]
14, # - leaf nitrogen per mass [14]
26, # - seed dry mass  [26] 
18, # - plant height  [18]
#22, # - photosynthetic pathway (factorial)  [22]
#42, # - plant growth form (factorial)  [42]
#12, # - plant lifespan [12]
8, # - plant nitrogen fixation capacity [8]
#587, # - plant growth rate [587]
29, # - pollination syndrome [29] (explanatory)
6, # - rooting depth [6]
11, # - Mycorrhizal.intensity [11]
679, # - plant palatability [679]
433 # - leafPpermass [433]
#1080# - specific root lenght [1080]
)

subset(try_traits, TraitID %in% request_id)

# trait availability for plants (TRY DB request on 02.06.2016)

try_traitmatch <- read.table("data/plants_traits_TRY/TRY_traitavailability.txt", sep = "\t", header = TRUE, skip = 3)
colnames(try_traitmatch) <- c("AccSpeciesName", request_id)

# plants of BE grasslands not included in TRY traitset

plant_species[!plant_species  %in% try_traitmatch$AccSpeciesName]

try_traitmatch <- subset(try_traitmatch, AccSpeciesName %in% plant_species )
try_traitmatch
