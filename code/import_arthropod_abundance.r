library(plyr)

# read in arthropod  data

IDs <- c(16871, 16893, 16894, 16895, 16897, 16908, 17006, 17007, 17008, 17009, 17010, 17011, 17012, 17013, 17014, 17015, 17266, 17246, 17247, 17267)

lapply(IDs, function(x) colnames(read.csv2(paste0("data/arthropods_core/", x, ".txt"), sep = "", header = TRUE, nrows = 1 )))

ddf <- lapply(IDs, function(ID) read.csv2(paste0("data/arthropods_core/", ID, ".txt"), sep = "\t") )
# original files: ______,____  downloaded from   BExis on ____ as Bexis dataset ________ ; Maintainer: Martin Goßner


arthropods_core_0 <- ddf[[1]]
for(i in 2:length(IDs)) {
  arthropods_core_0 <- rbind(arthropods_core_0, ddf[[i]])
}

arthropods_core_0 <- subset(arthropods_core_0, Species != "<NA>")

plots <- unique(plants_full$EP_PlotId)
levels(arthropods_core_0$PlotID) <- plots
arthropod_species <- levels(arthropods_core_0$SpeciesID)

# select replicatess for analysis: chose replicates of June and August sampling, except for Schorfheide sampling in 2009, where sampling took place in June and August. 
arthropods_core_0$selector <- with(arthropods_core_0, (CollectionMonth %in% c("Jun", "Aug") & !(CollectionYear == 2009 & Exploratory == "SCH") | CollectionMonth %in% c("Jul", "Sep") & CollectionYear == 2009 & Exploratory == "SCH" ))

#remove replicates without a second counterpart
replicates <- ddply(arthropods_core_0[arthropods_core_0$selector == TRUE, c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, CollectionYear), summarize, n = length(unique(CollectionMonth)), .drop = TRUE )
arthropods_core_0$selector[paste(arthropods_core_0$PlotID, arthropods_core_0$CollectionYear) %in% paste(replicates$PlotID, replicates$CollectionYear)[replicates$n != 2]] <- FALSE

save(arthropods_core_0, file = "data/arthropods_core_0.rData")
## WARNING: Check calculation of annual means: species that have not been reported should have value 0 at each plot and not be treated as NA.  
#arthropods_annual <- ddply(arthropods_core[arthropods_core$selector == TRUE,c("PlotID", "Species","CollectionYear","NumberAdults", "CollectionMonth")], .(PlotID, Species, CollectionYear), summarize, mean = round(sum(NumberAdults, na.rm = TRUE)/2 ,4), .drop = TRUE )


arthropods_core <- read.csv2("data/arthropods_core/Arthropods_GL_0813.csv")
predators_core <- subset(arthropods_core, Feeding_guild == "c")
herbivores_core <- subset(arthropods_core, Feeding_guild %in% c("h"))

# These data have been sent to me directly by Martin Gossner. (TODO: transfer via BExis). They contain the pooled abundance data of arthropod core project samplings in 2008 -- 2013 of all EPs. Extra samplings on VIPs have been omitted to match the sampling of the EP plots. 

save(predators_core, file = "data/predators_core.rData")
save(herbivores_core, file = "data/herbivores_core.rData")
