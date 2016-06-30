# read in data

plants_full <- read.csv2("data/plants_core/plantcover.csv") 

# original file: 19686.xlsx  downloaded from   BExis on 17.05.2016 as Bexis dataset 19686 vegetation relevés EP 2008-2015 1.2.5 ; Maintainer: Markus Fischer

plant_traits <- read.csv("data/plants_traits_TRY/GPgrasslandspeciestraitsFDs.csv")


## read in arthropod core data

IDs <- c(17011, 17012, 17013, 17014, 17015, 17266, 17246, 17247, 17267)


lapply(IDs, function(x) colnames(read.csv2(paste0("data/arthropods_core/", x, ".txt"), sep = "", header = TRUE, nrows = 1 )))

ddf <- lapply(IDs, function(ID) read.csv2(paste0("data/arthropods_core/", ID, ".txt"), sep = "\t") )
# original files: ______,____  downloaded from   BExis on ____ as Bexis dataset ________ ; Maintainer: Martin Goßner

arthropods_core <- ddf[[1]]
for(i in 2:length(IDs)) {
  arthropods_core <- rbind(arthropods_core, ddf[[i]])
}

arthropods_core <- subset(arthropods_core, Species != "<NA>")

plots <- unique(plants_full$EP_PlotId)
levels(arthropods_core$PlotID) <- plots
species <- levels(arthropods_core$Species)


arthropod_traits <- read.csv("data/arthropod_traits/ArthropodSpeciesTraits.csv", sep = ";")
# Original file: ArthropodSpeciesTraits.csv   from   Gossner MM, Simons NK, Achtziger R, Blick T, Dorow WHO, Dziock F, Köhler F, Rabitsch W, Weisser WW (2015) Data from: A summary of eight traits of Coleoptera, Hemiptera, Orthoptera and Araneae, occurring in grasslands in Germany. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.53ds2

levels(arthropod_traits$Stratum_use_short) <- c(1,6,4,2,3,5)
arthropod_traits$Stratum_use_numeric <- as.numeric(arthropod_traits$Stratum_use_short)
