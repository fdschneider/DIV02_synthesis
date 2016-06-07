# read in data

plants_full <- read.csv2("data/plants_core/plantcover.csv") 

# original file: 19686.xlsx  downloaded from   BExis on 17.05.2016 as Bexis dataset 19686 vegetation relevés EP 2008-2015 1.2.5 ; Maintainer: Markus Fischer

plant_traits <- read.csv("data/plants_traits_TRY/GPgrasslandspeciestraitsFDs.csv")

arthropods_core <- read.csv2("data/arthropods_core/16926.txt", sep = "\t")
arthropods_core <- subset(arthropods_core, Traptype == "KEF")

arthropod_traits <- read.csv("data/arthropod_traits/ArthropodSpeciesTraits.csv", sep = ";")
# Original file: ArthropodSpeciesTraits.csv   from   Gossner MM, Simons NK, Achtziger R, Blick T, Dorow WHO, Dziock F, Köhler F, Rabitsch W, Weisser WW (2015) Data from: A summary of eight traits of Coleoptera, Hemiptera, Orthoptera and Araneae, occurring in grasslands in Germany. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.53ds2

plots <- unique(plants_full$EP_PlotId)
levels(arthropods_core$PlotID) <- plots

