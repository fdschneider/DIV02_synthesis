
arthropod_traits <- read.csv("data/arthropod_traits/ArthropodSpeciesTraits.csv", sep = ";")
# Original file: ArthropodSpeciesTraits.csv   from   Gossner MM, Simons NK, Achtziger R, Blick T, Dorow WHO, Dziock F, Köhler F, Rabitsch W, Weisser WW (2015) Data from: A summary of eight traits of Coleoptera, Hemiptera, Orthoptera and Araneae, occurring in grasslands in Germany. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.53ds2
# ToDo get data via BExis!!

lepidoptera_traits <- read.csv("data/arthropod_traits_c_westphal/butterfly_traits.csv", sep = ";")
cicadomorpha_traits <- read.csv("data/arthropod_traits_c_westphal/cicadomorpha_traits.csv", sep = ";")
coleoptera_traits <- read.csv("data/arthropod_traits_c_westphal/coleoptera_traits.csv", sep = ";")


arthropod_traits$Stratum_use_numeric <- c(1,2,3,4,2.5,NA)[match(arthropod_traits$Stratum_use_short, c("s", "g", "h", "t", "u", "w"))]
#arthropod_traits$Feeding_mode_numeric <- c(1,2,NA)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]
arthropod_traits$Feeding_specialization_numeric <- c(1,2,3)[match(arthropod_traits$Feeding_specialization, c("m", "o", "p"))]
arthropod_traits$Feeding_suckers <- c(0,0,1)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]
arthropod_traits$Feeding_chewers <- c(1,0,0)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]
#arthropod_traits$Feeding_tissue <- c(...)[match(arthropod_traits$Feeding_mode, c("m", "m-p", "m-p-r", "m-p-x", "m-r", "p", "p-r", "p-se", "r", "r-(m-p)", "r-se", "se", "x"))]

arthropod_trait_matrix <- arthropod_traits[,c(1:5,6,7,18,19,20,21)]

save(arthropod_trait_matrix, file = "data/arthropod_trait_matrix.rData")

rm(arthropod_traits, lepidoptera_traits, cicadomorpha_traits, coleoptera_traits)