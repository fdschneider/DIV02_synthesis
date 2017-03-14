
arthropod_traits <- read.csv("data/arthropod_traits/ArthropodSpeciesTraits.csv", sep = ";")
# Original file: ArthropodSpeciesTraits.csv   from   Gossner MM, Simons NK, Achtziger R, Blick T, Dorow WHO, Dziock F, Köhler F, Rabitsch W, Weisser WW (2015) Data from: A summary of eight traits of Coleoptera, Hemiptera, Orthoptera and Araneae, occurring in grasslands in Germany. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.53ds2
# ToDo get data via BExis!!

lepidoptera_traits <- read.csv("data/arthropod_traits_c_westphal/butterfly_traits.csv", sep = ";")
cicadomorpha_traits <- read.csv("data/arthropod_traits_c_westphal/cicadomorpha_traits.csv", sep = ";")
coleoptera_traits <- read.csv("data/arthropod_traits_c_westphal/coleoptera_traits.csv", sep = ";")


arthropod_traits$Stratum_use_numeric <- c(1,2,3,4,2.5,NA)[match(arthropod_traits$Stratum_use_short, c("s", "g", "h", "t", "u", "w"))]
#arthropod_traits$Feeding_mode_numeric <- c(1,2,NA)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]
arthropod_traits$Feeding_generalist <- c(1,2,3)[match(arthropod_traits$Feeding_specialization, c("m", "o", "p"))]
arthropod_traits$Feeding_suckers <- c(0,0,1)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]
arthropod_traits$Feeding_chewers <- c(1,0,0)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]
arthropod_traits$Feeding_extraint <- c(0,1,0)[match(arthropod_traits$Feeding_mode, c("c", "e", "s"))]
arthropod_traits$consumer <- arthropod_traits$Feeding_guild_short == "c"
arthropod_traits$herbivore <- arthropod_traits$Feeding_guild_short == "h"

#arthropod_traits$Feeding_tissue <- c(...)[match(arthropod_traits$Feeding_mode, c("m", "m-p", "m-p-r", "m-p-x", "m-r", "p", "p-r", "p-se", "r", "r-(m-p)", "r-se", "se", "x"))]
traits <- c("Body_Size", "Dispersal_ability", "Feeding_generalist", "Stratum_use_numeric", "Feeding_extraint", "Feeding_chewers", "Feeding_suckers", "consumer", "herbivore")
arthropod_trait_matrix <- cbind(arthropod_traits[,1:5], arthropod_traits[,traits])

save(arthropod_trait_matrix, file = "data/arthropod_trait_matrix.rData")

rm(arthropod_traits, lepidoptera_traits, cicadomorpha_traits, coleoptera_traits)

if(FALSE){
dd_0 <- na.omit(arthropod_traits[,c("Body_Size", "Dispersal_ability", "Stratum_use_numeric", "Feeding_suckers", "Feeding_generalist")]) 

pca_dd <- rda(dd_0, scale = TRUE)
eig_rel <- round(eigenvals(pca_dd)/sum(eigenvals(pca_dd))*100,1)
biplot(pca_dd, scaling = -1, 
       xlab = paste0("PC1 (", eig_rel[1], "%)"),
       ylab = paste0("PC2 (", eig_rel[2], "%)"),
       col = c("#000000", "red")) -> fig
points(fig, "sites", pch = 20, col = "#000000")
text(fig, "species", col="red", cex= 1)
}