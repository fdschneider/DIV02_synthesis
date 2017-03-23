## synthesis project: trait synchrony
## READ IN DATA FOR SOIL ANALYSIS
## Florian D. Schneider
## 07.2016



# bacteria data

IDs <- c(20250, 20251)

ddm <- lapply(IDs, function(ID) read.csv2(paste0("data/soil_2011_2014/", ID, ".txt"), sep = "\t", dec = ".", colClasses = c(rep("factor", 3), rep("numeric", 23))) )
# original data 20250_SSC 2011, EP grassland, microbial soil properties, SCALEMIC_1.1.1; 


# combine rows of two years into one table
microbial <- ddm[[1]]
for(i in 2:length(IDs)) {
  microbial <- rbind(microbial, ddm[[i]])
}

# calculate gram negative to positive ratio
microbial$Ratio_neg_pos <- microbial$gram_negative/microbial$gram_positive

names(microbial)

#pairs(microbial[,c("Ratio_Cmic_Nmic", "Ratio_neg_pos", "fungi_bacteria", "Invertebrates", "Ergosterol")], pch = 20, col = microbial$Year)

microbial$Region <- lui$Exploratory[match(microbial$EP_Plot_ID, lui$EP.Plotid)]

microbial <- microbial[,c("Year", "EP_Plot_ID", "Region", "Ratio_Cmic_Nmic", "Ratio_neg_pos","fungi_bacteria", "Invertebrates")]

save(microbial, file = "data/microbial.rData")

rm(microbial, ddm, IDs)

# Runa checks data for 2014
# IDs <- c(20246, 20247)
# ddm <- lapply(IDs, function(ID) read.csv2(paste0("data/soil_2011_2014/", ID, ".txt"), sep = "\t", dec = ".") )
# # original data 20250_SSC 2011, EP grassland, microbial soil properties, SCALEMIC_1.1.1; 
# 
# enzyme <- ddm[[1]]
# for(i in 2:length(IDs)) {
#   enzyme <- rbind(enzyme, ddm[[i]])
# }

# enzyme <- read.csv2(paste0("data/soil_2011_2014/", 20246, ".txt"), sep = "\t", dec = ".")
#   
# names(enzyme)
# pairs(enzyme[,c("Urease","beta_Glucosidase","Xylosidase","N_Acetyl_beta_Glucosaminidase","Phosphatase","DEA_with_C2H2", "DEA_without_C2H2" ,"DEA_N2","DEA_N2O_N2O_plus_N2")], pch = 20, col = microbial$Year)
# 
# enzyme$Region <- lui$Exploratory[match(enzyme$EP_Plot_ID, lui$EP.Plotid)]
# 
# save(enzyme, file = "data/enzyme.rData")


# fungal data

ddf <- read.csv2("data/fungi/21048.txt", sep = "\t", dec = ".")
# original data 221048_EP-Grassland abundant Soil fungi from Soil Sampling Campain 2011_1.1.3; Kezia Goldmann & Tesfaye Wubet, accessed on 22.02.2017 

ddf <- subset(ddf, species != "unclassified")
levels(ddf$function.)
ddf$Year <- 2011

fungi_trait_matrix <- ddply(ddf, .(species), summarize, 
                            saprotroph = mean(function. == "Saprotroph"), 
                            arbuscular_m = mean(function. == "AM"), 
                            ecto_m =  mean(function. == "EcM"), 
                            pathogen = mean(function. %in% c("Pathogen", "Plant pathogen")), 
                            parasitic = mean(function. %in% c("Parasite", "Mycoparasite" ))  
                            )

subset(fungi_trait_matrix, !saprotroph %in% c(1,0) )
subset(fungi_trait_matrix, !arbuscular_m %in% c(1,0) )
subset(fungi_trait_matrix, !ecto_m %in% c(1,0) )
subset(fungi_trait_matrix, !pathogen %in% c(1,0) )
subset(fungi_trait_matrix, !parasitic %in% c(1,0) )

save(fungi_trait_matrix, file = "data/fungi_trait_matrix.RData")

fungi <- ddf[,c("Plotid", "Year", "species", "Abundance")]

save(fungi, file = "data/fungi.RData")
