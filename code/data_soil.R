## synthesis project: trait synchrony
## READ IN DATA FOR SOIL ANALYSIS
## Florian D. Schneider
## 07.2016



## bacteria data



IDs <- c(20250, 20251)

ddm <- lapply(IDs, function(ID) read.csv2(paste0("data/soil_2011_2014/", ID, ".txt"), sep = "\t", dec = ".", colClasses = c(rep("factor", 3), rep("numeric", 23))) )
# original data 20250_SSC 2011, EP grassland, microbial soil properties, SCALEMIC_1.1.1; 


# bind rows of two years into one table
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
