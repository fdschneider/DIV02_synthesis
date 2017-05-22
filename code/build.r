# build data for synthesis project


library(foreach)
library(doSNOW)
library(rgl)
library(vegan)
library(plyr)
library(labdsv)
library(geometry)
library(reshape2)
l_ply(as.list(paste0("R/",dir("R"))), source )

## import data

### plant traits

#source("code/import_plant_traits.r")
load("data/plant_trait_matrix.rData")

### plant abundances
#source("code/import_plant_abundance.r")
load("data/plants_full.rData")

### arthropod traits

#source("code/import_arthropod_traits.r")
load("data/arthropod_trait_matrix.rData")

### predator and herbivore abundances

#source("code/import_arthropod_abundance.r")
load("data/herbivores_core.rData")
load("data/predators_core.rData")



### microbiological properties

#source("code/data_soil.R")
load("data/microbial.rData")
load("data/fungi.rData")

### environmental data: lui

#source("code/import_lui.r")
load("data/lui.rData")
load("data/topo_soil.rData")

## perform calculations 

### correct data structure and get community weighted means

#source("code/weightedmeans.r")
load("data/cwm_plants.rData")
load("data/cwm_herbivores.rData")
load("data/cwm_predators.rData")
load("data/cwm_fungi.rData")



#### pool microbial and fungal data

cwm_soil <- cbind(cwm_fungi[order(cwm_fungi$EP_PlotId),1:7], subset(microbial, Year == "2011")[order(subset(microbial, Year == "2011")$EP_Plot_ID), c("Ratio_Cmic_Nmic", "Ratio_neg_pos", "fungi_bacteria", "Invertebrates", "Region")])

#cwm_soil[,3:7] <- cwm_soil[,3:7]+0.1


### Preview data structure


if(FALSE){
  pairs(log(cwm_plants[3:10]), pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_plants$Region, oma = c(2,9,2,9))
  
  pairs((cwm_herbivores[3:5]), pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_herbivores$Region, oma = c(2,9,2,9))
  
  pairs(log(cwm_predators[3:5]), pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_predators$Region, oma = c(2,9,2,9))
  
  pairs(log(cwm_fungi[3:7]+0.1), pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_fungi$Region, oma = c(2,9,2,9))
  
  pairs((microbial[,c("Ratio_Cmic_Nmic", "Ratio_neg_pos", "fungi_bacteria", "Invertebrates")]), pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = microbial$Region)
  
  
  pairs(cwm_soil[3:11], pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_soil$Region, oma = c(2,9,2,9))
  
}


if(FALSE){
  mfrow3d(2,2)
  with(na.omit(cwm_plants), crystalplot(log(height),log(leaf_N),log(SLA), col = "black", xlab = "plant height", ylab = "leaf N", zlab = "SLA"))
  
  with(na.omit(cwm_herbivores), crystalplot(log(Body_Size),(Dispersal_ability),(Stratum_use_numeric), col = "black", xlab = "body size", ylab = "Dispersal ability", zlab = "Stratum use") )
  
  with(na.omit(cwm_predators), crystalplot(log(Body_Size),(Dispersal_ability),(Stratum_use_numeric), col = "black", xlab = "body size", ylab = "Dispersal ability", zlab = "Stratum use") )
  
  with(na.omit(microbial), crystalplot((Ratio_Cmic_Nmic),(Ratio_neg_pos),(fungi_bacteria), col = "black", xlab = "Cmic/Nmic", ylab = "gram neg/pos", zlab = "fungi/bacteria") )
}

### pool plot replicates


### get principal component axes


## analysis

pdf("figures/PCA.pdf", width = 15, height = 4, useDingbats = FALSE)

par(mfrow = c(1,4), mar = c(4,5,3,1))
# trait_pca(microbial, 
#           traits = c("Ratio_Cmic_Nmic",  "fungi_bacteria", "Invertebrates", "Ratio_neg_pos"), 
#           log = FALSE, cex = 1, i = 2, y = 1) -> pca_microbial
# mtext("Microbes", line = 0.5)

trait_pca(cwm_soil,
          traits = c("saprotroph", "arbuscular_m", "ecto_m", "pathogen", "parasitic","Ratio_Cmic_Nmic",  "fungi_bacteria", "Invertebrates", "Ratio_neg_pos"),
          log = FALSE, cex = 1, i = 1, y = 2) -> pca_soil
mtext("Microbes", line = 0.5)

trait_pca(cwm_plants, 
          traits = c("leaf_N", "leaf_P", "SLA", "leaf_drymass", "seedmass","height", "SSD"), 
          log = FALSE, cex = 1,
          xlim = c(0.8,-0.8)) -> pca_plants
mtext("Plants", line = 0.5)

trait_pca(cwm_herbivores,  choices = c(2,1),
          traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric", "Feeding_suckers", "Feeding_generalist"),
          log = FALSE, cex = 1, 
          xlim = c(0.6, -0.6), ylim = c(1.0,-0.6)) -> pca_herbivores
mtext("Herbivores", line = 0.5)

trait_pca(cwm_predators, choices = c(2,1),
          traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"), 
          log = FALSE, cex = 1, ylim = c(0.8,-0.4)) -> pca_predators
mtext("Predators", line = 0.5)

dev.off()




### structural equation modelling


load("data/sem_input.rData")


library(lavaan)
library(AICcmodavg)
source("http://jarrettbyrnes.info/ubc_sem/lavaan_materials/lavaan.modavg.R")

# Q1 - how to include the land use factors? alternative testing? compound variable. Same for environmental factors. 
# Q2 - Environmental factors: plot-wise, not annually resolved. Are there weather data (precipitation etc) for the plots for each year? Meaning of TWI? slope? elevation? 
# Q3 - how enter region as a covariate in SEM? 
# Q4 - Annual variation also is about synchrony across TL in time. how enter year into SEM? temporal sequence? synchrony of annual variation tested in SEM?  (Should assess variation within plot across years for each PCA). 
# Q5 - missing data in SEM: e.g. predator values for certain plots are missing due to lack of species with trait data. or microbial data only for 2011 and 2014. 
# Q6 - Bacterial groups: which to use? functional interpretation. Use as proportion of ... (analogue to fungi)  Protists: What problems?


dd<- droplevels(dd)
fit6 <-  sem('PC2_plants ~ LUI + TWI
             PC1_plants ~ LUI + TWI
             PC1_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_herbivores ~  PC2_plants
             PC2_predators ~ PC1_herbivores + PC2_herbivores +PC1_plants
             PC1_predators ~ PC2_herbivores', 
             data = dd)


summary(fit6)

### build figures