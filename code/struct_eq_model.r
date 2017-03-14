# SEM 


plotvec <- paste0(lui[,"Year"],"_",lui[,"EP.Plotid"])

env_dd <- topo_soil[match(rep(topo_soil$EP_PlotID, times = length(levels(lui$Year))), topo_soil$EP_PlotID),]
env_dd$Year <- rep(levels(lui$Year), each = length(unique(topo_soil$EP_PlotID)))
rownames(env_dd) <- paste0(env_dd$Year,"_", env_dd$EP_PlotID)

dd <- data.frame(
  EP = lui$EP.Plotid,
  LUI = lui$LUI,
  #LUI_minusone = lui$LUI[plotvec, paste0((2008:2015)[as.integer(lui[,"Year"])]-1, "_",lui[,"EP.Plotid"])],
  YEAR = lui$Year,
  TWI = env_dd$TWI[match( plotvec, rownames(env_dd))],
  pH = env_dd$pH[match( plotvec, rownames(env_dd))],
  elevation = env_dd$pH[match( plotvec, rownames(env_dd))],
  REGION = lui$Exploratory,
  PC1_plants = pca_plants$CA$u[,1][match( plotvec, rownames(pca_plants$CA$u))],
  PC2_plants = pca_plants$CA$u[,2][match( plotvec, rownames(pca_plants$CA$u))],
  PC1_microbial = pca_microbial$CA$u[,1][match( plotvec, rownames(pca_microbial$CA$u))],
  PC2_microbial = pca_microbial$CA$u[,2][match( plotvec, rownames(pca_microbial$CA$u))],
  PC1_herbivores = pca_herbivores$CA$u[,1][match( plotvec, rownames(pca_herbivores$CA$u))],
  PC2_herbivores = pca_herbivores$CA$u[,2][match( plotvec, rownames(pca_herbivores$CA$u))],
  PC1_predators = pca_predators$CA$u[,1][match( plotvec, rownames(pca_predators$CA$u))],
  PC2_predators = pca_predators$CA$u[,2][match( plotvec, rownames(pca_predators$CA$u))]
)

save(dd, file = "data/sem_input.rData")


library(lavaan)
library(AICcmodavg)
source("http://jarrettbyrnes.info/ubc_sem/lavaan_materials/lavaan.modavg.R")

fit1 <- sem('PC2_plants ~ LUI + TWI 
            PC1_plants ~ LUI + TWI
            PC1_herbivores ~ LUI + PC1_plants + PC2_plants
            PC2_herbivores ~ LUI + PC1_plants + PC2_plants
            PC2_predators ~ PC1_herbivores + PC2_herbivores
            PC1_predators ~ PC1_herbivores + PC2_herbivores', 
            data = dd)


summary(fit1)

mi1 <- modindices(fit1); print(mi1[mi1$mi > 2.0,])

# PC2 of predators affects PC1 of plants  

fit2 <-  sem('PC2_plants ~ LUI + TWI
             PC1_plants ~ LUI  + TWI
             PC1_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_predators ~ PC1_herbivores + PC2_herbivores + PC1_plants
             PC1_predators ~ PC1_herbivores + PC2_herbivores', 
             data = dd)

anova(fit1, fit2)

mi2 <- modindices(fit2); print(mi2[mi2$mi > 2.0,])

# LUI affects PC2_predators

fit3 <-  sem('PC2_plants ~ LUI + TWI
             PC1_plants ~ LUI + TWI
             PC1_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_predators ~ PC1_herbivores + PC2_herbivores + LUI +PC1_plants
             PC1_predators ~ PC1_herbivores + PC2_herbivores', 
             data = dd)

anova(fit3, fit2)

mi3 <- modindices(fit3); print(mi3[mi3$mi > 2.0,])

# model simplification

summary(fit2)


fit4 <-  sem('PC2_plants ~ LUI + TWI
             PC1_plants ~ LUI + TWI
             PC1_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_predators ~ PC1_herbivores + PC2_herbivores + PC1_plants
             PC1_predators ~ PC2_herbivores', 
             data = dd)

anova(fit4, fit2)


summary(fit4)


fit5 <-  sem('PC2_plants ~ LUI + TWI
             PC1_plants ~ LUI + TWI
             PC1_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_herbivores ~ LUI + PC2_plants
             PC2_predators ~ PC1_herbivores + PC2_herbivores +PC1_plants
             PC1_predators ~ PC2_herbivores', 
             data = dd)

anova(fit5, fit4)

summary(fit5)


fit6 <-  sem('PC2_plants ~ LUI + TWI
             PC1_plants ~ LUI + TWI
             PC1_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_herbivores ~  PC2_plants
             PC2_predators ~ PC1_herbivores + PC2_herbivores +PC1_plants
             PC1_predators ~ PC2_herbivores', 
             data = dd)

anova(fit5, fit6)

summary(fit6)


fit7 <-  sem('PC2_plants ~ LUI + TWI
             PC1_plants ~ LUI + TWI
             PC1_herbivores ~ LUI + PC1_plants + PC2_plants
             PC2_herbivores ~  PC2_plants
             PC2_predators ~ PC2_herbivores +PC1_plants
             PC1_predators ~ PC2_herbivores', 
             data = dd)

anova(fit6, fit7)

summary(fit7)


aictab.lavaan(list(fit1, fit2, fit4, fit5, fit6, fit7), 
              c("Model1", "Model2",  "Model4", "Model5", "Model6", "Model7"))
standardizedSolution(fit8, type = "std.all")
