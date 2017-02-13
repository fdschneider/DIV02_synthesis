# import environmental data

# these data have been assembled by Pete Manning and include plot level
# elevation, slope, aspect, soil profile, litter layer C and N, soil type,
# topographic wetnes index (TWI) as calculated from a DEM, and more. I will use the main parameters
# to test for an effect of water runoff/collection and soil type/profile.
#
# I assume this is a merged dataset from Bexis datasets:
# -  6223 : Soil: CN gridplots
# -  6226 : Soil: pH-analysis grid plots
# -  10022 : Digital Terrain Models of all Exploratories
#
# The data contain all plots, thus need to be filtered for EPs and matched EP IDs. 

topo <- read.csv2("data/environment/topo_data.txt", sep = "\t", dec = ".") 
soil <- read.csv2("data/environment/soil_data.txt", sep = "\t", dec = ".") 

# to match EP_PlotIDs and GP IDs I get this table from Bexis dataset 20826 "Basic Information of all Experimental Plots (EPs)_1.7.5  
# NOTE: The topographic dataset lacks a matching EP set for Plot AEG10, which is one of the plots which has been moved. I need to extract the topographic data for this plot from the GP data or ask the maintainer. 

IDs <- read.csv2("data/environment/20826.txt", sep = "\t")
IDs <- subset(IDs, Landuse == "G" & ActivePlot == "yes")

topo_soil <- cbind(EP_PlotID =  IDs$EP_PlotID, 
                   soil[soil$PlotID %in% IDs$PlotID, ],
                   topo[match(IDs$PlotID, topo$PlotID), ]
                   )[,c("EP_PlotID", "Exploratorium", "pH", "elevation", "slope", "TWI", "HLI")]


save(topo_soil, file = "data/topo_soil.rData")

