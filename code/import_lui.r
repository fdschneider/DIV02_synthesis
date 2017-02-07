# import LUI data

lui <- read.csv2("data/land_use/LUI_reg_sep_19.10.2016+115351.txt", sep = "\t") 
lui$Year <-  lui$Std_procedure.year.
levels(lui$Year) <- 2008:2015
lui$Exploratory <-  lui$Std_procedure.exploratory.
levels(lui$Exploratory) <- c("ALB", "HAI", "SCH")

save(lui, file = "data/lui.rData")

# clean temporary objects
#rm(lui)
