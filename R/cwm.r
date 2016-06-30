#' Calculate community weighted means. 
#'
#' @param abundances A data frame containing a column of species names or IDs and a column of abundances or counts.
#' @param traits A vector of trait names to get community weighted means for. 
#' @param trait_table A data frame containing a column of species names and one or several columns of traits. Traits can only be continuous numbers, no factorials. 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' arhropods_annual <- ddply(arthropods_core[,c("PlotID", "Species","CollectionYear","NumberAdults")], .(PlotID, Species, CollectionYear), summarize, mean = round(mean(NumberAdults, na.rm = TRUE),2) )
#' 
#' cwm_arthropods <- ddply(arhropods_annual, .(PlotID,CollectionYear), cwm)
#' 
#' pairs(cwm_arthropods[3:5])
#' 

cwm <- function(abundances, traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"), trait_table = arthropod_traits) {
  temp <- cbind(abundances[,c("Species", "mean")], trait_table[match(abundances$Species, trait_table$SpeciesID),traits])
  
  output <- colSums(temp[,traits]*temp$mean, na.rm = TRUE)/sum(temp$mean, na.rm = TRUE)
  
}