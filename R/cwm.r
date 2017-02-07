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

cwm <- function(abundances, 
                trait_table = arthropod_traits, 
                traits = colnames(arthropod_traits)[-1], 
                spec_label = "Species", 
                abund_label = "mean", 
                trait_spec_label = "SpeciesID",
                log = FALSE
                ) {
  
   temp <- cbind(abundances[,c(spec_label, abund_label)], trait_table[match(abundances[,spec_label], trait_table[,trait_spec_label]),traits])
    
   output <- colSums(temp[,traits]*temp[,abund_label], na.rm = TRUE)/sum(temp[,abund_label], na.rm = TRUE)
   return(output) 
}
