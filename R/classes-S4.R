library(dplyr)

#return results
statResult <- setClass("stat_package_result",
                       slots = c(data = "data.frame",
                                 statistics = "matrix"
                                 ))

#always need to define the generic method first
setGeneric(name= "get_statistics", 
           def= function(object) {
             standardGeneric("get_statistics")
             }
           )

#define the function
setMethod(f="get_statistics",
          #signature is how the function is called
          signature = "stat_paackage_result", 
          definition = function(object){
            return(object@statistics)
          })

setGeneric(name = "get_significant_results", 
           def = function(object){
             standardGeneric("get_significant_results")
             
           } )

#This is our method for our stat_package_result class
setMethod("get_significant_results", 
          signature = c("stat_package_result", "numeric"),
          definition = function(object, cutoff){
            
            if(cutoff < 0 | cutoff > 1){
              stop("Your threshold makes no sense")
            }
            
            filtered_results <- object@statistics %>%
              filter(pvalue < cutoff)
            
            return(filtered_results)
          })

#checking things about our package
setValidity()