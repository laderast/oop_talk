library(dplyr)

#S4 object definition
StatPackageResult <- setClass("StatPackageResult",
                       slots = c(data = "data.frame",
                                 statistics = "data.frame")
                       )

#always need to define the generic method first
setGeneric(name= "get_statistics", 
           def= function(object) {
             standardGeneric("get_statistics")
             }
           )

#define the function
setMethod(f="get_statistics",
          #signature is how the function is called
          signature = signature("StatPackageResult"), 
          definition = function(object){
            return(object@statistics)
          })

#making a generic with two arguments
setGeneric(name = "get_significant_results",
           def = function(object, cutoff){
             standardGeneric("get_significant_results")
             
           })

#This is our method for our StatPackageResult class
setMethod("get_significant_results", 
          signature = signature(object="StatPackageResult", cutoff="numeric"),
          definition = function(object, cutoff){
            
            if(cutoff < 0 | cutoff > 1){
              stop("Your threshold makes no sense")
            }
            
            filtered_results <- data.frame(object@statistics) %>%
              filter(pvalue < cutoff)
            
            return(filtered_results)
          })

#checking things about our class
check_stats_object <- function(object){
  #check whether statistics data.frame contains "pvalue"
  #as column name
  if("pvalue"  %in%  colnames(get_statistics(object))) {TRUE}
  else{ "statistics slot needs a pvalue column"}
}

setValidity(Class = "StatPackageResult", method=check_stats_object)


#Inheritance: making a new class from our old class
AnovaResult <- setClass("AnovaResult", 
                        contains= "StatPackageResult")
