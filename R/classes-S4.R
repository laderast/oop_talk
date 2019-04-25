library(dplyr)

#S4 object definition
StatPackageResult <- setClass("StatPackageResult",
                       slots = c(data = "data.frame",
                                 statistics = "data.frame"),
                       prototype = c(data=NA, statistics=NA)
                       )

#always need to define the generic method first
setGeneric(name= "statistics", 
           def= function(object) {
             standardGeneric("statistics")
             }
           )

#define the function
setMethod(f="statistics",
          #signature is how the function is called
          signature = signature("StatPackageResult"), 
          definition = function(object){
            return(object@statistics)
          })

#making a generic with two arguments
setGeneric(name = "get_significant_results",
           def = function(object, cutoff){
             standardGeneric("get_significant_results")
             
           } )

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
#setValidity()





#Inheritance: making a new class from our old class

