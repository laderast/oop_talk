library(R6)
library(dplyr)

R6::R6Class(classname = "StatPackageResult",
            public = list(
              
              #this is what we use to initialize our object
              initialize = function(data, statistics){
                self$data <- data
                self$statistics <- statistics
                invisible(self)
              },
              
              #define our slots here
              data = NULL,
              statistics = NULL,
            
              get_statistics = function(){
                  return(self$statistics)
              },
              
              get_threshold = function(){
                private$threshold
              },
            
              set_threshold = function(threshold){
                private$threshold <- threshold
              }
              
              get_significant_results = function(){
                
                  if(is.null(self$threshold)){
                    stop("Threshold is not set")
                  }
                
                  filtered_results <- self$statistics %>%
                    filter(pvalue < threshold)
                  
                  filtered_results
                  
              }),
            
            private=list(
              threshold=NULL
              )
            )