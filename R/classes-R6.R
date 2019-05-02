library(R6)
library(dplyr)

StatPackageResultR6 <- R6::R6Class(classname = "StatPackageResultR6",
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
                invisible(self)
              },
              
              get_significant_results = function(){
                
                  if(is.null(private$threshold)){
                    stop("Threshold is not set")
                  }
                
                  filtered_results <- self$statistics %>%
                    filter(pvalue < private$threshold)
                  
                  filtered_results
                  
              }),
            
            private=list(
              threshold=NULL
              )
            )

#Adding a print method to the class
StatPackageResultR6$set(which = "public", name = "print", 
                        value = function() {
                          
                          print(head(self$data))
                          print(head(self$statistics))
                        }
                      )

#Adding a validate method to the class
StatPackageResultR6$set(which = "public", name = "validate", 
                        value = function() {
                          
                          if(!is.data.frame(self$statistics)){
                            warning("statistics field is not a data.frame")
                            return(FALSE)
                          }
                          
                          if(!"pvalue" %in% colnames(self$statistics)){
                            warning("statistics field doesn't contain 
                a pvalue column")
                            return(FALSE)
                          }
                          return(TRUE)
                        }
)

AnovaResultR6 <- R6::R6Class(classname = "AnovaResultR6", 
                             inherit=StatPackageResultR6,
                             public = list(
                                    groups = NULL,
                                    
                                    initialize=function(data, statistics, groups){
                                      self$groups <- groups
                                      super$initialize(data, statistics)
                                      invisible(self)
                                    },
                                    
                                    print = function(){
                                   print(self$groups)
                                   super$print()
                                 })
                             )