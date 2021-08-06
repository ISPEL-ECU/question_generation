# Author:               Joel Montano
# File:                 distractor.R
# Date:                 08/5/2021

library(stringr)
source("utils/set-generation.R") #used in the distractor bank for cartesian product.





#function that holds all the distractors for the different difficulty level
#qa_level determines which level the distractors are 
#question_type determines which type of question we'll be generating distractors for
#distractor_type determines which type of distractor do you want 
#source_set_1 to source_set_3 are the sets you pass in to manipulate
#setSize and dType are parameters incase the distractor_type has to generate a matching set to the original source sets.
power_set_distractor_bank <- function(qa_level = NULL, question_type = NULL, distractor_type = NULL, distractor_style = NULL, source_set_1 = NULL, source_set_2 = NULL, source_set_3 = NULL, setSize = NULL, dType = NULL) {
    
    distractor_list = vector(mode = "list", length = 3)
    
    if(qa_level == 2) {
        
        if(question_type == 1) {
            
            if (distractor_type == 1) {
                
                #generate a set using getSets function.
                distractorSourceSet <- source_set_1
                #convert the set into a vector to manipulate better.
                distractorSourceSetVector <- unlist(distractorSourceSet)
                #the powerset function takes a vector but prints out a list. 
                currentDist <- powerset(distractorSourceSetVector)
                #formats into a string 
              
                currentDist <- formatListAsSet(currentDist)
                
                #gets rid of extra fluff power_set function throws in.
                currentDist <- str_replace_all(currentDist, c("list" = ""))
                currentDist <- str_replace_all(currentDist, c('\"' = ""))
                currentDist <- str_replace_all(currentDist, c('c' = ""))
                
                #Note the single brackets '[1]' here
                distractor_list[[1]] <- currentDist
                
            
            }
            
            if (distractor_type == 2) {
                
                #generate a set using getSets function.
                distractorSourceSet <- source_set_1
                #convert the set into a vector to manipulate better.
                distractorSourceSetVector <- unlist(distractorSourceSet)
                #the powerset function takes a vector but prints out a list. 
                currentDist <- powerset(distractorSourceSetVector)
               
                currentDist <- formatListAsSet(currentDist)
                
                
                #gets rid of extra fluff power_set function throws in and adds character to generate wrong Power set format.
                currentDist <- str_replace_all(currentDist, c("list" = ""))
                currentDist <- str_replace_all(currentDist, c("\\(\\), " = ""))
                currentDist <- str_replace_all(currentDist, c("\\(" = "\\\\{"))
                currentDist <- str_replace_all(currentDist, c("\\)" = "\\\\}"))
                currentDist <- str_replace_all(currentDist, c('\"' = ""))
                currentDist <- str_replace_all(currentDist, c('c' = ""))
                
                #Note the single brackets '[1]' here
                distractor_list[[1]] <- currentDist
                
                
            }
            
            if (distractor_type == 3) {
                
                #generate a set using getSets function.
                distractorSourceSet <- source_set_1
                #convert the set into a vector to manipulate better.
                distractorSourceSetVector <- unlist(distractorSourceSet)
                #the powerset function takes a vector but prints out a list. 
                currentDist <- powerset(distractorSourceSetVector)
                
              
                #uses an alternative power set function from the rje package that takes the same value but doesn't generate correct set. 
                currentDist <- powerSetCond(source_set_1, m = sample(1:setSize, 1),rev = FALSE)
                currentDist <- toString(currentDist,  width = NULL)
                
                #uses the formatListAsSet function to format the string located in "utils/format.R".
                currentDist <- formatListAsSet(currentDist)
                
                
                
                
                currentDist <- str_replace_all(currentDist, c("list" = ""))
                currentDist <- str_replace_all(currentDist, c("\\(" = "\\\\{"))
                currentDist <- str_replace_all(currentDist, c("\\)" = "\\\\}"))
                currentDist <- str_replace_all(currentDist, c('\"' = ""))
                currentDist <- str_replace_all(currentDist, c(':' = ", "))
                currentDist <- str_replace_all(currentDist, c('c' = ""))
                
                 
                #Note the single brackets '[1]' here
                distractor_list[[1]] <- currentDist
                
             
            }
        } else {
            
            if (distractor_type == 1) {
                
                if(distractor_style == 1) {
                    #get the length of the original set to coumpound by 3. 
                    currentDist <- 3 ** length(source_set_1)
                    currentDist <- toString(currentDist, width = NULL)
                    
                    
                    #Note the single brackets '[1]' here
                    distractor_list[[1]] <- currentDist
                } else {
                    #style should be added in here
                    
                    #3^length format
                    currentDist <- toString(length(source_set_1), width = NULL)
                    currentDist <- paste("\\$", "3^", length(source_set_1), "\\$")
                    
                    
                    #Note the single brackets '[1]' here
                    distractor_list[[1]] <- currentDist
                }
            }
            
            if (distractor_type == 2) {
                
                if (distractor_style == 1) {
                    #using the length of the original set 'A" to generate a better distractors. 
                    cardinality_source_set_1 <- length(source_set_1)
                    if (cardinality_source_set_1 != 1 & cardinality_source_set_1 != 2) {
                        currentDist <- 2 * cardinality_source_set_1
                        currentDist <- toString(currentDist, width = NULL)
                    } else {
                        currentDist <- 10 * cardinality_source_set_1
                        currentDist <- toString(currentDist, width = NULL)
                        
                    }
                    
                    distractor_list[[1]] <- currentDist 
                } else {
                    #2 x length 
                    cardinality_source_set_1 <- length(source_set_1)
                    currentDist <- toString(cardinality_source_set_1, width = NULL)
                    currentDist <- paste("\\$", "2\\times", currentDist, "\\$")
                    
                    
                    #Note the single brackets '[1]' here
                    distractor_list[[1]] <- currentDist
                }
            }
            
            if (distractor_type == 3) {
                
                if (distractor_style == 1) {
                    #generates a random cardinality not relevant to correct. 
                    currentDist <- sample(30:100, 1)
                    currentDist <- toString(currentDist, width = NULL)
                    
                    
                    #Note the single brackets '[1]' here
                    distractor_list[[1]] <- currentDist
                } else {
                
                    #style    
                    multiplied_number = toString(sample(30:100, 1))
                    
                    #log format.
                    currentDist <-  paste("\\$", multiplied_number, "\\times", length(source_set_1), "\\$")
                    
                    
                    #Note the single brackets '[1]' here
                    distractor_list[[1]] <- currentDist
                }
            }
        }
    }
    
    return(distractor_list)
}





#function that holds all the distractors for the different difficulty level
#qa_level determines which level the distractors are 
#distractor_type determines which type of distractor do you want 
#source_set_1 to source_set_3 are the sets you pass in to manipulate
#setSize and dType are parameters incase the distractor_type has to generate a matching set to the original source sets.
cartseian_product_distractor_bank <- function(qa_level = NULL, question_type = NULL, distractor_type = NULL, source_set_1 = NULL, source_set_2 = NULL, source_set_3 = NULL, setSize = NULL, dType = NULL) {
    
    distractor_list = vector(mode = "list", length = 1)
    
    if(qa_level == 2) {
        
        #reverses the sets using the rev() function and then flips the 1st and 2nd set.
        if (distractor_type == 1) {
            
            distractor_list[[1]] <- rev(source_set_1)
            distractor_list[[2]] <- rev(source_set_2)
            distractor_list[[3]] <- rev(source_set_3)
            
            temp_set <- distractor_list[[1]]
            source_set_1 <- distractor_list[[2]]
            source_set_2 <- temp_set
            distractor_list[[1]] <- source_set_1
            distractor_list[[2]] <- source_set_2
            distractor_list[[3]] <- source_set_3
        }
        
        #flips the 1st and 2nd set. 
        if (distractor_type == 2) {
            
           
            
            temp_set <- source_set_1
            source_set_1 <- source_set_2
            source_set_2 <- temp_set
            distractor_list[[1]] <- source_set_1
            distractor_list[[2]] <- source_set_2
            distractor_list[[3]] <- source_set_3

        }
        
        #keeps the sets normal
        if (distractor_type == 3) {
            
            distractor_list[[1]] <- unlist(getSets(n =1, m = setSize, x = dType))
            distractor_list[[2]] <- unlist(getSets(n =1, m = setSize, x = dType))
            distractor_list[[3]] <- unlist(getSets(n =1, m = setSize, x = dType))
            
        }
    }
    
    
    return(distractor_list)
}
