#worked on 6/25/2021

library(set)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")   



cartesianProduct <- function(numSets = 2, setSize = 3, dType = 1) {
  
  questionText <-('Let A and B be two sets. What is \\$A\\times B\\$?')
  
  
  
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  # 
  # 
  # #creating an empty list to store original. Elements will be sets.
  # sourceSets <- list()   
  # 
  # 
  # #creating an empty list to store set with cartesian product. Elements will be sets 
  # sets <- list()    
  # 
  # 
  # #for each in a given number of sets, fill the set with ints (1:9). This will generate random set length. 
  # for(e in (1:n)) {
  #   sourceSets[[e]] <- sample(1:9, m, replace = F)
  # }
  # 
  
  
  #finds the correct length/cardinality of the cartesian product set.
  setOneLength = length(sourceSets[[1]])
  setTwoLength = length(sourceSets[[2]])
  cartesianProductCardinality = setOneLength * setTwoLength 
  
  
  #creates a list to store the pairs from the cartesian product. 
  cartesianSet <- list()
  #merge two sets to get a matrix.  
  cartesianSet.df <- merge(sourceSets[[1]], sourceSets[[2]])
  #converts the matrix back to a vector which is populated with the pairs for the cartesian product. 
  for(e in (1:cartesianProductCardinality)) {
    cartesianSet[[e]] <- as.vector(cartesianSet.df[e, ])
  
  }
  
  
  
  for(e in (1:cartesianProductCardinality)) {
    correct[[e]] <- formatListAsSet(correct[[e]])
  }
  
  
  correct <- toString(cartesianSet, width = NULL) 
  #uses the formatListAsSet function to format the string located in "utils/format.R".
  
  #formats the cartesian product string correctly.
  correct <- formatListAsSet(cartesianSet)
  answer <- str_replace_all(answer,c("list" = ""))
  answer <- str_replace_all(answer, c("\\(" = "\\{"))
  answer <- str_replace_all(answer, c("\\)" = "\\}"))
  answer <- str_replace_all(answer, c("x = " = ""))
  answer <- str_replace_all(answer, c("y = " = ""))



  #avilk: sets modified in order to have them displayed with the text.
  #sets are being modified in the correct format with the for loop below. 
  sets <- insertSetQStrings(sourceSets)
 

  # sets[[1]] <- str_replace_all(sets[[1]], c("c" = ""))
  # sets[[1]] <- str_replace_all(sets[[1]], c("\\(" = "\\{"))
  # sets[[1]] <- str_replace_all(sets[[1]], c("\\)" = "\\}"))
  # sets[[2]] <- str_replace_all(sets[[2]], c("c" = ""))
  # sets[[2]] <- str_replace_all(sets[[2]], c("\\(" = "\\{"))
  # sets[[2]] <- str_replace_all(sets[[2]], c("\\)" = "\\}"))
    

  for (i in (1:n)) {
    temporarySet <- sets[[i]]
    temporarySet <- str_replace_all(temporarySet, c("c" = ""))
    temporarySet <- str_replace_all(temporarySet, c("\\(" = "\\{"))
    temporarySet <- str_replace_all(temporarySet, c("\\)" = "\\}"))
    temporarySet <- str_replace_all(temporarySet, c(":" = ", "))
    sets[[i]] <- temporarySet
  }
  
  print(sets)
  qstn <- ('Let A and B be two sets. What is \\$A\\times B\\$?')
  print(qstn)
  
  
  #avilk: the question was moved to the end of the list 
  sets <- c( qstn, sets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= sets, correct= answer, distractors= wrongs)
  
  #jsonToSend <- toJSON(toSend)
  
  return(toSend)
  
  
}


x <- getCartesianProduct()
print(x)



