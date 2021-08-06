#worked on 6/23/2021

library(set)
library(tidyr)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")    #set generation

numSets = 2 
setSize = 3 
dType = 1

#Create the distractors
#NOTE: we declare the list with vector() here so that 
# we can also declare a length. This prevents R from
# copying the list every time we add an element



distractors <- vector(mode="list", length = 3)

# add distractors to the list. 
# each element of the list should be a set
# represented as a list.

# NOTE: this example is only to show how to generate and add sets to the requisite
# data structures. Any implementation of this template should also implement
# good distractor generation. That is, generate believable and valid distractors.
# This implementation generates random distractors that also have a chance
# at being the correct answer. That should not be possible.



for(i in (1:3)){
  

  #generate a set
  #find the length and product of 4 sets (will need to figure out way to generate automatically given a different n)
  distractorSet <- list()
  
  #set generation for Cartesian product. 
  setGen1 <- (getSets(n = 1, m = (setSize), x = dType))
  setGen2 <- (getSets(n = 1, m = (setSize), x = dType))

  #changes list to vector to make code work
  setGen1 <- unlist(setGen1)
  setGen2 <- unlist(setGen2)
 
  #gets length for each set. 
  gen1Length = length(setGen1)
  gen2Length = length(setGen2)
  
  #gets length for Cartesian product. 
  correctCartesianDistractorLength = gen1Length * gen2Length

  
  #creates a list to store pairs of Cartesian product. 
  cartesianPairDistractor <- list()
  
  #creates data frame using two sets, which inherently gets cartesian product. 
  cartesianDistractors.df <- merge(setGen1, setGen2)
 
  #converts df to a vector also populates a 'cartesianSet' with pairs. 
  for(e in (1:correctCartesianDistractorLength)) {
    cartesianPairDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
  }
  

  
  currentDist <- cartesianPairDistractor
  
  

  currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R

  
  




  #Note the single brackets '[1]' here 
  distractors[i] <- currentDist
}

print(distractors)

for(i in (1:3)) {
  temporarySet <- distractors[[i]]
  temporarySet <- str_replace_all(temporarySet, c("list" = ""))
  temporarySet <- str_replace_all(temporarySet, c("\\(" = "\\{"))
  temporarySet <- str_replace_all(temporarySet, c("\\)" = "\\}"))
  temporarySet <- str_replace_all(temporarySet, c(":" = ", "))
  temporarySet <- str_replace_all(temporarySet, c("x = " = ""))
  temporarySet <- str_replace_all(temporarySet, c("y = " = ""))
  distractors[[i]] <- temporarySet
}


print(distractors)


#format each string to have right amount of escape characters
#have something to show by friday, if not get Chris. 