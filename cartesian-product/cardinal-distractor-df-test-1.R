library(set)
library(tidyr)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")    #set generation

numSets = 2 
setSize = 5 
dType = 1


# code below is from cardinal-distractor-test-1

for(i in (1:3)){
  
  
  #generate a set
  #find the length and product of 4 sets (will need to figure out way to generate automatically given a different n)
  distractorSet <- list()
  #set generation for Cartesian product. 
  setGen1 <- c(getSets(n = 1,  
 m = (setSize), x = dType))
  setGen2 <- c(getSets(n = 1, m = (setSize), x = dType))
  
  #gets length for each set. 
  gen1Length = length(setGen1)
  gen2Length = length(setGen2)
  
  #generates Cartesian product using length. 
  correctCartesianDistractorLength = gen1Length * gen2Length
  
  
  #creates a list to store pairs of Cartesian product. 
  cartesianPairDistractor <- list()
  #creates data frame using two sets, which inherently gets cartesian product. 
  cartesianDistractors.df <- merge(setGen1, setGen2)
  #converts df to a numeric vector also populates a 'cartesianSet' with numeric pairs. 
  for(e in (1:correctCartesianDistractorLength)) {
    cartesianPairDistractor[[e]] <- as.numeric(cartesianDistractors.df[e, ])
  }
  
  currentDist <- cartesianPairDistractor
  
  currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
  
  
  
  #Note the single brackets '[1]' here 
  distractors[i] <- currentDist
}

print(distractors)