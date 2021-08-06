#worked on 6/23/2021

library(set)
library(sets)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")    #set generation


#https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r


numSets = 1
setSize = 3
dType = 1


#define the text of the question
powerSetQA <- function(numSets = 2, setSize = 5, dType = 1) {

  questionText <-('What is the power set of A?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  sourceSets <- unlist(sourceSets)
  
  #creates the actual powerset using hte sets package to do so is called recursively so it's pretty slow. 
  correct <- 2^as.set(sourceSets)
  
  #stringifies the vector
  correct <- toString(correct, width = NULL) 
  
  #formats the vector
  correct <- formatListAsSet(correct[[1]])
  
  
  #format the string correctly
  correct <- str_replace_all(correct, c("list" = ""))
  correct <- str_replace_all(correct, c("\\(" = "\\{"))
  correct <- str_replace_all(correct, c("\\)" = "\\}"))
  
  
   
  
  
  #Create the distractors
  #NOTE: we declare the list with vector() here so that 
  # we can also declare a length. This prevents R from
  # copying the list every time we add an element
  distractors <- vector(mode="list", length = 3)
  
  #add distractors to the list. 
  # each element of the list should be a set
  # represented as a list.
  
  # NOTE: this example is only to show how to generate and add sets to the requisite
  # data structures. Any implementation of this template should also implement
  # good distractor generation. That is, generate believable and valid distractors.
  # This implementation generates random distractors that also have a chance
  # at being the correct answer. That should not be possible.
  for(i in (1:3)){
    #generate a set
    currentDist <- (getSets(n = 1, m = setSize, x = dType))
    currentDist <- unlist(currentDist)
    currentDist <- 2^as.set(currentDist)
    currentDist <- toString(currentDist, width = NULL) 
    currentDist <- formatListAsSet(currentDist[[1]])    #The [[1]] is important here as it removes a layer of abstraction imposed by R
    currentDist <- str_replace_all(currentDist, c("list" = ""))
    currentDist <- str_replace_all(currentDist, c("\\(" = "\\{"))
    currentDist <- str_replace_all(currentDist, c("\\)" = "\\}"))
    
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  #COpy a
  
  sourceSets <- formatListAsSet(sourceSets)
  
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetRStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  return(toSend)

}

x <- powerSetQA
print(x)
