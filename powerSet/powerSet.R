# Author:           Joel
# Date:             sys.date()

library(set)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")    #set generation
library('rje')


# Set Theory Multiple Choice Template. This example uses the Set Union problem.
#
#   @param      numSets         The number of sets to consider in the question
#   @param      setSize         The length of the source sets.
#   @param      dType           The desired data type for set elements
#                               (1: Ints, 2: Real, 3: Complex, 
#                               4: Char, 5: String, 6: Mixed)  
#
#   @return     toSend          A json-like object containing the
#                               source sets, correct, and 
#                               distractors (incorrect answers)
#
powerSetFunction <- function(numSets = 1, setSize = 3, dType = 1){
  #define the text of the question
  
  
  questionText <-('What is the power set of A?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #list don't play nice in powerSet function so went ahead and unlisted it.
  sourceSets <- unlist(sourceSets)
  
  #creating the correct answer
  #NOTE: this will change based on the desired algorithm,
  #       but the answer should be stored as a list
  # 
  # Here, the union is used as an example
  
  
  
  correct <- powerSet(sourceSets, setSize)
  
  #converted it back to list to play nice with the rest of the functionality
  correct <- list(correct)
  correct <- formatListAsSet(correct[[1]]) #format for output
  
  
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
    currentDist <- powerSet(currentDist, setSize)
    currentDist <- list(currentDist)
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  sourceSets <- formatListAsSet(sourceSets)
  
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetRStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  print(toSend)
  #return question info
  return(toSend)
}


x <- powerSetFunction()
print(x)
