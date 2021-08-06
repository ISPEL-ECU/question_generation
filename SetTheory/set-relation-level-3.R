# Author:         Trevor Strobel, Joel Montano, Christopher A. Wright
# File:           set-Relation.R


library(set)
library(nsprcomp)
library(sets)
library(rje)
library(stringr)
library(ggm)


source("utils/format.R") #set string formatting
source("utils/questions.R")
source("utils/distractors.R")
source("utils/set-generation.R") #set generation



# getSetUnionMC() generates and prepares a number
# of sets as well as 3 "distractors" and 1 
# correct "answer" when considering the union of
# said sets. 
#
# param   numSets     The number of sets to consider
# param   setSize     The number of elements in each set. 
# param   dType       The desired data type for set elements
#                     (1: Ints, 2: Real, 3: Complex, 
#                     4: Char, 5: String, 6: Mixed)               
#
# return  toSend      A json-like object containing the
#                     sets, correct, and incorrect
#                     answers.

get_set_union_level_3 <- function(numSets=2, setSize=5, dType = 1) {
  
  probability <- sample(1:2, 1, replace = FALSE)
  sourceSets <- vector(mode = "list", length = 3)
  Firstsource <- vector(mode = "list", length = 2)
  Secondsource <- vector(mode = "list", length = 2)
  Thirdsource <- vector(mode = "list", length = 2)
  Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:3, 1, replace = FALSE), 
                                 rightBorder = sample(4:6, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
  Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(7:9, 1, replace = FALSE), 
                                  rightBorder = sample(10:13, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
  Thirdsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(14:16, 1, replace = FALSE), 
                                 rightBorder = sample(17:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
  sourceSets[[1]] <- Firstsource[[2]]
  sourceSets[[2]] <- Secondsource[[2]]
  sourceSets[[3]] <- Thirdsource[[2]]
  
  if (probability == 1) {
    #define the text of the question
    questionText <-('Let A, B, and C be three sets. What is A \\cup (B \\cup C)?')
    first <- union(Secondsource[[1]], Thirdsource[[1]])
    correct <- union(first, Firstsource[[1]])
    correct <- sample(correct, length(correct), replace = FALSE)
  }
  if (probability == 2) {
    #define the text of the question
    questionText <-('Let A, B, and C be three sets. What is (A \\cup B) \\cup C?')
    first <- union(Firstsource[[1]], Secondsource[[1]])
    correct <- union(first, Thirdsource[[1]])
    correct <- sample(correct, length(correct), replace = FALSE)
  }
  
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  # add distractors to the list. 
  # each element of the list should be a set
  # represented as a list.
  
  for(i in (1:3)){
    # generate a set
    currentDist <- correct
    currentDist <- sample(correct, replace = FALSE)
    
    
    if(i == 1){ #alter answer by removing an element
      currentDist <- list(currentDist[-(setSize-1)])
    }
    else if(i ==2){ #add an element to the correct answer
      # the issue here is that the "incorrect" element needs to be believable and 
      # also not possible to be in the source sets. 
      currentDist <- list(c(currentDist, getValue(x=dType)))
    }
    else if(i == 3){ #remove another element
      currentDist <- list(currentDist[-(setSize-2)])
    }
    
    
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  
  #formatting for output
  correct <- formatListAsSet(correct) #format for output
  
  sourceSets <- insertSet3Strings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
  
  
  
  
}


# getSetIntersect() generates and prepares n sets
# of m elements, 3 "distractors" and 1 correct answer
# when considering the intersections of said sets.
#
# param   numSets     The number of sets to consider
# param   setSize     The number of elements in each set. 
# param   dType       The desired data type for set elements
#                     (1: Ints, 2: Real, 3: Complex, 
#                     4: Char, 5: String, 6: Mixed)               
#
# return  toSend      A json-like object containing the
#                     sets, correct, and incorrect
#                     answers.

get_set_intersect_level_3 <- function(numSets=2, setSize=5, dType = 1) {
  
  
  #define the text of the question
  questionText <-('Let A and B be two sets. What is \\$A\\cap B\\$?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- intersect(sourceSets[[1]], sourceSets[[2]])
  
  correct <- sample(correct, length(correct), replace = FALSE)
  
  if(length(correct) > 0){
    correct <- formatListAsSet(correct) #format for output
  } else {
    correct <- "\\$\\emptyset\\$"
  }
  
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  #add distractors to the list. 
  for(i in (1:3)){
    currentDist <- correct
    
    #difficulty higher than 1 scrambles lists in output.
    currentDist <- sample(correct, replace = FALSE)
    
    
    if(i == 1){ #alter answer by removing an element
      if(currentDist == "\\$\\emptyset\\$"){
        currentDist <- not(sourceSets[[1]], sourceSets[[2]])
        currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
        
      } else {
        currentDist <- "\\$\\emptyset\\$"
      }
    }
    else if(i ==2){ 
      currentDist <- list(not(sourceSets[[1]], sourceSets[[2]]))
      currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
      
    }
    else if(i == 3){ #remove another element
      currentDist <- list(union(sourceSets[[1]], sourceSets[[2]]))
      currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    }
    
    
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= questionContents, correct= correct, distractors= distractors)
  
  return(toSend)
  
}


# getAsymDiff(n, m) genereates and prepares n sets
# of m integers, 3 "distractors" and 1 correct answer
# when considering the difference of the generated
# sets.
# 
# NOTE: the results reflect A-B where A is the first set in
# 'source' and B is the second set in 'source'

# param   numSets     The number of sets to consider
# param   setSize     The number of elements in each set. 
# param   dType       The desired data type for set elements
#                     (1: Ints, 2: Real, 3: Complex, 
#                     4: Char, 5: String, 6: Mixed)               
#
# return  toSend      A json-like object containing the
#                     sets, correct, and incorrect
#                     answers.

get_asym_diff_level_3 <- function(numSets=2, setSize=5, dType = 1) {
  
    probability <- sample(1:2, 1, replace = FALSE)
    sourceSets <- vector(mode = "list", length = 3)
    Firstsource <- vector(mode = "list", length = 2)
    Secondsource <- vector(mode = "list", length = 2)
    Thirdsource <- vector(mode = "list", length = 2)
    Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:3, 1, replace = FALSE), 
                                   rightBorder = sample(5:6, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
    Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(8:9, 1, replace = FALSE), 
                                    rightBorder = sample(11:13, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
    Thirdsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(15:16, 1, replace = FALSE), 
                                   rightBorder = sample(18:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
    sourceSets[[1]] <- Firstsource[[2]]
    sourceSets[[2]] <- Secondsource[[2]]
    sourceSets[[3]] <- Thirdsource[[2]]
    
    if (probability == 1) {
      #define the text of the question
      questionStr <-('Let A, B, and C be three sets. What is A - (B - C)?')
      first <- not(Secondsource[[1]], Thirdsource[[1]])
      correct <- not(first, Firstsource[[1]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
    if (probability == 2) {
      #define the text of the question
      questionStr <-('Let A, B, and C be three sets. What is (A - B) - C?')
      first <- not(Firstsource[[1]], Secondsource[[1]])
      correct <- not(first, Thirdsource[[1]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
  
  
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  #add distractors to the list. 
  for(i in (1:3)){
    # generate a set
    currentDist <- correct
    
    if(i == 1){ #alter answer by removing an element
      currentDist <- list(currentDist[-(setSize-1)])
    }
    else if(i ==2){ #add an element to the correct answer
      # the issue here is that the "incorrect" element needs to be believable and 
      # also not possible to be in the source sets. 
      currentDist <- list(c(currentDist, getValue(x=dType)))
    }
    else if(i == 3){ #remove another element
      currentDist <- list(currentDist[-(setSize-2)])
    }
    
    
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  correct <- formatListAsSet(correct)

  


  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  # "C = {...}"
  sourceSets <- insertSet3Strings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionStr, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  return(toSend)
  
}


# getSetCompliment() generates and prepares n sets of m members 
# as well as 3 "distractors" and 1 correct answer.
# The correct answer reflects the compliment of the n sets against the
# universal set.
# 
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#
get_set_complement_level_3 <- function(numSets = 2, setSize = 9, dType = 1) {
  
  questionText <- "Let A be a set and B be the universal set. What is the complement of set A?"
  
  

  sourceSets <- vector(mode = "list", length = 2)
  Firstsource <- vector(mode = "list", length = 2)
  Secondsource <- vector(mode = "list", length = 2)
  Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(5:10, 1, replace = FALSE), 
                                 rightBorder = sample(11:15, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
  Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:4, 1, replace = FALSE), 
                                  rightBorder = sample(16:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
  sourceSets[[1]] <- Firstsource[[2]]
  sourceSets[[2]] <- Secondsource[[2]]
  correct <- not(Secondsource[[1]], Firstsource[[1]])
  
  
  d1 <- correct
  d2 <- correct
  correct <- formatListAsSet(correct)
  
  distractors <- vector(mode="list", length = 3)
  
  #distractor 1 Is similar to the correct answer, but with one different value
  d1 <- replace(d1, length(d1) - 2, getValue(x = dType, min = 1, max = 20)) 
  d1 <- sample(d1, replace= FALSE)
  
  #distractor 2 is also similar to the correct answer, but with one replaced value
  d2 <- replace(d2, length(d2), getValue(x = dType, min = 1, max = 20))
  d2 <- sample(d2, replace= FALSE)
  
  #distractor 3 is the original set which is not the complement and is wrong
  d3 <- sourceSets[[1]]
  d3 <- sample(d3, replace= FALSE)
  
  
  distractors[[1]] <- formatListAsSet(d1)
  distractors[[2]] <- formatListAsSet(d2)
  distractors[[3]] <- d3
  
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}



# getSetEquality() generates and prepares 2 sets of m members 
# as well as 1 "distractor" and 1 correct answer.
# The correct answer is a string which states whether the sets are equal
# or not.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#

get_set_equality_level_3 <- function(numSets = 2, setSize = 5, dType = 1) {
  probability <- sample(1:2, 1, replace = FALSE)
  
    questionText <- "Let A be a set. Which set is not equivalent to set A?"
    
    numSets <- 1
    leftBorder <- sample(2:5, 1, replace = FALSE)
    rightBorder <- sample(7:10, 1, replace = FALSE)
    leftIncl <- TRUE
    rightIncl <- TRUE
    # Generates sourceSet with setNotations function, and alters notation of correct answer
    # so that it is always different than the notation of the sourceSet for a more challenging question.
    sourceSets <- vector(mode = "list", 2)
    answer <- vector(mode = "list", 2)
    probability <- sample(1:3, 1, replace = FALSE)
    
    sourceSets <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                                  rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
    if (probability == 2) {
      answer <- getSetNotations(leftIncl, rightIncl, leftBorder - 1, 
                                rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
      correct <- answer[[2]]
    }
    else if (probability == 3){
      answer <- getSetNotations(leftIncl, rightIncl, leftBorder + 1, 
                                rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
      correct <- answer[[2]]
    }
    else {
      answer <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                                rightBorder - 1, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
      correct <- answer[[2]]
    }
  

    #Create the distractors
    distractors <- vector(mode="list", length = 3)
    
    for(i in (1:3)){
      # Generates distractors with different included or excluded borders to make them
      # different from the correct answer.
      if (difficulty == 2) {
        d1 <- getSetNotations(leftIncl = TRUE, rightIncl = FALSE, leftBorder, 
                              rightBorder, membersType = 1, notation = 1)
        d2 <- getSetNotations(leftIncl = FALSE, rightIncl = FALSE, leftBorder, 
                              rightBorder, membersType = 1, notation = 1)
        d3 <- getSetNotations(leftIncl = FALSE, rightIncl = TRUE, leftBorder, 
                              rightBorder, membersType = 1, notation = 1)
      }
      else if (difficulty > 2) {
        d1 <- getSetNotations(leftIncl = TRUE, rightIncl = FALSE, leftBorder, 
                              rightBorder + 1, membersType = 1, notation = 2)
        d2 <- getSetNotations(leftIncl = FALSE, rightIncl = FALSE, leftBorder - 1, 
                              rightBorder + 1, membersType = 1, notation = 3)
        d3 <- getSetNotations(leftIncl = FALSE, rightIncl = TRUE, leftBorder - 1, 
                              rightBorder, membersType = 1, notation = 1)
      }
      distractors[[1]] <- d1[[2]]
      distractors[[2]] <- d2[[2]]
      distractors[[3]] <- d3[[2]]
    }
    
  


    #format the the sourceSets as Question Strings
    # "A = {...}"
  sourceSets <- insertSetRStrings(sourceSets[[2]])
  
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}

# getSetCardinality() generates and prepares 1 set of a random number of 
# members between 1 and 9, as well as 3 "distractors" and 1 correct answer.
# The correct answer is a string which states the correct cardinality
# of the generated set.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#
get_set_cardinality_level_3 <- function(numSets = 1, setSize = sample(1:9, 1, replace = FALSE), dType = 1) {
  #define the text of the question
  questionText <-('Let A be a set. What is the cardinality of set A?')

  

    probability <- sample(1:2, 1, replace = FALSE)
    leftIncl <- sample(c(TRUE,FALSE), 1, replace = FALSE)
    rightIncl <- sample(c(TRUE,FALSE), 1, replace = FALSE)
    sourceSets <- vector(mode = "list", length = 2)
    Firstsource <- vector(mode = "list", length = 2)
    Secondsource <- vector(mode = "list", length = 2)
    if (probability == 1) {
      questionText <-('Let A be a set and B be a set. What is the total cardinality of sets A and B?')
      Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(5:10, 1, replace = FALSE), 
                                     rightBorder = sample(11:15, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
      Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:4, 1, replace = FALSE), 
                                      rightBorder = sample(16:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
      sourceSets[[1]] <- Firstsource[[2]]
      sourceSets[[2]] <- Secondsource[[2]]
      correct <- lengths(Firstsource[1]) + lengths(Secondsource[1])
    }
    if (probability == 2) {
      questionText <-('Let A be a set and B be a set. Based on the relative cardinalities of A and B, what type of function is A → B?')
      Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(6:7, 1, replace = FALSE), 
                                     rightBorder = sample(15:16, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
      Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(6:7, 1, replace = FALSE), 
                                      rightBorder = sample(15:16, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
      sourceSets[[1]] <- Firstsource[[2]]
      sourceSets[[2]] <- Secondsource[[2]]
      distractors <- vector(mode="list", length = 2)
      if ((lengths(Firstsource[1])) == (lengths(Secondsource[1]))) {
        correct <- "Bijection"
        distractors[[1]] <- "Surjection"
        distractors[[2]] <- "Injection"
      }
      else if ((lengths(Firstsource[1])) < (lengths(Secondsource[1]))) {
        correct <- "Injection"
        distractors[[1]] <- "Surjection"
        distractors[[2]] <- "Bijection"
      }
      else {
        correct <- "Surjection"
        distractors[[1]] <- "Bijection"
        distractors[[2]] <- "Injection"
      }
    }
  
  
  
  
  if (probability == 1) {
    #Creating distractors based on correct answer.
    distractors <- vector(mode="list", length = 3)
    probability <- sample(1:2, 1, replace = FALSE)
    for(i in 1:3) {
      if (probability == 1) {
        distractors[[i]] <- correct - sample(1:3, 1, replace = FALSE)
      }
      if (probability == 2) {
        distractors[[i]] <- correct + sample(1:3, 1, replace = FALSE)
      }
    }
  }
  
  #Iterate through the sourceSet. format list as Set and insert at the index.
  
  
  
  #format the the sourceSet as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}

# getSymmDiff() generates and prepares 2 sets of length 5, 
# as well as 3 "distractors" and 1 correct answer.
# The correct answer is a string which states the unique members of each set
# which constitute the symmetric difference between the two sets.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#

get_symm_diff_level_3 <- function(numSets = 2, setSize = 5, dType = 1){
  
  #define the text of the question
  questionText <-('Let A and B be two sets. What is the symmetric difference of A and B?')
  

  

    #Creates two partitions within a larger set based on the differences between the
    # borders of the sourceSet.
    
    
    #define the text of the question
    questionText <-('Let A, B, and C be three sets. What is the symmetric difference of A, B, and C?')
    sourceSets <- vector(mode = "list", 3)
    SetOne <- list()
    SetTwo <- list()
    SetThree <- list()
    
    leftIncl <- TRUE
    rightIncl <- TRUE
    # Difference variable defines the random difference in both borders between the 1st and 2nd
    # sourceSets so there will always be a symmetric difference.
    difference <- sample(1:2, 1, replace = FALSE)
    leftdifference <- list()
    rightdifference <- list()
    leftBorder <- sample(7:8, 1, replace = FALSE)
    rightBorder <- sample(9:10, 1, replace = FALSE)
    SetOne <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                              rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = TRUE)
    SetTwo <- getSetNotations(leftIncl, rightIncl, leftBorder + difference, 
                              rightBorder + difference, membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = TRUE)
    
    SetThree <- getSetNotations(leftIncl, rightIncl, leftBorder + (difference * 2), 
                                  rightBorder + (difference * 2), membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = TRUE)
    
    sourceSets[[1]] <- SetOne[[2]]
    sourceSets[[2]] <- SetTwo[[2]]
    sourceSets[[3]] <- SetThree[[2]]
    
    # Creates partitions containing the symmetric differences of both sets
    # and appends them to the correct list
    leftdifference <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                                      leftBorder + difference - 1, membersType = 1, notation = 1, format = FALSE)
    rightdifference <- getSetNotations(leftIncl, rightIncl, rightBorder + 1, 
                                       rightBorder + difference, membersType = 1, notation = 1,format = FALSE )
    correct <- list()
    correct <- append(correct, leftdifference[[1]])
    # Creates partitions containing the symmetric differences of both sets
    # and appends them to the correct list
    rightdifferencetwo <- list()
    rightdifferencetwo <- getSetNotations(leftIncl, rightIncl, rightBorder + difference + 1, 
                                          rightBorder + difference + difference, membersType = 1, notation = 1, format = FALSE)
    correct <- append(correct, rightdifferencetwo[[1]])
    
  
  #Creating distractors based on correct answer.
  distractors <- vector(mode="list", length = 3)
  
  for (i in (1:3)) {
    currentDist <- list()
    currentDist[[1]] <- correct
    wrong <- currentDist[[1]]
    wrong <- replace(wrong, length(wrong) - sample(0:1, 1, replace = FALSE), getValue(x = dType, min = 1, max = 30, cat = 6))
    currentDist[[1]] <- wrong
    currentDist <- formatListAsSet(currentDist[[1]])
    distractors[i] <- currentDist
  }
  
  
  correct <- formatListAsSet(correct)
  #format the the sourceSet as Question Strings
  # "A = {...}"
  # "B = {...}"
  # "C = {...}"
  sourceSets <- insertSet3Strings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}


# getSetPartitions() generates and prepares 1 set of length 5, 
# as well as 3 "distractors" and 1 correct answer.
# The correct answer is the set which represents an incorrect partition
# of the sourceSet.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#

get_set_partitions_level_3 <- function(numSets = 1, setSize = 5, dType = 1) {
  
  #define the text of the question
  questionText <-('Let A be a set. Which answer represents an incorrect set partition of set A?')


  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #scrambling the sets to be used for both the correct and distractor partitions.
  initial <- sample(sourceSets[[1]], length(sourceSets[[1]]), replace  = FALSE)
  
  correct <- list()
  
  # Creating a probability variable which will cause the correct answer to be 
  # generated with one of three different partitions for question variety.
  # Setting length of initial variable creates the first partition.
  probability <- sample(1:3, 1, replace = FALSE)
  if (probability == 1) {
    length(initial) <- 3
  }
  else if (probability == 2) {
    length(initial) <- 2
  }
  else {
    length(initial) <- 0
  }
  
  # Then creates the second partition with the remaining members from the source.
  secondSet <- not(sourceSets[[1]], initial)
  

  
  # Formats each inner partition as a set and concatenates each set
  # within the larger list. Then formats the larger list as a set.
  initial <- formatPartitionAsSet(initial)
  secondSet <- formatPartitionAsSet(secondSet)
  correct <- c(correct, initial)
  correct <- c(correct, secondSet)
  correct <- formatListAsSet(correct)
  
 
  # Define sourceSets and generate set with setNotations function.
  sourceSets <- vector(mode="list", length = 1)
  originalSet <- vector(mode="list", length = 2)
  leftBorder <- sample(10:11, 1, replace = FALSE)
  rightBorder <- sample(19:20, 1, replace = FALSE)
  PartitionLength <- sample(2:5, 1, replace = FALSE)
  originalSet <- getSetNotations(leftIncl = TRUE, rightIncl = TRUE, leftBorder, 
                                 rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = TRUE)
  sourceSets[[1]] <- originalSet[[2]]
  # Set correct variable as list that contains an incorrect partitioning of the sourceSet.
  # Then append to the correct list and format.
  correct <- vector(mode = "list", length = 1)
  firstPartition <- getSetNotations(leftIncl = FALSE, rightIncl = TRUE, leftBorder, 
                                    rightBorder - PartitionLength, membersType = 1, notation = 1, format = FALSE)
  secondPartition <- getSetNotations(leftIncl = TRUE, rightIncl = FALSE, rightBorder - PartitionLength + 1, 
                                     rightBorder, membersType = 1, notation = 1, format = FALSE)
  correct[[1]] <- append(correct[[1]], firstPartition[[2]])
  correct[[1]] <- append(correct[[1]], secondPartition[[2]])
  correct[[1]] <- formatListAsSet(correct[[1]])
  
  
  distractors <- vector(mode="list", length = 3)
  
  
  for(i in (1:3)) {
  
    
    #generate distractor partitions and notations, and append to currentDist
    currentDist <- vector(mode = "list", length = 1)
    DistractorPartitionLength <- sample(2:9, 1, replace = FALSE)
    firstDistractorPartition <-getSetNotations(leftIncl = TRUE, rightIncl = TRUE, leftBorder, 
                                               rightBorder - DistractorPartitionLength, membersType = 1, notation = 1, format = FALSE)
    secondDistractorPartition <- getSetNotations(leftIncl = TRUE, rightIncl = TRUE, rightBorder - DistractorPartitionLength + 1, 
                                                 rightBorder, membersType = 1, notation = 1, format = FALSE)
    currentDist[[1]] <- append(currentDist[[1]], firstDistractorPartition[[2]])
    currentDist[[1]] <- append(currentDist[[1]], secondDistractorPartition[[2]])
    
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  

  #format the the sourceSet as Question Strings
  # "A = {...}"
  sourceSets <- insertSetRStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}


# powerSetLevel2() generates and prepares 1 set of a random number of 
# members between 1 and 9, as well as 3 "distractors" and 1 correct answer.
# The correct answer is a string which states the correct cardinality
# of the generated set.
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
get_power_set_level_3 <- function(numSets = 1, setSize = 3, dType = 6) {


  qa_level = 2
  question_type = sample(1:2, 1)
  question_style = sample(1:2, 1)
  
  
  #assigning the borders to determine the length of the set
  leftBorder = sample(1:100, 1)
  rightBorder = leftBorder + setSize
  
  #generate and fill sets
  sourceSets <- getSetNotations(leftIncl = TRUE, rightIncl = TRUE, leftBorder = leftBorder, rightBorder = rightBorder, membersType = 1, notation = 2, format = TRUE)
  
  #assinging the set to a variable to manipulate below
  a <- sourceSets[[2]]
  
  
  
  
  #logic that assigns the question text and calculates the correct answer.
  if (question_type == 1) {
    
    question <- power_set_question_bank(qa_level = 2, question_type = question_type, question_style = question_style)
    correct <- powerset(a)
    correct <- formatPowerSetListAsSet(correct)
    
    
  } else if (question_type == 2) {
    
    question <- power_set_question_bank(qa_level = 2, question_type = question_type, question_style = question_style)
    correct <- power_set_cardinality_format(input_list = a, cardinality_style = sample((1:2), 1))
    correct <- toString(correct)
  }  
  
  #Create a vector that will hold distractors
  #NOTE: we declare the list with vector() here so that
  # we can also declare a length. This prevents R from
  # copying the list every time we add an element
  distractors <- vector(mode = "list", length = 3)
  
  #add distractors to the list.
  # each element of the list should be a set
  # represented as a list.
  for (i in (1:3)) {
    currentDist <- power_set_distractor_bank(qa_level = qa_level, question_type = question_type, distractor_type = i, distractor_style = sample(1:2, 1), source_set_1 = a, setSize = setSize, dType = dType)
    
    
    #power_set_distractor_bank returns a list of values and index 1 of the list has the value we care for.
    distractors[i] <- currentDist[[1]]
  }
  
  
  
  #format the the sourceSets as Question String.
  # "A = {...}"
  formattedSet <- insertSetRStrings(sourceSets[[1]])
  
  # now we concatenate the question contents together
  questionContents <- c(question, formattedSet)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  return(toSend)



}


#   @param      numSets         The number of sets to consider in the question
#   @param      setSize         The length of the source sets.
#   @param      dType           The desired data type for set elements
#                               (1: Ints, 2: Real, 3: Complex, 
#                               4: Char, 5: String, 6: Mixed)  
#   @param      difficulty     The level of difficulty of the question generated.
#
#   @return     toSend          A json-like object containing the
#                               source sets, correct, and 
#                               distractors (incorrect answers)
#  
get_cartesian_product_level_3 <- function(numSets = 2, setSize = 3, dType = 1) {



  #generates and fills a list of 3 lists (the sets that we will use). 
  sourceSets <- getSets(n = 3, m = 3, x = 1)
  
  #assign each set to a variable.
  a <- sourceSets[[1]]
  b <- sourceSets[[2]]
  c <- sourceSets[[3]]
  
  
  #determines which level/type/style of questions being asked.
  question_type <- sample(1:5, 1)
  question_style <- sample(1:2, 1)
  qa_level = 3 
  
  
  
  #conditions that check which question is being asked
    
  if (question_type == 1) {
  
    question <- cartseian_product_question_bank(qa_level = 3, question_type = question_type, question_style = question_style)
    correct <- cartesian2sets(a, intersect(b, c))
   
    
    
  } else if (question_type == 2) {
  
    question <- cartseian_product_question_bank(qa_level = 3, question_type = question_type, question_style = question_style)
    correct <- cartesian2sets(a, union(b, c))
   
    
  } else if (question_type == 3) {
    
    question <- cartseian_product_question_bank(qa_level = 3, question_type = question_type, question_style = question_style)
    correct <- cartesian2sets(intersect(a, b), c)
  
    
  } else if (question_type == 4) {
    
    question <- cartseian_product_question_bank(qa_level = 3, question_type = question_type, question_style = question_style)
    correct <- cartesian2sets(union(a, b), c)
   
    
  } else if (question_type == 5) {
   
    question <- cartseian_product_question_bank(qa_level = 3, question_type = question_type, question_style = question_style)
    correct <- cartesian3sets(a, b, c)
  
    
  }
   
  
  #Create a vector that will hold distractors
  #NOTE: we declare the list with vector() here so that
  # we can also declare a length. This prevents R from
  # copying the list every time we add an element
  distractors <- vector(mode = "list", length = 3)
  
  
  #getting a distractor 
  
  for (i in (1:3) ) {
    #generate and fill sets.
    distractorSet <- getSets(n = 3, m = 2, x = 1)
    #assign each set to a variable.
    x <- distractorSet[[1]]
    y <- distractorSet[[2]]
    z <- distractorSet[[3]]
    
    #gets cartesian product and formats it as well. 
    currentDist <- cartesian3sets(x, y, z)
    currentDist <- formatListAsSet(format_cart_set_list(list_length = length(currentDist), currentDist)) 
  
    
    #Note the single brackets '[1]' here
    distractors[i] <- currentDist
    
  }
  
  
  #formats the correct answer 
  correct <- formatListAsSet(format_cart_set_list(list_length = length(correct), correct)) 
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  #COpy a
  counter <- 1
  for (s in sourceSets) {
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  #format the the sourceSets as Question String.
  # "A = {...}"
  # "B = {...}"
  # "C = {...}"
  sourceSets <- insertSet3Strings(sourceSets)
  
  
  
  # now we concatenate the question contents together
  questionContents <- c(question, sourceSets)
  
  #format answers and sources into json and return results
  toSend <-list(content = questionContents, correct = correct, distractors = distractors)
  
  #jsonToSend <- toJSON(toSend)
  return(toSend)

}


