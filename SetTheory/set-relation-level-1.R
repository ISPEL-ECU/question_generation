library(set)
library(nsprcomp)
library(sets)
library(rje)
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

get_set_union_level_1 <- function(numSets=2, setSize=5, dType = 1) {
  #define the text of the question
  questionText <-('Let A and B be two sets. What is \\$A\\cup B\\$?')
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- union(sourceSets[[1]], sourceSets[[2]])
  
  
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  # add distractors to the list. 
  # each element of the list should be a set
  # represented as a list.
  
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
  
  
  #formatting for output
  correct <- formatListAsSet(correct) #format for output
  

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

get_set_intersect_level_1 <- function(numSets=2, setSize=5, dType = 1) {
  
  
  #define the text of the question
  questionText <-('Let A and B be two sets. What is \\$A\\cap B\\$?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- intersect(sourceSets[[1]], sourceSets[[2]])
  
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

get_asym_diff_level_1 <- function(numSets=2, setSize=5, dType = 1) {
  
    questionStr <- "Let A and B be two sets. What is A - B?"
    
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    
    #creating the correct answer
    correct <- not(sourceSets[[1]], sourceSets[[2]])
    
    if(length(correct) > 0){
      correct <- correct #format for output
    } else {
      correct <- "\\$\\emptyset\\$"
    }
  
  
  
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  #add distractors to the list. 
  for(i in (1:3)) {
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

    #now we format the sourceSets for output. We waited to do this so we could use
    # the sourceSets for distractor generation.
    
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
get_set_complement_level_1 <- function(numSets = 2, setSize = 9, dType = 1) {
  
  questionText <- "Let A be a set and B be the universal set. What is the complement of set A?"
  
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    sourceSets[[1]] <- sourceSets[[2]]
    #scramble Universal set
    sourceSets[[2]] <- sample(sourceSets[[2]], length(sourceSets[[2]]), replace  = FALSE)
    length(sourceSets[[1]]) <- 5
    
    correct <- not(sourceSets[[2]], sourceSets[[1]])
  
  d1 <- correct
  d2 <- correct
  correct <- formatListAsSet(correct)
  
  distractors <- vector(mode="list", length = 3)
  
  #distractor 1 Is similar to the correct answer, but with one different value
  d1 <- replace(d1, length(d1) - 2, getValue(x = dType, min = 1, max = 20)) 

  #distractor 2 is also similar to the correct answer, but with one replaced value
  d2 <- replace(d2, length(d2), getValue(x = dType, min = 1, max = 20))
  
  #distractor 3 is the original set which is not the complement and is wrong
  d3 <- sourceSets[[1]]
  
  
  distractors[[1]] <- formatListAsSet(d1)
  distractors[[2]] <- formatListAsSet(d2)
  
    distractors[[3]] <- formatListAsSet(d3)
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

get_set_equality_level_1 <- function(numSets = 2, setSize = 5, dType = 1) {
  probability <- sample(1:2, 1, replace = FALSE)
 
    questionText <- "Let A and B be two sets. Are A and B equal?"
    #Hard Coded this as the function only works with two sets at the moment.
    numSets <- 2
    #sets 50/50 probability of generated sets being equal or not.
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    if (probability == 1) {
      #makes 2nd set equal to first and formats correct and incorrect answers.
      sourceSets[[2]] <- sourceSets[[1]]
      correct <- "Equal" #format for output
      distractors <- "Not Equal"
    }
    if (probability == 2) {
      #makes 2nd set equal to first except for one replaced member, and formats answers.
      sourceSets[[2]] <- sourceSets[[1]]
      sourceSets[[2]] <- replace(sourceSets[[2]], 
                                 length(sourceSets[[2]]) - sample(1:4, 1, replace = FALSE), 
                                 getValue(x = dType, min = 1, max = 20, cat = 6))
      correct <- "Not Equal"
      distractors <- "Equal"
      
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
get_set_cardinality_level_1 <- function(numSets = 1, setSize = sample(1:9, 1, replace = FALSE), dType = 1) {
  #define the text of the question
  questionText <-('Let A be a set. What is the cardinality of set A?')
  
  #generate and fill sets
  sourceSet <- getSets(n = numSets, m = setSize, x = dType)

  
  
  
    #creating the correct answer based on length of SourceSet
    correct <- lengths(sourceSet) 
  
  
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
  
    counter <- 1
    for (s in sourceSet){
      sourceSet[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
    #format the the sourceSet as Question Strings
    # "A = {...}"
    sourceSets <- insertSetRStrings(sourceSet)


  
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

get_symm_diff_level_1 <- function(numSets = 2, setSize = 5, dType = 1, difficulty = 1){
  
  #define the text of the question
  questionText <-('Let A and B be two sets. What is the symmetric difference of A and B?')
  

    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    
    #set sourceSet 2 equal to sourceSet 1 and scramble the set, then replace three members.
    sourceSets[[2]] <- sourceSets[[1]]
    
    sourceSets[[2]] <- sample(sourceSets[[2]], length(sourceSets[[2]]), replace  = FALSE)
    
    sourceSets[[2]] <- replace(sourceSets[[2]], length(sourceSets[[2]]) - sample(0:1, 1, replace = FALSE), 
                               getValue(x = dType, min = 21, max = 30, cat = 6))
    sourceSets[[2]] <- replace(sourceSets[[2]], length(sourceSets[[2]]) - sample(0:1, 1, replace = FALSE), 
                               getValue(x = dType, min = 21, max = 30, cat = 6))
    sourceSets[[2]] <- replace(sourceSets[[2]], length(sourceSets[[2]]) - sample(0:1, 1, replace = FALSE), 
                               getValue(x = dType, min = 21, max = 30, cat = 6))
    
    
    #set correct answer as a list of values unique to both sets
    correct <- list()
    correct <- not(sourceSets[[1]], sourceSets[[2]])
    correct <- append(correct, not(sourceSets[[2]], sourceSets[[1]]), after = length(correct))
    
    #Iterate through the sourceSets. format list as Set and insert at the index.
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
  
  
  
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
  sourceSets <- insertSetQStrings(sourceSets)
  
 
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

get_set_partitions_level_1 <- function(numSets = 1, setSize = 5, dType = 1) {
  

    #define the text of the question
    questionText <-('Let A be a set. Which answer represents a correct set partition of set A?')

  
  
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

  
  
  distractors <- vector(mode="list", length = 3)
  
  
  for(i in (1:3)) {
    
      #generate and partition distractor sets
      currentDist <- (getSets(n = 1, m = 5, x = dType))
      firstSet <- sample(sourceSets[[1]], length(sourceSets[[1]]), replace  = FALSE)
      length(firstSet) <- sample(2:4, 1, replace = FALSE)
      secondSet <- not(sourceSets[[1]], firstSet)
      
      firstSet <- replace(firstSet, length(firstSet) - sample(0:2, 1, replace = FALSE),
                            getValue(x = dType, min = 1, max = 20))
      
      firstSet <- formatPartitionAsSet(firstSet)
      secondSet <- formatPartitionAsSet(secondSet)
      wrong <- list()
      # and concatenating both sets inside larger empty list.
      # Wrong variable is created to deal with weird out of bounds issue in R.
      wrong <- c(wrong, firstSet)
      wrong <- c(wrong, secondSet)
      currentDist[[1]] <- wrong
    
   
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  
    #Iterate through the sourceSets. format list as Set and insert at the index.
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
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


# powerSetLevel1() generates and prepares 1 set of a random number of 
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
get_power_set_level_1 <- function(numSets = 1, setSize = 3, dType = 1) {


  #level 1 difficulty only generates random sets with random data type.
  
  #question text
  questionText <- ('What is the power set of A?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  #convert the set into a vector to manipulate better.
  sourceSetsVector <- unlist(sourceSets)
  
  
  #the powerset function takes a vector but prints out a list. 
  correct3 <- powerset(sourceSetsVector)
  
  #formats into a string 
  correct3 <- formatPowerSetListAsSet(correct3)
  print(correct3)
  
  
  
  
  
  #Create a vector that will hold distractors
  #NOTE: we declare the list with vector() here so that
  # we can also declare a length. This prevents R from
  # copying the list every time we add an element
  distractors <- vector(mode = "list", length = 3)
  
  #add distractors to the list.
  # each element of the list should be a set
  # represented as a list.
  for (i in (1:3)) {
    #generate a set using getSets function.
    distractorSourceSet <- getSets(n = numSets, m = setSize, x = dType)
    #convert the set into a vector to manipulate better.
    distractorSourceSetVector <- unlist(distractorSourceSet)
    #the powerset function takes a vector but prints out a list. 
    currentDist <- powerset(distractorSourceSetVector)
    #formats into a string 
    currentDist <- formatPowerSetListAsSet(currentDist)
    #Note the single brackets '[1]' here
    distractors[i] <- currentDist
  }
  
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  #COpy a
  
  counter <- 1
  for (s in sourceSets) {
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  
  
  
  
  
  #format the the sourceSets as Question String.
  # "A = {...}"
  sourceSets <- insertSetRStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <-
    list(content = questionContents,
         correct = correct3,
         distractors = distractors)
  
  print(toSend)

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
get_cartesian_product_level_1 <- function(numSets = 2, setSize = 3, dType = 1) {


  #sets difficulty level, question type, and style of the question type.
  qa_level = 1
  question_type = 1
  question_style = 1
  
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #assigning the sets to variables to manipulate each one
  a <- sourceSets[[1]]
  b <- sourceSets[[2]]
  
    
  question <- cartseian_product_question_bank(qa_level = 1, question_type = question_type, question_style = question_style)
  correct <- cartesian2sets(a, b)
  
  
  
  #Create a vector that will hold distractors
  #NOTE: we declare the list with vector() here so that 
  # we can also declare a length. This prevents R from
  # copying the list every time we add an element
  distractors <- vector(mode="list", length = 3)
  
  # three types of distractors are implemented for lvl 2 difficulty: reverse Sets, flipped cartesian product (for example B X A instead of A X B), and random sets. 
  # add distractors to the list. 
  # each element of the list should be a set
  # represented as a list.
  for(i in  (1:3) ) {
    
    current_distractor_list <- cartseian_product_distractor_bank(qa_level = 2, distractor_type = 3, source_set_1 =  a, source_set_2 =  b, setSize = setSize, dType = dType)
    currentDist <- cartesian2sets(current_distractor_list[[1]], current_distractor_list[[2]])
    currentDist <- formatListAsSet(format_cart_set_list(list_length = length(currentDist), currentDist)) 
    
    distractors[i] <- currentDist
  }
  
  
  
  
  #formats the correct answer 
  correct <- formatListAsSet(format_cart_set_list(list_length = length(correct), correct)) 
  
  
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  #COpy a
  
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  
  #format the the sourceSets as Question String.
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  
  
  
  
  # now we concatenate the question contents together
  questionContents <- c(question, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= questionContents, correct= correct, distractors= distractors)
  
  print(toSend)  


}
