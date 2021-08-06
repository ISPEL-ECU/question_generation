#worked on 6/26/2021
#using the cross2 function from the purrr library to get around data frames

library(set)
library(purrr)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")   

numSets = 2
setSize = 3
dType =1
  
questionText <-('Let A and B be two sets. What is \\$A\\times B\\$?')
  
  
  
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #vectorize each set. 
  for(i in (1:2)){
    sourceSets[[i]] <- unlist(sourceSets[[i]])
  }
  print(sourceSets)
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
  
  correct <- cross2(sourceSets[[1]],sourceSets[[2]])
 
  #uses the formatListAsSet function to format the string located in "utils/format.R".

  counter <- 1
  for (s in correct){
    correct[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  correct <- (toString(correct))
  
  
  
  
  
  
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
    
    
    #set generation for Cartesian product. 
   
    set1 <- (getSets(n = 1, m = (setSize), x = dType))
    set2 <- (getSets(n = 1, m = (setSize), x = dType))
    set1 <- unlist(set1)
    set2 <- unlist(set2)
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
    
    currdistractor <- cross2(set1, set2)
    #uses the formatListAsSet function to format the string located in "utils/format.R".
    
    counter <- 1
    for (s in currdistractor){
      currdistractor[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
    
    currdistractor <- (toString(distractor))
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currdistractor
  }
  
  print(distractors)
  
  for(i in (1:3)) {
    temporarySet <- distractors[[i]]
    temporarySet <- str_replace_all(temporarySet, c("list" = ""))
    temporarySet <- str_replace_all(temporarySet, c(":" = ", "))
    temporarySet <- str_replace_all(temporarySet, c("x = " = ""))
    temporarySet <- str_replace_all(temporarySet, c("y = " = ""))
    temporarySet <- str_replace_all(temporarySet, c('\"' = ""))
    distractors[[i]] <- temporarySet
  }
  
  
  
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  #COpy a
  
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  sourceSets <- insertSetQStrings(sourceSets)
  print(sourceSets)
  
  #formats the cartesian product string correctly.

  correct <- str_replace_all(correct, c("\\}\\$" = ""))
  correct <- str_replace_all(correct, c("x = " = ""))
  correct <- str_replace_all(correct, c("y = " = ""))
  correct <- str_replace_all(correct, c('\"' = ""))
  
  print(correct)
  
  
  #avilk: sets modified in order to have them displayed with the text.
  #sets are being modified in the correct format with the for loop below. 
  

  # sets[[1]] <- str_replace_all(sets[[1]], c("c" = ""))
  # sets[[1]] <- str_replace_all(sets[[1]], c("\\(" = "\\{"))
  # sets[[1]] <- str_replace_all(sets[[1]], c("\\)" = "\\}"))
  # sets[[2]] <- str_replace_all(sets[[2]], c("c" = ""))
  # sets[[2]] <- str_replace_all(sets[[2]], c("\\(" = "\\{"))
  # sets[[2]] <- str_replace_all(sets[[2]], c("\\)" = "\\}"))
    

  # for (i in (1:n)) {
  #   temporarySet <- sets[[i]]
  #   temporarySet <- str_replace_all(temporarySet, c("c" = ""))
  #   temporarySet <- str_replace_all(temporarySet, c("\\(" = "\\{"))
  #   temporarySet <- str_replace_all(temporarySet, c("\\)" = "\\}"))
  #   temporarySet <- str_replace_all(temporarySet, c(":" = ", "))
  #   sets[[i]] <- temporarySet
  # }
  # 
  # print(sets)
  # qstn <- ('Let A and B be two sets. What is \\$A\\times B\\$?')
  # print(qstn)
  # 
  
  #avilk: the question was moved to the end of the list 
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= questionContents, correct= correct, distractors= distractors)
  
  #jsonToSend <- toJSON(toSend)
  
  print(toSend)
  




