#worked on 7/7/2021

library(set)
library(sets)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")    #set generation


#https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r




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
powerSetQA <- function(numSets = 1, setSize = 3, dType = 6, difficulty = 2) {
  
  if(difficulty == 1) {
    
      questionText <-('What is the power set of A?')
      
      #generate and fill sets
      sourceSets <- getSets(n = numSets, m = setSize, x = dType)
      
      
      
      #convert the set into a vector to manipulate better. 
      sourceSetsVector <- unlist(sourceSets)
      #how the power set is gotten (uses the 'sets' package).
      #correct <- 2^as.set(sourceSetsVector)
      
      correct <- set_power(sourceSetsVector)
      #converts power set back to a string to format. 
      correct <- toString(correct, width = NULL) 
      #uses the formatListAsSet function to format the string located in "utils/format.R".
      correct <- formatListAsSet(correct[[1]])
      
    
      
      
      #formats the power set string correctly.
      correct <- str_replace_all(correct, c("list" = ""))
      correct <- str_replace_all(correct, c("\\(" = "\\\\{"))
      correct <- str_replace_all(correct, c("\\)" = "\\\\}"))
      correct <- str_replace_all(correct, c("\"" = ""))
      
      
      
      
       
      
      
      #Create a vector that will hold distractors
      #NOTE: we declare the list with vector() here so that 
      # we can also declare a length. This prevents R from
      # copying the list every time we add an element
      distractors <- vector(mode="list", length = 3)
      
      #add distractors to the list. 
      # each element of the list should be a set
      # represented as a list.
      for(i in (1:3)){
        #generate a set using getSets function. 
        currentDist <- (getSets(n = 1, m = setSize, x = dType))
        #makes the list into a vector to better manipulate 
        currentDist <- unlist(currentDist)
        #gets the power set of the vector. 
        #currentDist <- 2^as.set(currentDist)
        currentDist <- set_power(currentDist)
        #converts the power set into a string.
        currentDist <- toString(currentDist, width = NULL) 
        #formats the power set. 
        currentDist <- formatListAsSet(currentDist[[1]])    #The [[1]] is important here as it removes a layer of abstraction imposed by R
        #formats the power set string correctly. 
        currentDist <- str_replace_all(currentDist, c("list" = ""))
        currentDist <- str_replace_all(currentDist, c("\\(" = "\\\\{"))
        currentDist <- str_replace_all(currentDist, c("\\)" = "\\\\}"))
        currentDist <- str_replace_all(currentDist, c('\"' = ""))
        
        
        #Note the single brackets '[1]' here 
        distractors[i] <- currentDist
      }
      
  }    
      
  
  else if (difficulty == 2) {
      
      randomChoice <- sample(1:2,1)
      
      if (randomChoice == 1) {    
        
        questionText <-('What is the power set of A?')
        
        setSize = sample((1:4),1)
        
        #generate and fill sets
        sourceSets <- getSets(n = numSets, m = setSize, x = dType)
        
        
        
        #convert the set into a vector to manipulate better. 
        sourceSetsVector <- unlist(sourceSets)
        #how the power set is gotten (uses the 'sets' package).
        #correct <- 2^as.set(sourceSetsVector)
        correct <- set_power(sourceSetsVector)
        
        #converts power set back to a string to format. 
        correct <- toString(correct, width = NULL) 
        #uses the formatListAsSet function to format the string located in "utils/format.R".
        correct <- formatListAsSet(correct[[1]])
        
        #used for distractor generation. 
        correctDisctractor <- correct 
        
        
        
        #formats the power set string correctly.
        correct <- str_replace_all(correct, c("list" = ""))
        correct <- str_replace_all(correct, c("\\(" = "\\\\{"))
        correct <- str_replace_all(correct, c("\\)" = "\\\\}"))
        correct <- str_replace_all(correct, c("\"" = ""))
        
        
        
        
        
        #Create a vector that will hold distractors
        #NOTE: we declare the list with vector() here so that 
        # we can also declare a length. This prevents R from
        # copying the list every time we add an element
        distractors <- vector(mode="list", length = 3)
        
        #add distractors to the list. 
        # each element of the list should be a set
        # represented as a list.
        for(i in (1:3)){
          
          
          if (i == 1) {
            
            #using a correct set to generate better distractors. 
            currentDist <- correctDisctractor
            
            #gets rid of extra fluff power_set function throws in. 
            currentDist <- str_replace_all(currentDist, c("list" = ""))
            currentDist <- str_replace_all(currentDist, c('\"' = ""))
            
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
            
          } else if (i == 2) {
            
            #using a correct set to generate better distractors. 
            
            currentDist <- correctDisctractor
            
            #gets rid of extra fluff power_set function throws in and adds character to generate wrong Power set format.
            currentDist <- str_replace_all(currentDist, c("list" = ""))
            currentDist <- str_replace_all(currentDist, c("\\(\\), " = ""))
            currentDist <- str_replace_all(currentDist, c("\\(" = "\\\\{"))
            currentDist <- str_replace_all(currentDist, c("\\)" = "\\\\}"))
            currentDist <- str_replace_all(currentDist, c('\"' = ""))
            
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
            
          } else if (i == 3) {
            
            #uses an alternative power set function from the rje package. Will generate distractors that are nearly correct to completely incorrect.
            #
            currentDist <- powerSetCond(sourceSetsVector, m  = sample(1:setSize, 1),  rev = FALSE)
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
            distractors[i] <- currentDist
            
          }
          
        }
        
        
      } else { #for a different type of question involving power sets. 
        
        questionAsked <- sample(1:2, 1)
        
        if (questionAsked == 1) {
          questionText <-('What is the cardinality of the power set of A?')
        } else {
          questionText <-('What is |P(A)|?')
        }
        
        #used to determine how the answer and distractors will be formatted. 
        formatGenerated <- sample(1:2, 1)
        
        setSize = sample((1:4),1)
        
        #generate and fill sets
        sourceSets <- getSets(n = numSets, m = setSize, x = dType)
        
        
        #convert the set into a vector to manipulate better. 
        sourceSetsVector <- unlist(sourceSets)
        
        
        
        if (formatGenerated == 1) {
          #uses the set_power function from 'sets' package.
          
          correct <- set_power(sourceSetsVector)
          
          
          #the correct cardinality of the power set. 
          correct <- length(correct)
          
          #used later for distractor generation.
          correctDisctractor <- correct 
          
          #converts power set back to a string to format. This is one of the 2 formats 
          correct <- toString(correct, width = NULL) 
          
          
          #formats the power set string correctly.
          correct <- str_replace_all(correct, c("list" = ""))
          correct <- str_replace_all(correct, c("\\(" = "\\\\{"))
          correct <- str_replace_all(correct, c("\\)" = "\\\\}"))
          correct <- str_replace_all(correct, c("\"" = ""))
          
          
          
          
          
          
          #Create a vector that will hold distractors
          #NOTE: we declare the list with vector() here so that 
          # we can also declare a length. This prevents R from
          # copying the list every time we add an element
          distractors <- vector(mode="list", length = 3)
          
          #add distractors to the list. 
          # each element of the list should be a set
          # represented as a list.
          for(i in (1:3)){
            
            
            if (i == 1) {
              
              currentDist <- 3 ** length(sourceSetsVector)
              currentDist <- toString(currentDist, width = NULL) 
              
              
              #Note the single brackets '[1]' here 
              distractors[i] <- currentDist
              
            } else if (i == 2) {
              
              #using a correct set to generate better distractors. 
              if (length(sourceSetsVector) != (1 || 2)) {
                currentDist <- 2 * length(sourceSetsVector)
                currentDist <- toString(currentDist, width = NULL) 
              } else {
                currentDist <- 10 * length(sourceSetsVector)
                currentDist <- toString(currentDist, width = NULL)
                }
              }
              
              
              #Note the single brackets '[1]' here 
              distractors[i] <- currentDist
              
            } else if (i == 3) {
              
              #uses an alternative power set function from the rje package. Will generate distractors that are nearly correct to completely incorrect.
              currentDist <- sample((correctDisctractor + 1):(correctDisctractor + 5), 1)
              currentDist <- toString(currentDist, width = NULL) 
              
              
              #Note the single brackets '[1]' here 
              distractors[i] <- currentDist
              
            }
            
            
            
          }
          
          
          
        } else {
          #formats cardinality into different notation to make questions harder. 
          stringedAnswer <- toString(length(sourceSetsVector), width = NULL)
          correct <- paste("\\$", "2^", stringedAnswer, "\\$")
          
          
          #used later for distractor generation.
          correctDisctractor <- length(set_power(sourceSetsVector))
          
          
          #Create a vector that will hold distractors
          #NOTE: we declare the list with vector() here so that 
          # we can also declare a length. This prevents R from
          # copying the list every time we add an element
          distractors <- vector(mode="list", length = 3)
          
          #add distractors to the list. 
          # each element of the list should be a set
          # represented as a list.
          for(i in (1:3)){
            
            
            if (i == 1) {
              
              currentDist <-  toString(length(sourceSetsVector), width = NULL)
              currentDist <-  paste("\\$", "3^", stringedAnswer, "\\$")
              
              
              #Note the single brackets '[1]' here 
              distractors[i] <- currentDist
              
            } else if (i == 2) {
              
              currentDist <-  toString(length(sourceSetsVector), width = NULL)
              currentDist <-  paste("\\$", "2\times", stringedAnswer, "\\$")
              
              
              #Note the single brackets '[1]' here 
              distractors[i] <- currentDist
              
            } else if (i == 3) {
              
              currentDist <-  paste("\\$", "\\log_{2}", correctDisctractor, "\\$")
              
              
              #Note the single brackets '[1]' here 
              distractors[i] <- currentDist
              
            }
            
            
            
          }
        } 
        
      }    
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
  
  sourceSets <- str_replace_all(sourceSets, c("list" = ""))
  sourceSets <- str_replace_all(sourceSets, c("\\(" = ""))
  sourceSets <- str_replace_all(sourceSets, c("\\)" = ""))
  sourceSets <- str_replace_all(sourceSets, c("\"" = ""))
  
  
  
  #format the the sourceSets as Question String.
  # "A = {...}"
  sourceSets <- insertSetRStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  return(toSend)
}

x <- powerSetQA()
print(x)
