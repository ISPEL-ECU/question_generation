#worked on 7/1/2021

library(set)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")   



#   @param      numSets         The number of sets to consider in the question
#   @param      setSize         The length of the source sets.
#   @param      dType           The desired data type for set elements
#                               (1: Ints, 2: Real, 3: Complex, 
#                               4: Char, 5: String, 6: Mixed)  
#   @param      qDifficulty     The level of difficulty of the question generated.
#
#   @return     toSend          A json-like object containing the
#                               source sets, correct, and 
#                               distractors (incorrect answers)
#  
cartesianProduct <- function(numSets = 2, setSize = 3, dType = 1, qDifficulty = 1) {

    if (qDifficulty == 1) {
        questionText <-('Let A and B be two sets. What is \\$A\\times B\\$?')
          
          
        #generate and fill sets
        sourceSets <- getSets(n = numSets, m = setSize, x = dType)
        
        
        #convert each set into a vector to manipulate better
        for(i in (1:2)){
          sourceSets[[i]] <- unlist(sourceSets[[i]])
        }
        
        
        #finds the correct length/cardinality of the cartesian product set.
        setOneLength = length(sourceSets[[1]])
        setTwoLength = length(sourceSets[[2]])
        cartesianProductCardinality = setOneLength * setTwoLength 
        
        
        
        #creates a list to store the pairs when the cartesian product is calculated
        cartesianSet <- list()
        #merge two sets to get a matrix.  
        cartesianSet.df <- merge(sourceSets[[1]], sourceSets[[2]])
        #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
        for(e in (1:cartesianProductCardinality)) {
          cartesianSet[[e]] <- as.vector(cartesianSet.df[e, ])
        
        }
        #makes the cartesesian set into a string
        correct <- toString(cartesianSet, width = NULL) 
        #uses the formatListAsSet function to format the string located in "utils/format.R".
        correct <- formatListAsSet(correct[[1]])
        
        
        
        
        
        
      
        #Create a vector that will hold distractors
        #NOTE: we declare the list with vector() here so that 
        # we can also declare a length. This prevents R from
        # copying the list every time we add an element
        distractors <- vector(mode="list", length = 3)
        
        # add distractors to the list. 
        # each element of the list should be a set
        # represented as a list.
        for(i in (1:3)){
          
          #set generation for Cartesian product. 
          set1 <- (getSets(n = 1, m = (setSize), x = dType))
          set2 <- (getSets(n = 1, m = (setSize), x = dType))
          
          #changes list to vector to better manipulate.
          set1 <- unlist(set1)
          set2 <- unlist(set2)
          
          #gets length for each set. 
          set1Length = length(set1)
          set2Length = length(set2)
          
          #gets length for Cartesian product. 
          cartesianDistractorCardinality = set1Length * set2Length
          
          
          #creates a list to store pairs of Cartesian product. 
          cartesianDistractor <- list()
          
          #merge two sets to get a matrix.  
          cartesianDistractors.df <- merge(set1, set2)
          
          #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
          for(e in (1:cartesianDistractorCardinality)) {
            cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
          }
          
          
          #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
          currentDist <- cartesianDistractor
          currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R
          
          
          #Note the single brackets '[1]' here 
          distractors[i] <- currentDist
        }
        
        
        #formats the "distractor" cartesian product string correctly.
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
        
        
        #format the the sourceSets as Question String.
        # "A = {...}"
        # "B = {...}"
        sourceSets <- insertSetQStrings(sourceSets)
       
        
        #formats the "correct" cartesian product string correctly.
        correct <- formatListAsSet(cartesianSet)
        correct <- str_replace_all(correct,c("list" = ""))
        correct <- str_replace_all(correct, c("x = " = ""))
        correct <- str_replace_all(correct, c("y = " = ""))
        correct <- str_replace_all(correct, c('\"' = ""))
      
        
        
        # now we concatenate the question contents together
        questionContents <- c(questionText, sourceSets)
        
        #format answers and sources into json and return results 
        toSend <- list(content= questionContents, correct= correct, distractors= distractors)
        
        #jsonToSend <- toJSON(toSend)
        
        
        return(toSend)
    }
      
    else if (qDifficulty == 2) {
      #randomly decides whether the question will A x B or B x A
      randomChoice <- sample((1:2), 1)
      print(randomChoice)
      
      if (randomChoice == 1 ) {
        
        
        questionText1 <-('Let A and B be two sets. What is \\$A\\times B\\$?')
        
        #since it's level 2 difficulty random data types are introduced. 
        dType = sample(1:6, 1)
        
        #generate and fill sets
        sourceSets <- getSets(n = numSets, m = sample((2:4),1), x = dType)
        
        
        #convert each set into a vector to manipulate better
        for(i in (1:2)){
          sourceSets[[i]] <- unlist(sourceSets[[i]])
        }
        
        
        #finds the correct length/cardinality of the cartesian product set.
        setOneLength = length(sourceSets[[1]])
        setTwoLength = length(sourceSets[[2]])
        cartesianProductCardinality = setOneLength * setTwoLength 
        
        
        
        #creates a list to store the pairs when the cartesian product is calculated
        cartesianSet <- list()
        #merge two sets to get a matrix.  
        cartesianSet.df <- merge(sourceSets[[1]], sourceSets[[2]])
        #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
        for(e in (1:cartesianProductCardinality)) {
          cartesianSet[[e]] <- as.vector(cartesianSet.df[e, ])
          
        }
        #makes the cartesesian set into a string
        correct <- toString(cartesianSet, width = NULL) 
        #uses the formatListAsSet function to format the string located in "utils/format.R".
        correct <- formatListAsSet(correct[[1]])
        
        
        
        #Create a vector that will hold distractors
        #NOTE: we declare the list with vector() here so that 
        # we can also declare a length. This prevents R from
        # copying the list every time we add an element
        distractors <- vector(mode="list", length = 3)
        
        # three types of distractors are implemented for lvl 2 difficulty: reverse Sets, flipped cartesian product (for example B X A instead of A X B), and random sets. 
        # add distractors to the list. 
        # each element of the list should be a set
        # represented as a list.
        for(i in (1:3)){
          
          #check to see if question is a flipped question
          
          #generate different sets some that are reversed, some normal, some from source but multiplied from in wrong order. 
          
          if (i == 1) {
            
            
            #set generation for Cartesian product.Grabs a completely random set using the getSet function from "utils/set-generation.R'
            randomSet1 <- (getSets(n = 1, m = (setSize), x = dType))
            randomSet2 <- (getSets(n = 1, m = (setSize), x = dType))
            
            #changes list to vector to better manipulate.
            randomSet1 <- unlist(randomSet1)
            randomSet2 <- unlist(randomSet2)
            
            #gets length for each set. 
            set1Length = length(randomSet1)
            set2Length = length(randomSet2)
            
            #gets length for Cartesian product. 
            cartesianDistractorCardinality = set1Length * set2Length
            
            
            #creates a list to store pairs of Cartesian product. 
            cartesianDistractor <- list()
            
            #merge two sets to get a matrix.  
            cartesianDistractors.df <- merge(randomSet1, randomSet2)
            
            #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
            for(e in (1:cartesianDistractorCardinality)) {
              cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
            }
            
            
            #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
            currentDist <- cartesianDistractor
            currentDist <- formatListAsSet(currentDist)  
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
            
            
          }
          
          else if (i == 2) {
            
            #the reversed sets from the source sets. 
            reverseSet1 <- rev(sourceSets[[1]])
            reverseSet2 <- rev(sourceSets[[2]])
            
            
            #changes list to vector to better manipulate.
            reverseSet1 <- unlist(reverseSet1)
            reverseSet2 <- unlist(reverseSet2)
            
            #gets length for each set. 
            set1Length = length(reverseSet1)
            set2Length = length(reverseSet2)
            
            #gets length for Cartesian product. 
            cartesianDistractorCardinality = set1Length * set2Length
            
            
            #creates a list to store pairs of Cartesian product. 
            cartesianDistractor <- list()
            
            #merge two sets to get a matrix.  
            cartesianDistractors.df <- merge(reverseSet1, reverseSet2)
            
            #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
            for(e in (1:cartesianDistractorCardinality)) {
              cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
            }
            
            
            #assigns the cartesian pairs to currentDist and formats it using formatListAsSet function from "utils/format.R".
            currentDist <- cartesianDistractor
            currentDist <- formatListAsSet(currentDist)  
            
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
            
          }
          
          else {
            
            #the original sets used in the original question which will be calculated as B X A instead of A X B
            origSet1 <- sourceSets[[1]]
            origSet2 <- sourceSets[[2]]
            
            #append the randomly generated sets with the original sets and reversed sets in correct question 
            
            
            #changes list to vector to better manipulate.
            origSet1 <- unlist(origSet1)
            origSet2 <- unlist(origSet2)
            
            #gets length for each set. 
            set1Length = length(origSet1)
            set2Length = length(origSet2)
            
            #gets length for Cartesian product. 
            cartesianDistractorCardinality = set1Length * set2Length
            
            
            #creates a list to store pairs of Cartesian product. 
            cartesianDistractor <- list()
            
            #merge two sets to get a matrix.  
            cartesianDistractors.df <- merge(origSet2, origSet1)
            
            #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
            for(e in (1:cartesianDistractorCardinality)) {
              cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
            }
            
            
            #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
            currentDist <- cartesianDistractor
            currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R
            
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
          }
          
          
        }
        
        
        #formats the "distractor" cartesian product string correctly.
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
        
        
        #format the the sourceSets as Question String.
        # "A = {...}"
        # "B = {...}"
        sourceSets <- insertSetQStrings(sourceSets)
        
        
        #formats the "correct" cartesian product string correctly.
        correct <- formatListAsSet(cartesianSet)
        correct <- str_replace_all(correct,c("list" = ""))
        correct <- str_replace_all(correct, c("x = " = ""))
        correct <- str_replace_all(correct, c("y = " = ""))
        correct <- str_replace_all(correct, c('\"' = ""))
        
        
        # now we concatenate the question contents together
        questionContents <- c(questionText1, sourceSets)
        
        #format answers and sources into json and return results 
        toSend <- list(content= questionContents, correct= correct, distractors= distractors)
        
        print(toSend)
        
        
        #jsonToSend <- toJSON(toSend
      } else {
        
        questionText2 <-('Let A and B be two sets. What is \\$B\\times A\\$?')
        
        
        #since it's level 2 difficulty random data types are introduced. 
        dType = sample(1:6, 1)
        
        dType =1 
        
        #generate and fill sets
        sourceSets <- getSets(n = numSets, m = sample((2:4),1), x = dType)
        
        
        #convert each set into a vector to manipulate better
        for(i in (1:2)){
          sourceSets[[i]] <- unlist(sourceSets[[i]])
        }
        
        
        #finds the correct length/cardinality of the cartesian product set.
        setOneLength = length(sourceSets[[1]])
        setTwoLength = length(sourceSets[[2]])
        cartesianProductCardinality = setOneLength * setTwoLength 
        
        
        
        #creates a list to store the pairs when the cartesian product is calculated
        cartesianSet <- list()
        #merge two sets to get a matrix.  
        cartesianSet.df <- merge(sourceSets[[2]], sourceSets[[1]])
        #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
        for(e in (1:cartesianProductCardinality)) {
          cartesianSet[[e]] <- as.vector(cartesianSet.df[e, ])
          
        }
        #makes the cartesesian set into a string
        correct <- toString(cartesianSet, width = NULL) 
        #uses the formatListAsSet function to format the string located in "utils/format.R".
        correct <- formatListAsSet(correct[[1]])
        
        
        
        #Create a vector that will hold distractors
        #NOTE: we declare the list with vector() here so that 
        # we can also declare a length. This prevents R from
        # copying the list every time we add an element
        distractors <- vector(mode="list", length = 3)
        
        # three types of distractors are implemented for lvl 2 difficulty: reverse Sets, flipped cartesian product (for example B X A instead of A X B), and random sets. 
        # add distractors to the list. 
        # each element of the list should be a set
        # represented as a list.
        for(i in (1:3)){
          
          #check to see if question is a flipped question
          
          #generate different sets some that are reversed, some normal, some from source but multiplied from in wrong order. 
          
          if (i == 1) {
            
            
            #set generation for Cartesian product.Grabs a completely random set using the getSet function from "utils/set-generation.R'
            randomSet1 <- (getSets(n = 1, m = (setSize), x = dType))
            randomSet2 <- (getSets(n = 1, m = (setSize), x = dType))
            
            #changes list to vector to better manipulate.
            randomSet1 <- unlist(randomSet1)
            randomSet2 <- unlist(randomSet2)
            
            #gets length for each set. 
            set1Length = length(randomSet1)
            set2Length = length(randomSet2)
            
            #gets length for Cartesian product. 
            cartesianDistractorCardinality = set1Length * set2Length
            
            
            #creates a list to store pairs of Cartesian product. 
            cartesianDistractor <- list()
            
            #merge two sets to get a matrix.  
            cartesianDistractors.df <- merge(randomSet1, randomSet2)
            
            #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
            for(e in (1:cartesianDistractorCardinality)) {
              cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
            }
            
            
            #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
            currentDist <- cartesianDistractor
            currentDist <- formatListAsSet(currentDist)  
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
            
            
          }
          
          else if (i == 2) {
            
            #the reversed sets from the source sets. 
            reverseSet1 <- rev(sourceSets[[1]])
            reverseSet2 <- rev(sourceSets[[2]])
            
            
            #changes list to vector to better manipulate.
            reverseSet1 <- unlist(reverseSet1)
            reverseSet2 <- unlist(reverseSet2)
            
            #gets length for each set. 
            set1Length = length(reverseSet1)
            set2Length = length(reverseSet2)
            
            #gets length for Cartesian product. 
            cartesianDistractorCardinality = set1Length * set2Length
            
            
            #creates a list to store pairs of Cartesian product. 
            cartesianDistractor <- list()
            
            #merge two sets to get a matrix.  
            cartesianDistractors.df <- merge(reverseSet2, reverseSet1)
            
            #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
            for(e in (1:cartesianDistractorCardinality)) {
              cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
            }
            
            
            #assigns the cartesian pairs to currentDist and formats it using formatListAsSet function from "utils/format.R".
            currentDist <- cartesianDistractor
            currentDist <- formatListAsSet(currentDist)  
            
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
            
          }
          
          else {
            
            #the original sets used in the original question which will be calculated as A X B instead of B X A
            origSet1 <- sourceSets[[1]]
            origSet2 <- sourceSets[[2]]
            
            #append the randomly generated sets with the original sets and reversed sets in correct question 
            
            
            #changes list to vector to better manipulate.
            origSet1 <- unlist(origSet1)
            origSet2 <- unlist(origSet2)
            
            #gets length for each set. 
            set1Length = length(origSet1)
            set2Length = length(origSet2)
            
            #gets length for Cartesian product. 
            cartesianDistractorCardinality = set1Length * set2Length
            
            
            #creates a list to store pairs of Cartesian product. 
            cartesianDistractor <- list()
            
            #merge two sets to get a matrix.  
            cartesianDistractors.df <- merge(origSet1, origSet2)
            
            #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
            for(e in (1:cartesianDistractorCardinality)) {
              cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
            }
            
            
            #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
            currentDist <- cartesianDistractor
            currentDist <- formatListAsSet(currentDist)  
            
            
            #Note the single brackets '[1]' here 
            distractors[i] <- currentDist
          }
          
          
        }
        
        
        #formats the "distractor" cartesian product string correctly.
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
        
        
        #format the the sourceSets as Question String.
        # "A = {...}"
        # "B = {...}"
        sourceSets <- insertSetQStrings(sourceSets)
        
        
        #formats the "correct" cartesian product string correctly.
        correct <- formatListAsSet(cartesianSet)
        correct <- str_replace_all(correct,c("list" = ""))
        correct <- str_replace_all(correct, c("x = " = ""))
        correct <- str_replace_all(correct, c("y = " = ""))
        correct <- str_replace_all(correct, c('\"' = ""))
        
        
        # now we concatenate the question contents together
        questionContents <- c(questionText2, sourceSets)
        
        #format answers and sources into json and return results 
        toSend <- list(content= questionContents, correct= correct, distractors= distractors)
        
        return(toSend)
        
        
      }
    }
  }

x <- cartesianProduct()



