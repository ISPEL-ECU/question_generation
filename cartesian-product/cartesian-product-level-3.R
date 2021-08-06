#worked on 6/23/2021

library(set)
library(sets)
library(tidyr)
source("utils/format.R")            #set string formatting
source("utils/set-generation.R")    #set generation



#https://www.r-bloggers.com/2017/07/set-theory-ordered-pairs-and-cartesian-product-with-r/

# 
# 
# 
# a <- (getSets(n = 2, m = (1), x = 1))
# b <- (getSets(n = 2, m = (1), x = 1))
# 
# c <- (getSets(n = 2, m = (1), x = 1))
# 
# 
# 
# cartesian3sets <- function(h, i, j) {
#   hxixj <- list()
#   k <- 1
#   for (t in h) {
#     for (y in i) {
#       for( u in j) {
#         hxixj[[k]] <- c(t,y,u)
#         k <- k + 1
#       }
#     }
#   }
#   return(hxixj)
# }
# 
# cartesian2sets <- function(h, i) {
#   hxi <- list()
#   k <- 1
#   for (t in h) {
#     for (y in i) {
#       hxi[[k]] <- c(t,y)
#       k <- k + 1
#     }
#   }
#   return(hxi)
# }
# 
# 
# 
# 
# 
# tripletdistractors <- cartesian3sets(a,b,c)
# doubledistractors <- cartesian2sets(a, b)
# 
# 
# 
# currentDist <- doubledistractors
# 
# 
# 
# currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R
# 
# # temporarySet <- currentDist
# # temporarySet <- str_replace_all(temporarySet, c("list\\(list\\(" = "\\[\\"))
# # temporarySet <- str_replace_all(temporarySet, c("\\\"" = ""))
# # temporarySet <- str_replace_all(temporarySet, c("\\)\\)," = "],"))
# # temporarySet <- str_replace_all(temporarySet, c("\\)," = ","))
# # temporarySet <- str_replace_all(temporarySet, c("list\\(" = ""))
# # temporarySet <- str_replace_all(temporarySet, c("\\)\\)" = "]"))
# # currentDist <- temporarySet
# 
# 
# #for integers
# temporarySet <- currentDist
# temporarySet <- str_replace_all(temporarySet, c("list\\(" = "\\(\\"))
# temporarySet <- str_replace_all(temporarySet, c("\\)," = "],"))
# temporarySet <- str_replace_all(temporarySet, c("\\)" = "]"))
# temporarySet <- str_replace_all(temporarySet, c("\\[" = "\\("))
# temporarySet <- str_replace_all(temporarySet, c("]," = "\\),"))
# temporarySet <- str_replace_all(temporarySet, c("]" = "\\)"))
# currentDist <- temporarySet
# 
# print(currentDist)
# 
# 
# #different types of questions involving 2-3 sets 
# 
# questionText1 <-('Let A, B, and C be three sets. What is \\$A\\times (B\\cap C)\\$?')
# questionText1 <-('Let A, B, and C be three sets. What is \\$(A\\times B) \\cap (A\\times C)\\$?')
# 
# 
# correct <- cartesian2sets(a, intersect(b,c))
# print(intersect(b,c))
# correct <-formatListAsSet(correct)
# print(correct)
# 
# questionText1 <-('Let A, B, and C be three sets. What is \\$A\\times (B\\cup C)\\$?')
# questionText1 <-('Let A, B, and C be three sets. What is \\$(A\\times B) \\cup (A\\times C)\\$?')
# 
# correct2 <- cartesian2sets(a, union(b,c))
# correct2 <-formatListAsSet(correct2)
# print(correct2)
# 
# questionText1 <-('Let A, B, and C be three sets. What is \\$(A\\cap B) \\times C\\$?')
# questionText1 <-('Let A, B, and C be three sets. What is \\$(A\\times C) \\cap (B\\times C)\\$?')
# 
# correct3 <- cartesian2sets(intersect(a,b), c)
# correct3 <-formatListAsSet(correct3)
# print(correct3)
# 
# questionText1 <-('Let A, B, and C be three sets. What is \\$(A\\cup B) \\times C\\$?')
# questionText1 <-('Let A, B, and C be three sets. What is \\$(A\\times C) \\cup (B\\times C)\\$?')
# 
# correct4 <- cartesian2sets(union(a,b), c)
# correct4 <-formatListAsSet(correct4)
# print(correct4)
# 


#generate and fill sets.
sourceSets <- getSets(n = 3, m = 3, x = 1)
#assign each set to a variable. 
a <- sourceSets[[1]]
b <- sourceSets[[2]]
c <- sourceSets[[3]]
#randomly generates question asked
randomChoice <- sample(1:5, 1)

#functions that get cartesian product of 3 sets 
cartesian3sets <- function(h, i, j) {
  hxixj <- list()
  k <- 1
  for (t in h) {
    for (y in i) {
      for( u in j) {
        hxixj[[k]] <- c(t,y,u)
        k <- k + 1
      }
    }
  }
  return(hxixj)
}

#functions that get cartesian product of 2 sets
cartesian2sets <- function(h, i) {
  hxi <- list()
  k <- 1
  for (t in h) {
    for (y in i) {
      hxi[[k]] <- c(t,y)
      k <- k + 1
    }
  }
  return(hxi)
}

if(randomChoice == 1){
  questionType <- sample(1:2, 1)
  
  if(questionType == 1) {
    questionText1 <-('Let A, B, and C be three sets. What is \\$A\\times (B\\cap C)\\$?')
  } else {
    questionText1 <-('Let A, B, and C be three sets. What is \\$(A\\times B) \\cap (A\\times C)\\$?')
  }
  
  correct <- cartesian2sets(a, intersect(b,c))
  correct <-formatListAsSet(correct)
  
  correct <- str_replace_all(correct, c("c" = ""))
} else if(randomChoice == 2) {
  
  questionType <- sample(1:2, 1)
  
  if(questionType == 1) {
    questionText <-('Let A, B, and C be three sets. What is \\$A\\times (B\\cup C)\\$?')
  } else {
    questionText <-('Let A, B, and C be three sets. What is \\$(A\\times B) \\cup (A\\times C)\\$?')
  }
  
  correct <- cartesian2sets(a, union(b,c))
  correct <-formatListAsSet(correct)
  
  correct <- str_replace_all(correct, c("c" = ""))
  
} else if(randomChoice == 3) {
  
  questionType <- sample(1:2, 1)
  
  if(questionType == 1) {
    questionText <-('Let A, B, and C be three sets. What is \\$(A\\cap B) \\times C\\$?')
  } else {
    questionText <-('Let A, B, and C be three sets. What is \\$(A\\times C) \\cap (B\\times C)\\$?')
  }
  
  correct <- cartesian2sets(intersect(a,b), c)
  correct <-formatListAsSet(correct)
  
  correct <- str_replace_all(correct, c("c" = ""))
  
} else if(randomChoice == 4) {
  
  questionType <- sample(1:2, 1)
  
  if(questionType == 1) {
    questionText <-('Let A, B, and C be three sets. What is \\$(A\\cup B) \\times C\\$?')
  } else {
    questionText <-('Let A, B, and C be three sets. What is \\$(A\\times C) \\cup (B\\times C)\\$?')
  }
  
  
  
  correct <- cartesian2sets(union(a,b), c)
  correct <-formatListAsSet(correct)
  
  
  correct <- str_replace_all(correct, c("c" = ""))
  
} else if(randomChoice == 5) {
  
  questionText <-('Let A, B, and C be three sets. What is \\$A\\times B \\times C \\$?')
  
  correct <- cartesian3sets(a, b, c)
  
  temporarySet <- correct
  temporarySet <- str_replace_all(temporarySet, c("list\\(" = "\\(\\"))
  temporarySet <- str_replace_all(temporarySet, c("c" = ""))
  temporarySet <- str_replace_all(temporarySet, c("\\)," = "],"))
  temporarySet <- str_replace_all(temporarySet, c("\\)" = "]"))
  temporarySet <- str_replace_all(temporarySet, c("\\[" = "\\("))
  temporarySet <- str_replace_all(temporarySet, c("]," = "\\),"))
  temporarySet <- str_replace_all(temporarySet, c("]" = "\\)"))
  correct <- temporarySet
  
  correct <-formatListAsSet(correct)
}


#Create a vector that will hold distractors
#NOTE: we declare the list with vector() here so that 
# we can also declare a length. This prevents R from
# copying the list every time we add an element
distractors <- vector(mode="list", length = 3)


for(i in (1:3)){
  
  #generate and fill sets.
  distractorSet <- getSets(n = 3, m = 2, x = 1)
  #assign each set to a variable. 
  x <- distractorSet[[1]]
  y <- distractorSet[[2]]
  z <- distractorSet[[3]]

  
  currentDist <- cartesian3sets(x, y, z)
  print(currentDist)
  temporarySet <- currentDist
  temporarySet <- str_replace_all(temporarySet, c("list\\(" = "\\(\\"))
  temporarySet <- str_replace_all(temporarySet, c("c" = ""))
  temporarySet <- str_replace_all(temporarySet, c("\\)," = "],"))
  temporarySet <- str_replace_all(temporarySet, c("\\)" = "]"))
  temporarySet <- str_replace_all(temporarySet, c("\\[" = "\\("))
  temporarySet <- str_replace_all(temporarySet, c("]," = "\\),"))
  temporarySet <- str_replace_all(temporarySet, c("]" = "\\)"))
  currentDist <- temporarySet
  print(currentDist)
  
  currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R
 
  print(currentDist)
  #Note the single brackets '[1]' here 
  distractors[i] <- currentDist
  
}

print(distractors)

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
# "C = {...}"
sourceSets <- insertSet3Strings(sourceSets)




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


print(toSend)



