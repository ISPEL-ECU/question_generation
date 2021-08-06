# Author:               Trevor Strobel (strobelt09), Aleksei Vilkomir (avilk)
# File:                 utils.R
# Date:                 4/11/21

library(stringr)
source("utils/set-generation.R") #used in the distractor bank for cartesian product.



# formatAsSet(str) formats a List as a string representing
# a set in set notation for display with MathJax.
# TODO: there is currently an issue with having to escape the
# escape character. This causes MathJax to not render the '\\{'
# or '\\}'
formatListAsSet <- function(inputList){

    #format as string
    result <- paste(c(inputList), collapse=', ')
    #insert prefix and postfix. escape character nonsense involved here. 
    finalResult <- paste("\\$\\{", result," \\}\\$")
    return(finalResult)
}

# formatPowerSetListAsSet(str) formats a List that contains a powerSet as a string representing
# a set in set notation for display with MathJax.
# also paste {} into original list. 
# TODO: there is currently an issue with having to escape the
# escape character. This causes MathJax to not render the '\\{'
# or '\\}'
formatPowerSetListAsSet <- function(inputList){
    
    #concatenate {} to original list
    inputList <- c("[]", inputList)
    
    #format as string
    result <- paste(c(inputList), collapse='\\}, \\{')
    result <- str_replace_all(result, c("c" = ""))
    result <- str_replace_all(result, c("\\(" = "<"))
    result <- str_replace_all(result, c("<" = ""))
    result <- str_replace_all(result, c("\\)" = ">"))
    result <- str_replace_all(result, c("\\[" = "\\\\{"))
    result <- str_replace_all(result, c("]" = ""))
    result <- str_replace_all(result, c(":" = ", "))
    result <- str_replace_all(result, c(">" = ""))
    result <- str_replace_all(result, c("\"" = ""))
    result <- str_replace_all(result, c("list" = ""))
    
    
    #insert prefix and postfix. escape character nonsense involved here. 
    finalResult <- paste("\\$\\{ ", result,"\\} \\}\\$")
    return(finalResult)
}


#input_list is the list or vector that will used to calculate the cardinality
#cardinality_style is how the notation for cardinality will be printed to the front end. 
power_set_cardinality_format <- function(input_list = NULL, cardinality_style = NULL) {
    
    if (cardinality_style == 1) {
    
        cardinality <-  2**(length(input_list) + 1)
        cardinality <- toString(cardinality, width = NULL)
    }
    
    if (cardinality_style == 2) {
        
        cardinality <- length(input_list) + 1
        cardinality <- toString(cardinality, width = NULL)
        cardinality <- paste("\\$", "2^", cardinality, "\\$")
    }
    
    return(cardinality)
}


# insertSetQStrings takes a list of sets and appends strings to the list
#  such that the final list is as follows:
# 
#  'Let \\; A=' [a set of numbers]
#  'and \\; B=' [a set of numbers] 
#  'be \\; two \\; sets.'
insertSetQStrings <- function(sets) {
    nsets <- list()
    #nsets[1] <- 'Let A and B be two sets. What is \\$A\\cup B\\$?'
    nsets[1] <- paste('\\$A=\\$', sets[1])
    nsets[2] <- paste('\\$B=\\$',sets[2])

    return(nsets)
}

# insertSetQStrings takes a list of sets and appends strings to the list
#  such that the final list is as follows:
# 
#  'Let \\; A=' [a set of numbers]
#  'and \\; B=' [a set of numbers]
#  'and \\; C=' [a set of numbers]
#  'be \\; three \\; sets.'
insertSet3Strings <- function(sets) {
    nsets <- list()
    #nsets[1] <- 'Let A and B be two sets. What is \\$A\\cup B\\$?'
    nsets[1] <- paste('\\$A=\\$', sets[1])
    nsets[2] <- paste('\\$B=\\$',sets[2])
    nsets[3] <- paste('\\$C=\\$',sets[3])
    return(nsets)
}

# insertSetRStrings takes a single set and appends strings to the set
#  such that the final list is as follows:
# 
#  'Let \\; A=' [a set of numbers]
#  'be \\; a \\; set.'
insertSetRStrings <- function(sets) {
    nsets <- list()
    nsets[1] <- paste('\\$A=\\$', sets[1])
    
    return(nsets)
}


#function to format cartesian product.
#design to work with list and vectors
#paste function converts the list to a vector and adds additional characters to indentify what was inside the original list.
#list_length takes the length of the cartesian set.
#cartesian_set is the list of list that contains the cartesian set. 

format_cart_set_list <- function(list_length = NULL , cartesian_set) {
    
    if(list_length < 1) {
        return(NULL)
    }
    
    for (i in 1:list_length) {
        temporary_set <- cartesian_set[i]
        temporary_set <- paste("(",temporary_set, ")")
        temporary_set <- str_replace_all(temporary_set, c("c" = ""))
        temporary_set <- str_replace_all(temporary_set, c("\\( \\(" = "\\("))
        temporary_set <- str_replace_all(temporary_set, c("\\( " = "\\("))
        temporary_set <- str_replace_all(temporary_set, c("\\) \\)" = "\\)"))
        temporary_set <- str_replace_all(temporary_set, c(" \\)" = "\\)"))
        temporary_set <- str_replace_all(temporary_set, c(":" = ", "))
        temporary_set <- str_replace_all(temporary_set, c("list\\(" = ""))
        cartesian_set[[i]] <- temporary_set
        
    }

    return(cartesian_set)
}


#Function to correctly format inner sets so that there are no display issues.
formatPartitionAsSet <- function(inputList){
    
    #format as string
    result <- paste(c(inputList), collapse=', ')
    
    #insert prefix and postfix. escape character nonsense involved here. 
    finalResult <- paste("\\{", result," \\}")
    return(finalResult)
}


