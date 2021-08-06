# Authors:         Trevor Strobel, Joel Montano, Christopher A. Wright
# File:           question-hub.R
# Date:           4/11/2021

library(plumber)
source("SetTheory/set-relation-level-1.R")
source("SetTheory/set-relation-level-2.R")
source("SetTheory/set-relation-level-3.R")



#this file acts as a routing hub to fetch questions for use in the ISPeL system.

#The following is pretty unsafe and is only planned to be used during early development. 
#It allows for Cross Origin Resource Sharing from any client. 
# TODO: I'm not going to worry about this for the time being as the API is only available on
# the server. The port is not exposed to the network, so it shouldn't be an issue. 
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}


#* @post /getSetUnion
getSetUnion <- function(qType = 1, qDifficulty = 1, dataType = 1 ){
  qTopic <- "setUnion"
  qFormat <- "1"
  output <- "If you're seeing this, Question Generation isn't working properly."
  #question specifics are returned as a list
  question <- list()
  
  
  if(qType == 1){
    
    if(qDifficulty == 1) {
      question <- get_cartesian_product_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_cartesian_product_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_cartesian_product_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return(output)
}


#* @post /getSetIntersect
getSetIntersect <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setIntersect"
  qFormat <- "1"
  output <- "If you're seeing this message, question generation isn't working properly."
  
  #question details are returned as a list. 
  question <- list()
  
  if(qType == 1){
    
    
    if(qDifficulty == 1) {
      question <- get_set_intersect_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_set_intersect_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_set_intersect_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
  
  }
  
  return(output)
}

#* @post /getAsymDiff
getAsymDiff <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "asymecticDifference"
  qFormat <- "1"
  output <- "If you're seeing this message, question generation isn't working properly."
  
  if(qType == 1){
    
    if(qDifficulty == 1) {
      question <- get_asym_diff_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_asym_diff_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_asym_diff_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return (output)
}

#* @post /getSetComplement
#* @param qType   The data type.
getSetComplement <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setComplement"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    
    if(qDifficulty == 1) {
      question <- get_set_complement_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_set_complement_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_set_complement_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return(output)
}

#* @post /getSetEquality
getSetEquality <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setEquality"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    
    if(qDifficulty == 1) {
      question <- get_set_equality_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_set_equality_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_set_equality_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return(output)
}

#* @post /getSetCardinality
getSetCardinality <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setCardinality"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    
    if(qDifficulty == 1) {
      question <- get_set_cardinality_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_set_cardinality_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_set_cardinality_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return(output)
}

#* @post /getSymmDiff
getSymmDiff <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "SymmetricDifference"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    
    if(qDifficulty == 1) {
      question <- get_symm_diff_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_symm_diff_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_symm_diff_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return(output)
}

#* @post /getSetPartitions
getSetPartitions <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "SetPartitions"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    
    if(qDifficulty == 1) {
      question <- get_set_partitions_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_set_partitions_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_set_partitions_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
  
  }
  
  return(output)
}



#* @post  /powerSetQ
powerSetQ <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "PowerSet"
  qFormat<- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if(qType == 1){
    
    if(qDifficulty == 1) {
      question <- get_power_set_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_power_set_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_power_set_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return(output)
}

#* @post /cartesianProduct
cartesianProductQA <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "cartesianProduct"
  qFormat <- "1"
  output <- "If you're seeing this message, question generation isn't working properly."
  
  #question details are returned as a list. 
  question <- list()
  
  if(qType == 1){
    
    if(qDifficulty == 1) {
      question <- get_cartesian_product_level_1(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 2) {
      question <- get_cartesian_product_level_2(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
    if(qDifficulty == 3) {
      question <- get_cartesian_product_level_3(dType = dataType)
      output <- list(topic = qTopic, type = qType, format = qFormat, question = question)
    }
    
  }
  
  return(output)
}






