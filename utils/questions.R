# Author:               Joel Montano
# File:                 questions.R
# Date:                 8/5/2021

library(stringr)
source("utils/set-generation.R") #used in the distractor bank for cartesian product.




#The following is a question bank of all the questions for power set. 
#qa_level determines which level of the question we're asking.
#question_type determines which question we're asking within the difficulty level.
#question_style determines the style of the question being asked (sometimes the same question can be asked differently).
power_set_question_bank <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('What is the power set of A?')
            }
        }
    }
    
    if(qa_level == 2) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('What is the power set of A?')
            } else {
                question_text <- ('What is the P(A)?')
            }
        }
        
        
        if (question_type == 2) {
            
            if(question_style == 1) {
                question_text <- ('What is the cardinality of the power set of A?')
            } else {
                question_text <- ('What is |P(A)|?')
            }
        }
        
        
        if(question_type == 3) {
            
            if(question_style == 1) {
                question_text <- ('What is the cardinality of the power set of A?')
            } else {
                question_text <- ('What is |P(A)|?')
            }
        }
    }
    
    return(question_text)
} 



#The following is a question bank of all the questions for cartesian product.
#qa_level determines which level of the question we're asking.
#question_type determines which question we're asking within the difficulty level.
#question_style determines the style of the question being asked (sometimes the same question can be asked differently).
cartseian_product_question_bank <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {

    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is \\$A\\times B\\$?')
            }
        }
        
    }
    
    if(qa_level == 2) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is \\$A\\times B\\$?')
            }
        }
        
        
        if (question_type == 2) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is \\$B\\times A\\$?')
            } 
        }
    }
        
    
    if(qa_level == 3) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is \\$A\\times (B\\cap C)\\$?')
            } else {
                question_text <- ('Let A, B, and C be three sets. What is \\$(A\\times B) \\cap (A\\times C)\\$?')
            }
        }
        
        
        if (question_type == 2) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is \\$A\\times (B\\cup C)\\$?')
            } else {
                question_text <- ('Let A, B, and C be three sets. What is \\$(A\\times B) \\cup (A\\times C)\\$?')
            }
        
        }
        
        
        if (question_type == 3) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is \\$(A\\cap B) \\times C\\$?')
            } else {
                question_text <- ('Let A, B, and C be three sets. What is \\$(A\\times C) \\cap (B\\times C)\\$?')
            }
     
        }
        
        
        if (question_type == 4) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is \\$(A\\cup B) \\times C\\$?')
            } else {
                question_text <- ('Let A, B, and C be three sets. What is \\$(A\\times C) \\cup (B\\times C)\\$?')
            }
           
        } 
        
        
        if (question_type == 5) {
            question_text <- ('Let A, B, and C be three sets. What is \\$A\\times B \\times C \\$?')
        }
    }
    
    return(question_text)
} 

