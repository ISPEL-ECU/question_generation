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


get_union_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is \\$A\\cup B\\$?')
            }
        }
        
    }
    
    if(qa_level == 2) {
        
        if (question_type == 1) {
        
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is A ∪ (B ∪ C)?')
            }
        }
        if (question_type == 2) {
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is (A ∪ B) ∪ C?')
            }
        }
            
    }
    
    
    if(qa_level == 3) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is A ∪ (B ∪ C)?')
            }
        if (question_type == 2) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is (A ∪ B) ∪ C?')
            }
        }
    }
    
    return(question_text)
} 

get_intersect_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is \\$A\\cap B\\$?')
            }
        }
        
    }
    
    if(qa_level == 2) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is \\$A\\cap B\\$?')
            }
        }
    }
    
    
    if(qa_level == 3) {
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is \\$A\\cap B\\$?')
            }
        }
    }
    
    return(question_text)
} 

get_asymm_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ("Let A and B be two sets. What is A - B?")
            }
        }
        
    }
    
    if(qa_level == 2) {
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is A - (B - C)?')
            }
        }
        if (question_type == 2) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is (A - B) - C?')
            }
        }
    }
    
    
    if(qa_level == 3) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is A - (B - C)?')
            }
        }
        if (question_type == 2) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is (A - B) - C?')
            }
        }
    }
    
    return(question_text)
} 

get_complement_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ("Let A be a set and B be the universal set. What is the complement of set A?")
            }
        }
        
    }
    
    if(qa_level == 2) {
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ("Let A be a set and B be the universal set. What is the complement of set A?")
            }
        }
    }
    
    
    if(qa_level == 3) {
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ("Let A be a set and B be the universal set. What is the complement of set A?")
            }
        }
    }
    
    return(question_text)
} 

get_equality_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ("Let A and B be two sets. Are A and B equal?")
            }
        }
        
    }
    
    if(qa_level == 2) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ("Let A be a set. Which set is equivalent to set A?")
            }
        }
    }
    
    
    if(qa_level == 3) {
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ("Let A be a set. Which set is not equivalent to set A?")
            }
        }
    }
    
    return(question_text)
} 


get_cardinality_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A be a set. What is the cardinality of set A?')
            }
        }
        
    }
    
    if(qa_level == 2) {
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A be a set. What is the cardinality of set A?')
            }
        }
    }
    
    
    if(qa_level == 3) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A be a set and B be a set. What is the total cardinality of sets A and B?')
            }
        }
    }    
        if (question_type == 2) {
            
            if(question_style == 1) {
                question_text <- ('Let A be a set and B be a set. Based on the relative cardinalities of A and B, what type of function is A → B?')
            }
        }
    }
    
    return(question_text)
} 

get_symm_diff_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is the symmetric difference of A and B?')
            }
        }
        
    }
    
    if(qa_level == 2) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A and B be two sets. What is the symmetric difference of A and B?')
            }
        }
    }
    
    
    if(qa_level == 3) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A, B, and C be three sets. What is the symmetric difference of A, B, and C?')
            }
        }
    }
    
    return(question_text)
} 

get_partition_set_question <- function(qa_level = NULL, question_type = NULL, question_style = NULL) {
    
    if(qa_level == 1) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A be a set. Which answer represents a correct set partition of set A?')
            }
        }
        
    }
    
    if(qa_level == 2) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A be a set. Which answer represents a incorrect set partition of set A?')
            }
        }
    }
    
    
    if(qa_level == 3) {
        
        if (question_type == 1) {
            
            if(question_style == 1) {
                question_text <- ('Let A be a set. Which answer represents a incorrect set partition of set A?')
            }
        }
    }
    
    return(question_text)
} 




