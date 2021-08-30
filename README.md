# Set-Operations-api
Set Operations API

<h2>API functionallity</h2>

Currently implemented API functions are found within the `set-relation.R` file. Other files are for testing or documentation purposes and should not be used as an API. The following are the currently implemented endpoints:
  <ul>
  <li>/getSetUnion</li>
  <li>/getSetIntersect</li>
  <li>/getAsymDiff</li>
  <li>/getSetComplement</li>
  <li>/getSetEquality</li>
  <li>/getSetCardinality</li>
  <li>/getSymmDiff</li>
  <li>/getSetPartitions</li>
  </ul>

  <h2>File Structure</h2>
  
  The project has been refactored to allow for easier location and customization of specific topic parameters when writing or modifying both question banks, distractor banks, and question generations. The files related to question generation are structured as follows:
  
   <h4>/question-hub.R</h4>
     
   Contains the base functions for topic generation, which take the input parameters of qType, qDifficulty, and dataType and which call upon a given set-relation-level file to generate their output. The output is then sent to the API via the ` #* @post ` call. 
    
   <h4>/set-relation-level-1.R</h4>
      
   Contains the question generations for all topics of difficulty level 1 as well as function calls to both the question banks and distractor banks. 
   
   <h4>/set-relation-level-2.R</h4>
      
   Contains the question generations for all topics of difficulty level 2 as well as function calls to both the question banks and distractor banks. 
     
  <h4>/ set-relation-level-3.R</h4>
     
  Contains the question generations for all topics of difficulty level 3 as well as function calls to both the question banks and distractor banks. 
   
  <h4>/format.R</h4>
  
  Contains the functions related to formatting various types of set generations, included varying numbers of set, set partitions, and power sets. Additionally this file houses the string formatting functions used to format the sets within the API.
   
  <h4>/set-generation.R</h4>
  
  Contains the functions related to generating various types of sets, including sequential sets, power sets, sequential sets of varying notations, and random sets of varying datatypes.
 
  <h4>/distractors.R</h4>
  
  Contains the distractor banks for all three difficulty levels of a given question generation topic. The banks are subdivided by difficulty level(qa_level), question_type, and distractor type. Each corresponding difficulty level can be customized to allow for different distractors based on each question type asked, with the distractor_type variable corresponding to 1 individual distractor of the group of three to be paired with each correct answer.
  
  <h4>/questions.R</h4>
  
  Contains the question banks for all three difficulty levels of a given question generation. The banks are subdivided by difficulty level(qa_level), question_type, and question_style. Question_type refers to different questions for a given difficulty level, while the question_style variable refers to different wordings or stylings for the same question.
  
  <h2>Formatting Conventions<h2>
  
  Here I will outline a brief topic in pseudo code to give an idea of the formatting conventions used for topic generation:
  
  <h4>get_sample_topic_level_1</h4>
  
  qa_level <- question difficulty number
  question_type <- sample(range of number of question types)
  question_style <- sample(range of number of question styles)
  
  questionText <- get_sample_topic_set_question(with inputs of qa_level, question_type, and question_style)
  
  setGeneration
  #uses sourceSets, setNotation, or sequentialSets functions
  
  correct <- answer
  #perform set manipulations to generate and define correct answer
  
  define distractors list (vector(mode="list", length = number of distractors))  
  
  for(i in (1 : number of distractors)) :
  	current item in list <- get_sample_topic_set_distractor()
	
	
  sourceSets <- formatListAsSet(sourceSets)
 #formatting sourceSets for the API
 
 insertSetStrings(sourceSets)
 #formatting the formatted sourceSets into question strings for the topic.
 
 questionContents_c(questionText, sourceSets)
  #Question concatenation
  
  toSend <- list(content, correct, distractors)
  
  <h2>Endpoints in Detail</h2>

  <h4>/getSetUnion(qType, qDifficulty, dataType) </h4> 

  Topic:  `setUnion`

  <h5>Parameters</h5>  
  
```
  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficulty (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

The return is as follows:

```
  {
  "topic": [
    "setUnion"
  ],
  "type": [
    "1"
  ],
  "format": [
    "1"
  ],
  "difficulty": [
    "1"
  ],
  "question": {
    "content": [
      [
        "Let A and B be two sets. What is \\$A\\cup B\\$?"
      ],
      [
        "\\$A=\\$ \\$\\{ 13, 3, 4, 8, 12  \\}\\$"
      ],
      [
        "\\$B=\\$ \\$\\{ 17, 8, 5, 15, 13  \\}\\$"
      ]
    ],
    "correct": [
      "\\$\\{ 13, 3, 4, 8, 12, 17, 5, 15  \\}\\$"
    ],
    "distractors": [
      [
        "\\$\\{ 13, 3, 4, 12, 17, 5, 15  \\}\\$"
      ],
      [
        "\\$\\{ 13, 3, 4, 8, 12, 17, 5, 15, 5  \\}\\$"
      ],
      [
        "\\$\\{ 13, 3, 8, 12, 17, 5, 15  \\}\\$"
      ]
    ]
  }
}

```


  <h4>/getSetIntersect(qType, qDifficulty, dataType) </h4>
 
  Topic:  `setIntersect`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

<h4>/getAsymDiff(qType, qDifficulty, dataType) </h4>
  
  Topic:  `asymmetricDifference`

  <h5>Parameters</h5>  
  
```
  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```
  
  <h4>/getSetComplement(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setComplement`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```
  
  <h4>/getSetEquality(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setEquality`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```
  
  <h4>/getSetCardinality(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setCardinality`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

 <h4>/getSymmDiff(qType, qDifficulty, dataType) </h4>
  
  Topic:  `symmetricDifference`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

<h4>/getSetPartitions(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setPartitions`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```

 <h4>/setExpressions(qType, qDifficulty) </h4>
  
  Topic:  `setExpressions`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
 
```

<h4>/powerSetQ(qType, qDifficulty, dataType) </h4>
  
  Topic:  `powerSet`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```

<h4>/cartesianProduct(qType, qDifficulty, dataType) </h4>
  
  Topic:  `cartesianProduct`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```
