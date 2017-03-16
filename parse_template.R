parse_template <- function() {
  temp = readLines("template.yaml")
  dat <- NULL
  randList <- NULL
  for (row in temp) {

    if (grepl("CorrectAnswer:" , row) ||
        grepl("AnswerTests:" , row)) {
      val <- strsplit(row, "_random")[[1]]
      valLen <- length(val)
      answerFront <- val[1]
      answerBack <- NULL
      
      #Parses the string for the index of the random value 
      #[ ie _Random1, it extracts the 1 and finds the value in randList]
      for (n in 2:valLen) {
        subVal <- strsplit(val[n], "")[[1]]
        subLen <- length(subVal)
        index <- strtoi(subVal[1])
        randVal <- randList[index]
        answerBack <- NULL
        
        #Places the value from randList in the appropriate postion in the string
        for (n in 2:subLen) {
          if (is.na(subVal[n])) {
            break
          }
          answerBack <- paste(answerBack, subVal[n], sep = "")
        }
        #paste the sections that have been evaluated together
        answerFront <-
          paste(answerFront, randVal, answerBack, sep = "")
      }
      # Create the row vector and place in the DF
      resultRow <- answerFront[1]
      dat <- rbind(dat, resultRow)
      
      
    } else if (grepl("_random" , row)) {
      value <- strsplit(row, "_random")[[1]]
      valLen <- length(value)
      back <- NULL
      front <- value[1]
      # Parses the string for the random syntax _random1_5_10 
      # 5 is the lower bound and 10 is the higher bound for the random number generated
      for (i in 2:valLen) {
        backTemp <- value[i]
        backTemp <- strsplit(backTemp, " ")[[1]]
        randSplit <- strsplit(backTemp[1], "_")[[1]]
        len <- length(backTemp)
        randCount <- randSplit[2]
        num1 <- randSplit[3]
        num2 <- randSplit[4]
        randomNumber <- sample(num1:num2 ,1)
        randList <- c(randList, randomNumber)
        back <- NULL
        # paste the remaining portion of this subLine
        for (n in 2:len) {
          back <- paste(back, backTemp[n])
        }
        if (len == 1) {
          back <- NULL
        }
        # Paste the evaulated sections together.
        front <- paste(front, randomNumber, back)
      }
      # Create the row vector and place into the DF
      randomRow <- front[1]
      dat <- rbind(dat, randomRow)
      
      # Nulling randList avoids incorrect data being used in other questions
      } else {
      if (grepl("Class:" , row)) {
        randList <- NULL
      }
      dat <- rbind(dat,row)
    }
  }
  
  # Once the template rows has been completed by the for loop, dat is then printed to the lesson file.
  writeLines(dat, "lesson.yaml")
}
