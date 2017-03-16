parse_template <- function() { 
  temp=readLines("template.yaml")
  dat <-NULL
  randList <- NULL
  for ( row in temp){ 
  
     if ( grepl( "CorrectAnswer:" , row ) || grepl( "AnswerTests:" , row )) {
      val <-strsplit(row, "_random")[[1]]
      valLen <- length(val)
      answerFront <- val[1]
      answerBack <- NULL
      for (n in 2:valLen){
        subVal <- strsplit(val[n], "")[[1]]
        subLen <- length(subVal)
        index <- strtoi(subVal[1])
      
        randVal <- randList[index]
       answerBack <- NULL
        for ( n in 2:subLen ){
          if (is.na(subVal[n])){
            break
          }
          answerBack <- paste(answerBack, subVal[n], sep="" )
        }
      
        answerFront <- paste(answerFront, randVal, answerBack, sep = "")
      }
      resultRow <- answerFront[1]
      dat <- rbind(dat, resultRow)
    } else if( grepl( "_random" , row ) ){
     value <- strsplit(row, "_random")[[1]]
     valLen <- length(value)
     back <- NULL
     front <- value[1]
     for ( i in 2:valLen ){ 
       backTemp <- value[i]
       backTemp <- strsplit(backTemp, " ")[[1]]
       randSplit <- strsplit(backTemp[1], "_")[[1]]
       len <- length(backTemp)
       randCount <- randSplit[2]
       num1 <- randSplit[3]
       num2 <- randSplit[4]
       randomNumber <- sample(num1:num2 ,1)
      
       randList <- c(randList, randomNumber)
       back<-NULL
       for ( n in 2:len ){
           back <- paste(back, backTemp[n])
       }
      if (len == 1 ){
        back <- NULL
      } 
       front <- paste(front, randomNumber, back)
     }
     randomRow <- front[1]
     dat <- rbind(dat, randomRow)
    } else {
      if ( grepl( "Class:" , row )){
        randList <- NULL
      }
        dat <- rbind(dat,row)
      }
    }
  #print(dat)
  
  # Once the template rows has been completed by the for loop, dat is then printed to the lesson file.
  writeLines(dat, "C:/SCHOOL/IND. STUDY/project/Dynamic_YML/Test/lesson.yaml")
}
