#### CORRELATION MATRIX FORMATTING
format.rmatrix <- function(input, excel.friendly=TRUE) {

  library('stringr')
  char <- as.numeric(input$r)
  char <- round(char, 2)
  char <- as.character(char)
  char[char=="0"] <- "0.00"
  char[char=="1"] <- ""
  cols <- sqrt(length(input$r))

  # make negative ones be - and 3rd through 5th characters
  char[str_count(char, "-")==1] <- paste( "-", substring(char[str_count(char, "-")==1],3,5), sep="")
  char[str_count(char, "-")==0] <- substring(char[str_count(char, "-")==0],2,4)

  # check to make sure they are long enough. If too short, add a trailing zero (negatives)
  temp <- char[str_count(char, "-")==1]
  temp[str_length(temp)==3] <- paste(temp[str_length(temp)==3], "0", sep="")
  char[str_count(char, "-")==1] <- temp

  # check to make sure they are long enough. If too short, add a trailing zero (positives)
  temp <- char[str_count(char, "-")==0]
  temp[str_length(temp)==2] <- paste(temp[str_length(temp)==2], "0", sep="")
  char[str_count(char, "-")==0] <- temp

  char <- matrix(char, ncol=cols)


  star.3 <- matrix(input$p < .001, ncol=cols)
  star.2 <- matrix(input$p < .01 & input$p >= .001, ncol=cols)
  star.1 <- matrix(input$p <= .05 & input$p >= .01, ncol=cols)
  plus.1 <- matrix(input$p < .10 & input$p > .05, ncol=cols)

  char[star.3] <- paste(char[star.3], "***", sep="")
  char[star.2] <- paste(char[star.2], "**", sep="")
  char[star.1] <- paste(char[star.1], "*", sep="")
  char[plus.1] <- paste(char[plus.1], "+", sep="")

  # for excel purposes, nonsig shoudl get a _
  if(excel.friendly==TRUE){char[!star.3 & !star.2 & !star.1 & !plus.1] <- paste(char[!star.3 & !star.2 & !star.1 & !plus.1], "_", sep="")}

  # delete the upper half of the matrix
  low.diag <- function(temp){
    for (i in 1:cols) { # go row by row
      temp[i,] <- c(rep(F,i-1), rep(T, cols-i+1))
    }
    return(temp)
  }

  logic <- matrix(rep(F, length(input$r)), ncol=cols)
  logic <- low.diag(logic)
  char[logic] <- ""
  rownames(char) <- rownames(input$r)
  colnames(char) <- rownames(input$r)

  return(char)
}
