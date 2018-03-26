
format.lm <- function(input, endsummary=T){

  input.b <- input %>% coef()
  input.beta <- input %>% lm.beta::lm.beta() %>% coef()
  input.p <- summary(input)$coefficients[,4]
  input.se <- summary(input)$coefficients[,2]
  temp2 <- input.p
  temp2[input.p>=.10] <- ""
  temp2[input.p<.10] <- "+"
  temp2[input.p<.05] <- "*"
  temp2[input.p<.01] <- "**"
  temp2[input.p<.001] <- "***"
  input.pcode <-temp2


  bs <- input.b %>% round(.,2)
  bs[substr(bs,1,1)!="-"] <- paste(" ",bs[substr(bs,1,1)!="-"], sep="")

  ses <- input.se %>% round(., 2)
  ses.txt <- as.character(ses)
  ses.txt[nchar(ses.txt)==1] <- paste(ses.txt[nchar(ses.txt)==1],".00", sep="")
  ses.txt[nchar(ses.txt)==3] <- paste(ses.txt[nchar(ses.txt)==3],"0", sep="")
  ses.txt <- paste("(", ses.txt, ")", sep="")

  betas <- as.character(round(input.beta,2))
  betas[substr(betas,1,1)!="-"] <- paste(" ",betas[substr(betas,1,1)!="-"], sep="")
  betas[nchar(betas)==4] <- paste(betas[nchar(betas)==4], "0", sep="")
  betas[nchar(betas)==2] <- paste(betas[nchar(betas)==2], ".00", sep="")
  betas <- paste(substr(betas,1,1), substr(betas,3,5), sep="")
  betas.txt <- paste(betas, input.pcode, sep="")

  output <- cbind(b.se=paste(bs, ses.txt) , beta=betas.txt)
  rownames(output) <- rownames(coef(summary(input)))
  if(endsummary==T){
    R2 <- round(summary(input)$r.squared, 2) %>% as.character
    R2 <- substring(R2, 2, nchar(R2))
    if(R2 == ".00") {R2 <- "< .001"} else {R2 <- paste("= ", R2, sep="")}

    df1 <- summary(input)$fstatistic[2]
    df2 <- summary(input)$fstatistic[3]
    Fstat <- round(summary(input)$fstatistic[1], 2)

    p.val <- 1 - pf(summary(input)$fstatistic[1], summary(input)$fstatistic[2], summary(input)$fstatistic[3])
    p.val.txt <- as.character(p.val)
    p.val.txt <- substring(p.val, 2, nchar(p.val))
    if(p.val < .001) {p.val.txt <- "< .001"}
    if(p.val >= .001 & p.val < .01) {p.val.txt <- paste("= ",substr(p.val.txt, 1,4), sep="")}
    if(p.val >= .01) {p.val.txt <- paste("= ", substr(p.val.txt, 1,3), sep="")}

    endrow <- rbind(paste("R2 ",R2, sep=""),
                    paste(
                      paste("F(",as.character(df1), ", ", as.character(df2),") = ",Fstat, sep=""),
                      ", ",
                      paste("p ",as.character(p.val.txt), sep=""),
                      sep="")
    )

    output <- rbind(output, cbind(endrow, rep("", 2)))

  }
  return(output)
}


format.nb <- function(input, endsummary=T){

  input.b <- input %>% coef()
  input.p <- summary(input)$coefficients[,4]
  input.se <- summary(input)$coefficients[,2]
  temp2 <- input.p
  temp2[input.p>=.10] <- ""
  temp2[input.p<.10] <- "+"
  temp2[input.p<.05] <- "*"
  temp2[input.p<.01] <- "**"
  temp2[input.p<.001] <- "***"
  input.pcode <-temp2


  bs.txt <- input.b %>% round(.,2) %>% as.character()
  bs.txt[substr(bs.txt,1,1)!="-"] <- paste(" ",bs.txt[substr(bs.txt,1,1)!="-"], sep="")
  bs.txt[nchar(bs.txt)==2] <- paste(bs.txt[nchar(bs.txt)==2],".00", sep="")
  bs.txt[nchar(bs.txt)==4] <- paste(bs.txt[nchar(bs.txt)==4],"0", sep="")

  ses <- input.se %>% round(., 2)
  ses.txt <- as.character(ses)
  ses.txt[nchar(ses.txt)==1] <- paste(ses.txt[nchar(ses.txt)==1],".00", sep="")
  ses.txt[nchar(ses.txt)==3] <- paste(ses.txt[nchar(ses.txt)==3],"0", sep="")
  ses.txt <- paste("(", ses.txt, ")", sep="")



  output <- paste(bs.txt, " ",ses.txt, input.pcode, sep="")
  output <- t(t(output))
  rownames(output) <- rownames(coef(summary(input)))
  if(endsummary==T){
    pr2 <- modEvA::RsqGLM(input)[1]
    pr2 <- as.character(round(as.numeric(pr2), 2))
    pr2[nchar(pr2)==1] <- paste(pr2[nchar(pr2)==1],".00", sep="")
    pr2[nchar(pr2)==3] <- paste(pr2[nchar(pr2)==3],"0", sep="")
    pr2.txt <- paste("CS-PR2 = ",pr2, sep="")
    output <- rbind(output, pr2.txt)
    rownames(output)[nrow(output)] <- ""
  }
  return(output)
}

