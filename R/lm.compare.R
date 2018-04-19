lm.compare <- function(formulas, data){

  #local function 1: comparison
  format.modch <- function(input){
    Fch <- list()
    dfch1 <- list()
    dfch2 <- list()
    pch <- list()
    R2ch <- list()

    for(j in 2:length(input)){
      temp<-anova(input[[j-1]], input[[j]])
      Fch[[j]]<-temp$F[2]
      dfch1[[j]]<-temp$Df[2]
      dfch2[[j]]<-temp$Res.Df[2]
      pch[[j]]<-temp$`Pr(>F)`[2]
      R2ch[[j]] <- summary(input[[j]])$r.squared - summary(input[[j-1]])$r.squared
    }
    Fch[[1]]<-summary(input[[1]])$fstatistic[1]
    Fch <- lapply(Fch, function(x) round(x, 2))
    dfch1[[1]] <-summary(input[[1]])$fstatistic[2]
    dfch2[[1]] <-summary(input[[1]])$fstatistic[3]
    pch[[1]] <- pf(Fch[[1]],dfch1[[1]],dfch2[[1]],lower.tail=F)
    pch <- lapply(pch, function(x) round(x, 3))
    pfin <- pch
    pfin <- paste("= ",as.character(pch), sep="")
    pfin[pch<.001] <- "< .001"
    R2ch[[1]]<-summary(input[[1]])$r.squared
    R2ch <- lapply(R2ch, function(x) as.character(round(x, 3)))
    R2fin <- paste("= ",as.character(R2ch), sep="")
    R2fin[R2ch<.001] <- "< .001"

    final <- rbind(paste("R2ch ",R2fin, sep=""),
                   paste("F(",as.character(dfch1), ",", as.character(dfch2),") = ",Fch, sep=""),
                   paste("p ",as.character(pfin), sep=""))
    return(final)
  }

  #local function 2: table of betas
  format.betatab <- function(input){
    addna <- function(input, max){
      return(c(input, rep(NA, max-length(input))))
    }

    input.beta <- lapply(input, function(x) coef.lm.beta(lm.beta(x)) )
    input.p <- lapply(input,
                      function(x) {
                        temp<-summary(x)$coefficients[,4]
                        maxrows <- length(coef(input[[length(input)]]))
                        temp2 <- temp
                        temp2[temp>=.10] <- ""
                        temp2[temp<.10] <- "+"
                        temp2[temp<.05] <- "*"
                        temp2[temp<.01] <- "**"
                        temp2[temp<.001] <- "***"
                        return(temp2)
                      })

    input2 <- list(input.beta, input.p)

    final <- list()
    for(i in 1:length(input)){
      betas <- as.character(round(input.beta[[i]],2))
      betas[substr(betas,1,1)!="-"] <- paste(" ",betas[substr(betas,1,1)!="-"], sep="")
      betas[nchar(betas)==4] <- paste(betas[nchar(betas)==4], "0", sep="")
      betas[nchar(betas)==2] <- paste(betas[nchar(betas)==2], ".00", sep="")
      betas <- paste(substr(betas,1,1), substr(betas,3,5), sep="")
      ps <- input.p[[i]]
      paste(betas, ps)
      final[[i]] <- paste(betas, ps, sep="")
    }
    maxrows <- length(coef(input[[length(input)]]))
    final <- lapply(final, function(x){addna(x, maxrows)})
  }

  num.models <- length(formulas)

  #run model on last formula
  temp <- lm(formulas[[num.models]], data=data, na.action="na.exclude")
  skipped <- is.na(residuals(temp))
  rm(temp)
  dat.complete <- dat[!skipped,]
  models <- lapply(formulas, lm, data=dat.complete)


  table.info <- format.betatab(models)
  tab1 <- as.matrix(table.info[[1]])
  for(j in 2:num.models){
    tab1 <- cbind(tab1, as.matrix(table.info[[j]]))
  }
  colnames(tab1) <- paste("Model", seq(1:num.models))
  tab1 <- tab1[-1,] #drop intercept
  rownames(tab1) <- colnames(models[[num.models]]$model)[-1] #grab var names, less the DV, assign to rownames
  tab1 <- rbind(tab1, format.modch(models))
  return(tab1)
}
