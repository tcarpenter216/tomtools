### z-test comparing two correlations
r.cont <- function(r1, r2, n1, n2) {
  rprime1 <-  0.5 * (log(1+r1) - log(1-r1))
  rprime2 <-  0.5 * (log(1+r2) - log(1-r2))
  z <- (rprime1 - rprime2) / sqrt((1/(n1+3))+(1/(n2+3)))
  p <- 1-pnorm(abs(z))
  return(list(z=z, p=p))
}

r.comp <- function(x1, y1, x2, y2) {
  r1 <- corr.test(data.frame(x1, y1))$r[2,1]
  n1 <- corr.test(data.frame(x1, y1))$n[2,1]
  r2 <- corr.test(data.frame(x2, y2))$r[2,1]
  n2 <- corr.test(data.frame(x2, y2))$n[2,1]
  rprime1 <-  0.5 * (log(1+r1) - log(1-r1)) 
  rprime2 <-  0.5 * (log(1+r2) - log(1-r2)) 
  z <- (rprime1 - rprime2) / sqrt((1/(n1+3))+(1/(n2+3)))
  p <- 1-pnorm(abs(z))
  return(list(z=z, p=p))
}


##### TTEST
ttest <- function(m1, sd1, n1, m2, sd2, n2) {
  diff <- m1 - m2
  df1 <- n1-1
  df2 <- n2-1
  var1 <- sd1^2
  var2 <- sd2^2
  pooled.var <- (df1*var1 + df2*var2) / (df1 + df2)

  se <- sqrt(pooled.var*((n1 + n2)/(n1*n2)))

  d <- (diff / sqrt(pooled.var))
  t <- diff / se
  df <- n1+n2-2
  p <- 2*(1-pt(abs(t),df=df))

  CI.lower <- diff - qt(.975, df)*se
  CI.upper <- diff + qt(.975, df)*se

  return(list(t=t, df=df, p=p, d = d, se = se, pooled.var = pooled.var, diff=diff, CI.lower = CI.lower, CI.upper = CI.upper))

}

welchttest <- function(m1, sd1, n1, m2, sd2, n2) {
  diff <- m1 - m2
  df1 <- n1-1
  df2 <- n2-1
  var1 <- sd1^2
  var2 <- sd2^2
  pooled.var <- (df1*var1 + df2*var2) / (df1 + df2)

  se <- sqrt(var1/n1 + var2/n2)


  d <- (diff / sqrt(pooled.var))
  t <- diff / se
  df <- (var1/n1 + var2/n2)^2 / ((1/df1)*(var1/n1)^2 + (1/df2)*(var2/n2)^2)
  p <- 2*(1-pt(abs(t),df=df))

  CI.lower <- diff - qt(.975, df)*se
  CI.upper <- diff + qt(.975, df)*se

  return(list(t=t, df=df, p=p, d = d, se = se, pooled.var = pooled.var, diff=diff, CI.lower = CI.lower, CI.upper = CI.upper))

}


##### P VALUE FOR CHI SQUARE
chsq <- function(chi, df){
  return(p = 1-pchisq(abs(chi), df))
}

chsq(-1.277,2)


#### CORRELATION MATRIX FORMATTING
format.rmatrix <- function(input, low.only=T) {
  library('stringr')
  char <- as.numeric(input$r)
  char <- round(char, 2)
  char <- as.character(char)
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
  char[!star.3 & !star.2 & !star.1 & !plus.1] <- paste(char[!star.3 & !star.2 & !star.1 & !plus.1], "_", sep="")

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

####### CREATE CORRELATED DATA
cor.data.random <- function(r, n, reps=100, mean.x=0, sd.x=1, mean.y = 0, sd.y=1, min.x=NA, max.x=NA, min.y=NA, max.y=NA){

  uncorrelated.data <- function(n, reps=100){
    x <- matrix(rep(0, n*reps), nrow=n)
    y <- matrix(rep(0, n*reps), nrow=n)
    cors <- numeric()

    # generate reps number of random datasets of size n, and then pick one with correlation of closest to zero
    for (i in 1:reps) {
      x[,i] <- rnorm(n, mean=0, sd=1)
      y[,i] <- rnorm(n, mean=0, sd=1)
      cors[i] <- cor(x[,i],y[,i])
    }

    smallest <- which(abs(cors) == min(abs(cors))) # gives the dataset with the smallest correlation (closest to zero)

    # use smallest to make a correlated dataset
    x <- x[,smallest]
    y <- y[,smallest]
    cor(x,y)

    return(list(x=x, y=y, cor=cor(x,y)))
  }

  data <- uncorrelated.data(n=n, reps=reps)
  x <- data$x
  y <- data$y

  y <- r*x + sqrt(1-r^2) * y

  y <- y*sd.y + mean.y
  x <- x*sd.x + mean.x

  if (!is.na(min.x)) { x[x < min.x] <- NA  }
  if (!is.na(min.y)) { y[y < min.y] <- NA  }
  if (!is.na(max.x)) { x[x > max.x] <- NA  }
  if (!is.na(max.y)) { y[y > max.y] <- NA  }

  return(list(cor=cor(x,y, use="pairwise.complete.obs"), x=x, y=y))
}




####### FREQ COMMANDS

# Freq table command, 12-11-13
freq.spss <- function(input, sort=FALSE, missing=TRUE, cum=TRUE) {
  tab <- table(input)
  #if (sort == FALSE) {  if(is.character(input)) { sort <- TRUE } } #override disabled
  if (sort == TRUE) {tab <- sort(tab, decreasing=TRUE)}
  rows <- attributes(tab)$dimnames$input # save rownames
  tab <- as.vector(tab) # convert to vector
  if (missing == TRUE) {
    num.missing <- sum(is.na(input))
    tab <- c(tab,num.missing)
    rows <- c(rows,"Missing")
  }
  tab2 <- tab/sum(tab)*100
  tab3 <- tab2
  if(length(tab) > 1) {
    for(i in 2:length(tab) ){
      tab3[i] = tab2[i] + tab3[i-1]
    }
  }
  spsstable <- cbind(tab,tab2,tab3)
  colnames(spsstable) <- c("n","%","Cum %")
  rownames(spsstable) <- rows
  if (cum == FALSE) {spsstable <- spsstable[,-3]}
  return(round(spsstable, 0))
}


xtable.spss2<- function(input, sort=FALSE, missing=TRUE, cum=TRUE){
  tab <- freq.spss(input, sort=sort, missing=missing)
  out.align <- c("l","c","c","c")
  out.row <- dim(tab)[1]
  out.col <- dim(tab)[2] + 1
  out.dig <- matrix(0, nrow=out.row, ncol=out.col)
  if (cum == FALSE) {
    tab <- tab[,-3]
    out.dig <- out.dig[,-ncol(out.dig)]
    out.align <- out.align[-length(out.align)]
  }
  xtable(tab, digits=out.dig, align=out.align)
}



xtable.compare <- function(vars, group, title="%", percent=TRUE){
  if (!is.list(vars)) {
    tab <- as.matrix(tapply(vars, group, mean, na.rm=TRUE))
    colnames(tab) <- title
    out.align <- c("l","c")
  }
  if (is.list(vars)) {
    tab <- matrix(0, length(levels(group)), length(vars))
    for (i in 1:length(vars)){
      tab[,i] <- as.matrix(tapply(as.numeric(vars[[i]]), group, mean, na.rm=TRUE))
    }
    colnames(tab) <- title
    rownames(tab) <- levels(group)
    out.align <- c("l", rep("c", length(vars)))
  }
  if(percent ==TRUE) {tab <- tab*100}
  out.row <- dim(tab)[1]
  out.col <- dim(tab)[2] + 1
  if(percent==TRUE) { out.dig <- matrix(0, nrow=out.row, ncol=out.col)}
  if(percent==FALSE) { out.dig <- matrix(2, nrow=out.row, ncol=out.col)}
  xtable(tab, digits=out.dig, align=out.align)
}

view.compare <- function(vars, group, title="%", percent=TRUE){
  if (!is.list(vars)) {
    tab <- as.matrix(tapply(vars, group, mean, na.rm=TRUE))
    colnames(tab) <- title
    out.align <- c("l","c")
  }
  if (is.list(vars)) {
    tab <- matrix(0, length(levels(group)), length(vars))
    for (i in 1:length(vars)){
      tab[,i] <- as.matrix(tapply(as.numeric(vars[[i]]), group, mean, na.rm=TRUE))
    }
    colnames(tab) <- title
    rownames(tab) <- levels(group)
    out.align <- c("l", rep("c", length(vars)))
  }
  if(percent ==TRUE) {tab <- tab*100}
  if(percent==TRUE) { tab <- round(tab,0) }
  if(percent==FALSE) { tab <- round(tab,2)}
  return(tab)
}







##### Graphics related
nobins <- function(dat, num){
  return(seq(min(dat, na.rm=TRUE), max(dat, na.rm=TRUE), length.out=num))
}

###### Converts factor to dummy variable with user-provided names

dummy.var.req <- function(input) {
 newvec <- rep("A",length(input)) # define character vector of length = input
 for (i in 1:length(levels(input))) {
    current <- levels(input)[i]
    newvec[input == current] <-readline(paste("Define ",as.character(current),": "))
 }
 alt <- as.numeric(input)
 names(alt) <- newvec
 print(alt)
}



## Create named numeric dummy vector from character or factor
dummy.var <- function(input){
  alt <- as.numeric(as.factor(input))
  names(alt)<-as.character(input)
  alt <- alt - 1
  return(alt)
}


## Renames the levels of a factor. Asks user for individual names:
ren.levels <- function(input) {
    for (i in 1:length(levels(input))) {
    levels(input)[i] <- readline(paste("Define ",levels(input)[i],": "))
    }
  print(input)
}


###### Column Number function
coln <- function(X){
  y <- rbind(seq(1,ncol(X)))
  colnames(y) <- colnames(X)
  rownames(y) <- "col.number"
  return(as.data.frame(t(y)))
}


###### Recoding and scale scoring
recode <- function(df, startvar, endvar, rev.nums, scale.min=NA, scale.max=NA) {
  # col num function. Returns column numbers by var name.
  col.num <- function(df){
    var.nums <- seq(1,ncol(df))
    names(var.nums) <- colnames(df)
    return(var.nums)
  }
  # get original data
  start.num <- as.numeric(col.num(df)[startvar])
  end.num <- as.numeric(col.num(df)[endvar])
  orig.dat <- df[,start.num:end.num]
  final.dat <- orig.dat
  # report raw column numbers examined
  cols.examined <- start.num:end.num
  # report raw column numbers for recoded items
  cols.recoded <- rev.nums + start.num - 1
  # autodetect and correct scale range if either is not specified in call
  if ( is.na(scale.min) | is.na(scale.max) ) {
    scale.range <- range(orig.dat)
    scale.min <- scale.range[1]
    scale.max <- scale.range[2]
  } else {
    scale.range <- c(scale.min, scale.max)
  }
  # recode scale if scale min is 1
  if (scale.min == 1) {
    final.dat[rev.nums] <- (scale.max + 1) - final.dat[rev.nums]
  }
  # recode scale if scale min is 0
  if (scale.min == 0) {
    final.dat[rev.nums] <- (scale.max) - final.dat[rev.nums]
  }
  # rename recoded vars. Go through cols, if col is on list, rename it.
  for (i in 1:ncol(final.dat)) {
    if( sum(i == rev.nums) == 1){
      colnames(final.dat)[i] <- paste(colnames(final.dat)[i], "r", sep="")
    }
  }
  # save vector of revised names
  rev.names <- colnames(final.dat)
  # finalize output
  recode.out <- list(original=orig.dat, scale.range=scale.range, cols.examined=cols.examined, cols.recoded=cols.recoded, final=final.dat, rev.names=rev.names)
  return(recode.out)
}


# update scores function - revises your dataframe with revised scores with the recoding from the prev function
update.scores <- function(df, recode.output) {
  df[,recode.output$cols.examined] <- recode.output$final
  colnames(df)[recode.output$cols.examined] <- recode.output$rev.names
  return(df)
} ### CONSIDER MAKING THIS AN OPTION OF THE PREVIOUS FUNCTION (ADD update=true, newdf arguments to prev function)


scale.score <- function(df, startvar, endvar, score.nums=NA){
  # col num function. Returns column numbers by var name.
  col.num <- function(df){
    var.nums <- seq(1,ncol(df))
    names(var.nums) <- colnames(df)
    return(var.nums)
  }
  # get original data
  start.num <- as.numeric(col.num(df)[startvar])
  end.num <- as.numeric(col.num(df)[endvar])
  # report raw column numbers examined
  cols.examined <- start.num:end.num
  # infer score.nums if not stated
  if(sum(is.na(score.nums)) != 0) {score.nums <- 1:length(cols.examined)}
  # report raw column numbers in scale
  cols.scored <- score.nums + start.num - 1
  # report data in subscale
  data.scored <- df[,cols.scored]
  sums <- rowSums(data.scored)
  means <- rowMeans(data.scored) * ncol(data.scored)
  # final output
  score.out <- list(data.scored=data.scored, cols.examined=cols.examined, cols.scored=cols.scored, sums=sums, means=means)
  return(score.out)
}

##### CORRELATION AND P-VALUE REPORTING

# APA formats a value in correlation notation
r.format <- function(input) {
  rvalue <- sub('^(-)?0[.]', '\\1.', round(input,2))
  if (nchar(rvalue) == 2) {rvalue <- paste(rvalue, "0", sep="")}
  return(rvalue)
}


# APA formats a correlation from two variables
r.val <- function(var1, var2) {
  rvalue <- sub('^(-)?0[.]', '\\1.', round(corr.test(var1, var2)$r,2))
  if (nchar(rvalue) == 2) {rvalue <- paste(rvalue, "0", sep="")}
  return(rvalue)
}


# APA formats a p-value from input
p.format <- function(input) {
  pvalue <- input
  disp.p <- ""
  if (pvalue < .001) {disp.p <- "$<$ .001"}
  if (pvalue >= .001 & pvalue <= .0015) {disp.p <- "= .001"}
  if (pvalue > .0015 & pvalue <.01) {disp.p <- paste("=", sub('^(-)?0[.]', '\\1.', round(pvalue, 3)))}
  if (pvalue >= .01) {disp.p <- paste("=", sub('^(-)?0[.]', '\\1.', round(pvalue, 2)))}
  return(disp.p)
}


# APA formats a p-value from two variables
p.val <- function(var1, var2) {
  pvalue <- corr.test(var1, var2)$p
  disp.p <- ""
  if (pvalue < .001) {disp.p <- "$<$ .001"}
  if (pvalue >= .001 & pvalue <= .0015) {disp.p <- "= .001"}
  if (pvalue > .0015 & pvalue <.01) {disp.p <- paste("=", sub('^(-)?0[.]', '\\1.', round(pvalue, 3)))}
  if (pvalue >= .01) {disp.p <- paste("=", sub('^(-)?0[.]', '\\1.', round(pvalue, 2)))}
  return(disp.p)
}

# CALCULATES AND FORMATS POLYCHORIC ALPHA (REQUIRES PSYCH PACKAGE)
alpha.format <- function(vars) {
  poly.dat <- polychoric(vars)
  alpha.dat <- alpha(poly.dat$rho)$total[1]
  alpha.out <- substr(as.character(alpha.dat), start=2, stop=4)
  return(alpha.out)

# CALCULATES AND FORMATS POLYCHORIC OMEGA (REQUIRES PSYCH PACKAGE)
omega.format <- function(vars) {
    poly.dat <- polychoric(vars)
    omega.dat <- omega(poly.dat$rho)$omega.tot
    omega.out <- substr(as.character(omega.dat), start=2, stop=4)
    return(omega.out)
  }
}


######## CI.p

ci.p <- function(pbar, n){
  std.e <- sqrt(pbar*(1-pbar)/n)
  me.e <- qnorm(.975) * std.e
  ci.l <- pbar-me.e
  ci.u <- pbar+me.e
  return(round(100*me.e,1))
}
