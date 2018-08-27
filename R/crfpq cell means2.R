cellmeans.crf.pq <- function(y, a, b){


  p <- length(levels(a))
  q <- length(levels(b))
  h <- p*q
  N <- length(a)

  #### DESIGN MATRIX ####

  #empty design matrix
  x <- matrix(
    rep(rep(0, h), N),
    byrow=TRUE, nrow=N, ncol=h)


  #fill in design matrix
  colnum <- 0
  names <- character()
  j <- 1
  k <- 2
  for (j in 1:p){
    for (k in 1:q){

      #increment colnum
      colnum <- colnum+1

      #put a 1 into the design matrix for those rows and that col
      x[a==levels(a)[j] & b==levels(b)[k], colnum] <- 1

      names <- c(names, paste("a",j,"b",k, sep=""))
    }
  }
  colnames(x) <- names
  x

  #### J MATRIX ####

  J <- matrix(rep(1, N), nrow=N) %*% matrix(rep(1, N), ncol=N)

  #### HYP MATRIX ####
  hyp.matrix <- function(num) {
    return(t(as.matrix(
      diag(num)[1:(num-1),] +
        cbind(matrix(rep(0,(num-1)),ncol=1), -1*diag(num)[1:(num-1),1:(num-1)]))
    ))
  }

  H.a <- hyp.matrix(p)
  A1 <- matrix(rep(1, p), ncol=p)
  H.b <- hyp.matrix(q)
  B1 <- matrix(rep(1, q), ncol=q)
  C.a <- t(kronecker(t(H.a) , B1))
  C.b <- t(kronecker(A1, t(H.b)))
  C.ab <- t(kronecker(t(H.a) , t(H.b)))

  #### TERMS ####
  u <- solve(t(x)%*%x) %*% t(x)%*%y
  SSA <- t(t(C.a)%*%u) %*% solve( t(C.a) %*% solve(t(x)%*%x) %*% C.a ) %*% t(C.a)%*%u
  SSB <- t(t(C.b)%*%u) %*% solve( t(C.b) %*% solve(t(x)%*%x) %*% C.b ) %*% t(C.b)%*%u
  SSAB<- t(t(C.ab)%*%u) %*% solve( t(C.ab) %*% solve(t(x)%*%x) %*% C.ab ) %*% t(C.ab)%*%u
  SSWG <- t(y)%*%y - t(u) %*% t(x) %*% y
  SSTO <- t(y)%*%y - t(y) %*% J %*% y * (1/N)

  dfa <- p-1
  dfb <- q-1
  dfab <- (p-1)*(q-1)
  dfwg <- N-h
  dfto <- dfa + dfb + dfab + dfwg

  MSA <- SSA / dfa
  MSB <- SSB / dfb
  MSAB <- SSAB / dfab
  MSWG <- SSWG / dfwg

  FA <- MSA/MSWG
  FB <- MSB/MSWG
  FAB <- MSAB/MSWG

  p.A <- 1-pf(FA, dfa, dfwg)
  p.B <- 1-pf(FB, dfb, dfwg)
  p.AB <- 1-pf(FAB, dfab, dfwg)

  o2.A <- ((p-1)*(FA-1))/((p-1)*(FA-1) + N)
  o2.B <- ((q-1)*(FB-1))/((q-1)*(FB-1) + N)
  o2.AB <- ((p-1)*(q-1)*(FAB-1))/((p-1)*(q-1)*(FAB-1) + N)

  tab<-rbind(
    cbind(SSA, dfa, MSA, FA, p.A, o2.A),
    cbind(SSB, dfb, MSB, FB, p.B, o2.B),
    cbind(SSAB, dfab, MSAB, FAB, p.AB, o2.AB),
    cbind(SSWG, dfwg, NA, NA, NA, NA),
    cbind(SSTO, dfto, NA, NA, NA, NA)
  )
  colnames(tab) <- c("SS", "df", "MS", "F", "p", "omega2")
  rownames(tab) <- c("A", "B", "AB", "WG", "TOTAL")
  tab

  tab2 <- aggregate(y, list(a, b), mean, na.rm=T)
  tab2 <- cbind(tab2, aggregate(y, list(a, b), sd, na.rm=T)[3])
  tab2[,3:4] <- round(tab2[,3:4], 3)
  colnames(tab2) <- c("A", "B", "M", "SD")

  return(list(anova=tab, descs=tab2, H.a = H.a, A1=A1, C.a=C.a, H.b=H.b, B1=B1, H.b=H.b, C.ab=C.ab, design=x))
}

#library(phia)
#cellmeans.crf.pq(y=Boik$edr, a=Boik$therapy, b=Boik$medication)
