location.mturk <- function(State){

  #region code
  region <- NA
  region[State==49 | State==38 | State==5 | State==2 |State==12] <- 1
  region[State==29 | State==27 | State==13 | State==3 |
           State==46 | State==32 | State==6 | State==52 ] <- 2
  region[State==45 | State==37 | State==4 | State==19] <- 3
  region[State==25| State==1 | State==44 | State==18] <- 4
  region[State==10 | State==11 | State==42 | State==34 | State==48 |
           State==50 | State==9 | State==21 | State==8 | State==40] <- 5
  region[State==35 | State==43 | State==28 | State==17 | State==24 |
           State==16 | State==26] <- 6
  region[State==51| State==14 | State==15 | State==23 | State==36] <- 7
  region[State==33| State==31 | State==39] <- 8
  region[State==7 | State==41 | State==22 | State==47 | State==30 | State==20] <- 9


  geo <- NA
  geo[region==1 | region==2] <- 1
  geo[region==3 | region==4 | region==5] <- 2
  geo[region==6 | region==7] <- 3
  geo[region==8 | region==8] <- 4


  region <- factor(region)
  levels(region) <- c("West - Pacific","West - Mountain", "South - West Central",
                      "South - East Central", "South - South Atlantic", "Midwest - West North Central",
                      "Midwest - East North Central", "Northeast - Middle Atlantic", "Northeast - New England")

  geo <- factor(geo)
  levels(geo) <- c("West", "South", "Midwest", "Northeast")

  return(data.frame(region=region, geo=geo))
}
