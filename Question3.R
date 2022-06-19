library(readr)
library(lattice)
library(rgl)
library(caTools)
library(evaluate)

BABA <- read_csv("D:/RData/FinalTest/BABA.csv")
str(BABA)
summary(BABA)

#声明范围和步骤
lowerBound <- c(n1 =5 , nFact = 3, nSharpe = 22, shThresh = 0.05)
upperBound <- c(n1 = 80, nFact = 3, nSharpe = 22, shThresh = 0.95)
stepSize <- c(n1 = 5, nFact = 1, nSharpe = 1, shThresh = 0.05)
pnames <- names(stepSize)
np <- length(pnames) 

#创建POINTS
POINTS <- list()
for( p in pnames ){
  POINTS[[p]] <- seq(lowerBound[[p]], upperBound[[p]], stepSize[[p]])
}
OPTIM <- data.frame(matrix(NA, nrow = prod(unlist(lapply(POINTS, length))),
                           ncol = np + 1))
names(OPTIM)[1:np] <- names(POINTS)
names(OPTIM)[np+1] <- "obj"




# Declare list of all test points
POINTS <- list()
for( p in pnames ){
  POINTS[[p]] <- seq(lowerBound[[p]], upperBound[[p]], stepSize[[p]])
}

OPTIM <- data.frame(matrix(NA, nrow = prod(unlist(lapply(POINTS, length))),
                           ncol = np + 1))
names(OPTIM)[1:np] <- names(POINTS)
names(OPTIM)[np+1] <- "obj"
# Store all possible combinations of parameters
for( i in 1:np ){
  each <- prod(unlist(lapply(POINTS, length))[-(1:i)])
  times <- prod(unlist(lapply(POINTS, length))[-(i:length(pnames))])
  OPTIM[,i] <- rep(POINTS[[pnames[i]]], each = each, times = times)
}
# Test each row of OPTIM
timeLapse <- proc.time()[3]
for( i in 1:nrow(OPTIM) ){
  OPTIM[i,np+1] <- evaluate(OPTIM[i,1:np], transform = FALSE, y = 2014)
  cat(paste0("## ", floor( 100 * i / nrow(OPTIM)), "% complete\n"))
  cat(paste0("## ",
             round( ((proc.time()[3] - timeLapse) *
                       ((nrow(OPTIM) - i)/ i))/60, 2),
             " minutes remaining\n\n"))
}

wireframe(obj ~ n1*shThresh, data = OPTIM,
          xlab = "n1", ylab = "shThresh",
          main = "Long-Only MACD Exhaustive Optimization",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = 15, x = -60)
)
levelplot(obj ~ n1*shThresh, data = OPTIM,
          xlab = "n1", ylab = "shThresh",
          main = "Long-Only MACD Exhaustive Optimization"
)

