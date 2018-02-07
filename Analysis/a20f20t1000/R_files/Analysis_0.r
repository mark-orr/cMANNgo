#Read in the data


#PROTOTYPE ANALYSIS
#PROTOTYPE ANALYSIS
#PROTOTYPE ANALYSIS
prototypes <- read.table("../prototypes_0.txt")
uber.proto <- prototypes[1,]

#SIMILARITY FUNCTION WITH TWO VECTS
csim.1 <- function(x1,x2){
    x <- as.numeric(x1[2:length(x1)])
    y <- as.numeric(x2[2:length(x2)])
    c <- x %*% y / sqrt(x%*%x * y%*%y)
    return(c)
}
#WORKS

#SIMILARITY FUNCITON WITH ONE VECT COMP TO PROTO
csim.2 <- function(x1){
    x <- as.numeric(x1[2:length(x1)])
    y <- as.numeric(uber.proto[2:length(uber.proto)])
    c <- x %*% y / sqrt(x%*%x * y%*%y)
    return(c)
}

apply(prototypes,1,csim.2)


#HISTORY DATA ANALYSIS  
d.0 <- read.table("../history_0.txt",skip=2)
#REMOVE t0
outputs <- d.0[21:length(d.0[,1]),25:44]

csim.3 <- function(x1){
    x <- as.numeric(x1)
    y <- as.numeric(uber.proto[2:length(uber.proto)])
    c <- x %*% y / sqrt(x%*%x * y%*%y)
    return(c)
}

d.sim <- apply(outputs,1,csim.3)

plot(d.sim,col=2,type="b",lty=2)

#EOF
