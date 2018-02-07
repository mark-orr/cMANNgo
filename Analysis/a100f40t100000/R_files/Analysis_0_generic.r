rm(list=ls())
#PROTOTYPE ANALYSIS AND FEATURE EXTRACTION
prototypes <- read.table("../prototypes_0.txt")
uber.proto <- prototypes[1,2:length(prototypes[1,])]
no.features <- length(uber.proto)
no.agents <- length(prototypes[,1])-1

#HISTORY DATA ANALYSIS
d.0 <- read.table("../history_0.txt",skip=2)
#REMOVE t0
outputs <- d.0[(no.agents+1):length(d.0[,1]),
               (4+no.features+1):(4+no.features+1+no.features-1)]

csim <- function(x1){
    x <- as.numeric(x1)
    y <- as.numeric(uber.proto)
    c <- x %*% y / sqrt(x%*%x * y%*%y)
    return(c)
}

d.sim <- apply(outputs,1,csim)

pdf("test_0.pdf")
plot(d.sim,col=2,type="b",lty=2,
     xlab="tick",ylab="similarity to uber prototype",main=paste("A=",no.agents," F=",no.features,sep=""))
dev.off()

#EOF
