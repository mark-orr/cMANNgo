#THIS COMES FROM <temp_meta_II_SNA.r see ~/MANN/ANalysis/BetaTestingForFIrstParameterSpaceSweep for actual copy>
#ORIGINALLY DEVELOPED TO CAPTURE ATTITUDE CLUSTERING ON MANN PARAM SPACE LARGE SCALE RUN
#GEN 12-11-15

#LIBRARIES
library(igraph)
library(rgexf)


rm(list=ls())
wd <- "./output/"

#LOOKING FOR RUN #_rXX IS EQUAL TO I
for(i in 0:0){
    
    rm(a)
    #a <- formatC(i,width=2,format="d",flag="0")
    setwd(wd) 
    d <- read.csv("./network_of_agents.pout",header=F)
    
    #GET MEAN, SD AND RANGE
    mean.for.d <- apply(d[,4:13],1,mean)
    sd.for.d <- apply(d[,4:13],1,sd)
    min.for.d <- apply(d[,4:13],1,min)
    max.for.d <- apply(d[,4:13],1,max)
    mean.for.d.neg <- apply(d[,4:8],1,mean)
    mean.for.d.pos <- apply(d[,9:13],1,mean)
    
    d <- cbind(d,mean.for.d,mean.for.d.neg,mean.for.d.pos,sd.for.d,min.for.d,max.for.d)
    
    d$diff.means.pos.neg <- d$mean.for.d.pos-d$mean.for.d.neg
    d$rank.diff <- "A"
    d$rank.diff[d$diff.means.pos.neg>-.25] <- "B"
    d$rank.diff[d$diff.means.pos.neg>0] <- "C"
    d$rank.diff[d$diff.means.pos.neg>.25] <- "D"

    d$node.name <- NA
    d$node.name <- paste("A",d$V2,sep="")

    rm(assort.catch)
    assort.catch <- rep(NA,100)
    for(j in 0:99){ #j is tick number; i
        assign(paste("d.t.",j,sep=""), d[d$V1==j,])
        
        #LOAD NETWORK
        e.list.in <- read.table(gzfile("edge_list.gz"))
        e.list.use <- e.list.in[,1:2]
        gg <- graph.data.frame(e.list.use,directed=FALSE)
        
        
        #ASSIGN)
        V(gg)$state <- get(paste("d.t.",j,sep=""))$diff.means.pos.neg[match(V(gg)$name,get(paste("d.t.",j,sep=""))$node.name)]
        V(gg)$state.cat <- get(paste("d.t.",j,sep=""))$rank.diff[match(V(gg)$name,get(paste("d.t.",j,sep=""))$node.name)]
        
        V(gg)$color=V(gg)$state.cat #assign the state attribute as the vertex color
        V(gg)$color=gsub("A","black",V(gg)$color) 
        V(gg)$color=gsub("B","gray50",V(gg)$color)
        V(gg)$color=gsub("C","gray75",V(gg)$color) 
        V(gg)$color=gsub("D","white",V(gg)$color)
        
        set.seed(12345)
        b <- formatC(j,width=4,format="d",flag="0")
        png(paste("../Run0_Tick_",b,".png",sep=""))
        plot.igraph(gg,vertex.label=NA,vertex.size=4,layout=layout.auto)
        dev.off()
        #NETWORK METRICS WILL GO HERE
        #assort.catch[j] <- assortativity(gg,V(gg)$state,directed=FALSE)
        #write(assort.catch,file=paste("../../MetaGraphs/Assort.Catch",i,".txt",sep=""),ncol=1)
    }
}

#CAN RUN VIDEO GENERATION
#setwd("../../MetaGraphs")
#for(i in 0:9){
#    a <- paste("ffmpeg -framerate 1/0.5 -i Net_Run",i,"_Tick_%04d.png -c:v libx264 -vf fps=25 -pix_fmt yuv420p out_Run",i,".mp4",sep="")
#    system(a)
#}

#PLOTTING
#setwd("~/MANN/Analysis/Exp17/Runs/MetaGraphs/")
#pdf("Assort_Graph.pdf")
#x <- read.table("Assort.Catch0.txt")
#plot(x$V1,type="l",ylim=c(-1,1))
#for(i in 1:9){
#x <- read.table(paste("Assort.Catch",i,".txt",sep=""))
#lines(x$V1,ylim=c(-1,1),lty=i+1)
#text(99,x$V1[99],i)
#}
#dev.off()

#TESTING THE NETWORK METRICS
#i <- 2
#assort.catch <- rep(NA,100)
#for(j in 1:99){ #j is tick number; i
#    assign(paste("d.t.",j,sep=""), d[d$V1==j,])
        
    #LOAD NETWORK
#    e.list.in <- read.table(gzfile("edge_list.gz"))
#    e.list.use <- e.list.in[,1:2]
#    gg <- graph.data.frame(e.list.use,directed=FALSE)
        
        
#    #ASSIGN)
#    V(gg)$state <- get(paste("d.t.",j,sep=""))$diff.means.pos.neg[match(V(gg)$name,get(paste("d.t.",j,sep=""))$node.name)]
#    V(gg)$state.cat <- get(paste("d.t.",j,sep=""))$rank.diff[match(V(gg)$name,get(paste("d.t.",j,sep=""))$node.name)]
        
#    V(gg)$color=V(gg)$state.cat #assign the state attribute as the vertex color
#    V(gg)$color=gsub("A","red",V(gg)$color) 
#    V(gg)$color=gsub("B","black",V(gg)$color)
#    V(gg)$color=gsub("C","blue",V(gg)$color) 
#    V(gg)$color=gsub("D","green",V(gg)$color)
#    assort.catch[j] <- assortativity(gg,V(gg)$state,directed=FALSE)
#    write(assort.catch,file=paste("Assort.Catch",i,".txt",sep=""),ncol=1)
#}



#EOF
