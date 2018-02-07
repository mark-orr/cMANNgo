#depends on Analysis_0_generic.r
library(igraph)
library(rgexf)

num.runs <- 1
#i captures run
#for(i in 0:(num.runs-1)){
#testing
i <- 0
    #ARCHIVAL
    #rm(a)
    #a <- formatC(i,width=2,format="d",flag="0")
    
    #OLD d <- read.csv("./network_of_agents.pout",header=F)
    #THESE ARE THE STATES OF THE SYSTEM OVER TIME
    d <- outputs
    #ADDING THE SIMILARITY TO PROTOTYP
    d$sim <- d.sim
    
    d$rank.diff <- "A"
    d$rank.diff[d$sim>.25 & d$sim>=.50] <- "B"
    d$rank.diff[d$sim>.50 & d$sim>=.75] <- "C"
    d$rank.diff[d$sim>.75 & d$sim>=1] <- "D"

    #GRAB AGENT NAME FOR GRAPH FROM d.0
    d$node.name <- NA
    d$node.name <- paste("A",d.0$V2[(no.agents+1):length(d.0[,1])],sep="")

    #THIS CATCH IS ON TICKS
    rm(assort.catch)
    assort.catch <- rep(NA,length(d[,1])
    for(j in 0:99){ #j is tick number; i
        assign(paste("d.t.",j,sep=""), d[d$V1==j,])
        
        #LOAD NETWORK
        e.list.in <- read.table(gzfile("edge_list.gz"))
        #THIS IS THE EDGELIST, SIMPLE READ COLs
        e.list.use <- e.list.in[,1:2]
        #SELECT FIRST TWO COLS
        gg <- graph.data.frame(e.list.use,directed=FALSE)
        #GEN A DIRECTED GRAPH
        
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
        png(paste("../../Run0_Tick_",b,".png",sep=""))
        plot.igraph(gg,vertex.label=NA,vertex.size=4,layout=layout.auto)
        dev.off()
        #NETWORK METRICS WILL GO HERE
        #assort.catch[j] <- assortativity(gg,V(gg)$state,directed=FALSE)
        #write(assort.catch,file=paste("../../MetaGraphs/Assort.Catch",i,".txt",sep=""),ncol=1)
    }
}
