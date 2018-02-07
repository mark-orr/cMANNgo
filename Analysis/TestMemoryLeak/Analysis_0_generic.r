#COMPARING THE OUTPUT FROM THE NEW FUNCTION PLAUT WROTE FOR LOADING EXAMPLES TO THE OLD ONE#THE DATA ARE FINE WRT TO THE OUTPUTS#SEE PASTED RESULTS BELOW
rm(list=ls())

#MULTIRUN
d.m <- read.table("./multirun_history_0.txt",skip=2)

#MULTIRUNFIXED
d.mf <- read.table("./multirunFixed_history_0.txt",skip=2)

d.m.out <- d.m[,(length(names(d.m))-39):length(names(d.m))]
d.mf.out <- d.mf[,(length(names(d.mf))-39):length(names(d.mf))]

d.comp <- d.m.out-d.mf.out
max(d.comp)
#>[1] 1e-05

d.comp.v <- as.vector(as.matrix(d.comp))
plot(d.comp.v,ylim=c(0,0.00001))
mean(d.comp.v)
#>[1] 8.409091e-09

d.comp.v.bin <- d.comp.v!=0
d.comp.v.bin.mean <- mean(d.comp.v.bin)
d.comp.v.bin.mean
#>[1] 0.004977273

#EOF
