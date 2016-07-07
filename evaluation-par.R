#
# rdd_parallel evaluation
# script for comparison between rdd package and its novel parallel version
#
# place this script at a folder named ~/rdd_parallel
# 
# Auxiliary resource for paper:
# Brazil's Bolsa Familia and Young Adult Workers: A Parallel RDD Approach to Large Datasets
#
# Requires: rdestimate_par.R and functions_par.R. Both must be in '~/rdd_parallel' folder

require(rdd)
library(doMC)
require(sm)
require(rdrobust)

#Bolsa Familia data is not avaliable BUT, previous results are. Goto to line 103
#load("~/rdd_parallel/par_pessoa.rda")

source('~/rdd_parallel/rdestimate_par.R')
source('~/rdd_parallel/functions_par.R')

n = 20 #Number of steps

#Dataframes to hold the test result data

#Sequential
rd_res = data.frame(size = 1:n, time= 1:n, est=1:n, bw= 1:n,ci1=1:n,  ci2=1:n)


#2 cores
rdp2_res = data.frame(size = 1:n, time= 1:n, est=1:n, bw= 1:n,ci1=1:n,  ci2=1:n)

#5 cores
rdp5_res = data.frame(size = 1:n, time= 1:n, est=1:n, bw= 1:n,ci1=1:n,  ci2=1:n)

#10 cores
rdp10_res = data.frame(size = 1:n, time= 1:n, est=1:n, bw= 1:n,ci1=1:n,  ci2=1:n)

for (i in 1:n) {
    if (i==n) {
      amostra <- pess #Full size sample
    } else {
      amostra <- pess[sample(nrow(pess),i*nrow(pess)/n),]
    }
    print(paste ("Sample Size:",nrow(amostra) ))

    y = amostra$VLR_RAIS
    x = amostra$DIAS18A
    z = ifelse(amostra$VLR_VARIAVEL_017_2014>0,0,1)
    
    #Sequential
    rd.t <- system.time(rd<-RDestimate(y~x+z))
    rd_res$size[i] = nrow(amostra)
    rd_res$time[i] = rd.t[3]
    rd_res$est[i]  = rd$est[1]
    rd_res$bw[i]   = rd$bw[1]
    rd_res$ci1[i]  = rd$ci[1,1]
    rd_res$ci2[i]  = rd$ci[1,2]
    print(paste ("Seq Time:",rd.t[3] ))
    
    #2 cores
    registerDoMC(2)
    rdp2.t <- system.time(rdp2<-RDestimate.par(y~x+z))
    rdp2_res$size[i] = nrow(amostra)
    rdp2_res$time[i] = rdp2.t[3]
    rdp2_res$est[i]  = rdp2$est[1]
    rdp2_res$bw[i]   = rdp2$bw[1]
    rdp2_res$ci1[i]  = rdp2$ci[1,1]
    rdp2_res$ci2[i]  = rdp2$ci[1,2]
    print(paste ("Par2 Time:",rdp2.t[3] ))

    #5 cores
    registerDoMC(5)
    rdp5.t <- system.time(rdp5<-RDestimate.par(y~x+z))
    rdp5_res$size[i] = nrow(amostra)
    rdp5_res$time[i] = rdp5.t[3]
    rdp5_res$est[i]  = rdp5$est[1]
    rdp5_res$bw[i]   = rdp5$bw[1]
    rdp5_res$ci1[i]  = rdp5$ci[1,1]
    rdp5_res$ci2[i]  = rdp5$ci[1,2]
    print(paste ("Par2 Time:",rdp5.t[3] ))
    
    #5 cores
    registerDoMC(10)
    rdp10.t <- system.time(rdp10<-RDestimate.par(y~x+z))
    rdp10_res$size[i] = nrow(amostra)
    rdp10_res$time[i] = rdp10.t[3]
    rdp10_res$est[i]  = rdp10$est[1]
    rdp10_res$bw[i]   = rdp10$bw[1]
    rdp10_res$ci1[i]  = rdp10$ci[1,1]
    rdp10_res$ci2[i]  = rdp10$ci[1,2]
    print(paste ("Par2 Time:",rdp10.t[3] ))
    
}

#Save the results for future use
#save(rd_res, file="bf_rd_res.rda")
#save(rdp2_res, file="bf_rdp2_res.rda")
#save(rdp5_res, file="bf_rdp5_res.rda")
#save(rdp10_res, file="bf_rdp10_res.rda")

#Previous results are provided
load(file="bf_rd_res.rda")  #sequential execution time, bandwidth and estimates
load(file="bf_rdp2_res.rda") #2cores-parallel execution time, bandwidth and estimates
load(file="bf_rdp5_res.rda") #5cores-parallel execution time, bandwidth and estimates
load(file="bf_rdp10_res.rda") #10cores-parallel execution time, bandwidth and estimates

#load(file="bf_rd_res.rda")
#load(file="bf_rdp_res.rda")

pdf(file="~/evaluation-par10-2.pdf",width=15,height=5)

#plot some graphics
par(mfrow=c(1,3))

#time
par(mar=c(4.5, 5, 3.5, 2))
plot(x=rd_res$size,y=rd_res$time, type="o", 
     col = "black",cex = .3, lty = 1, lwd=2, 
     main = "(a)" , ylab="Execution Time (s)",xlab=" Sample Size",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)
lines(x=rdp2_res$size, y=rdp2_res$time,type="o",col = "black",cex = .3, lty = 2, lwd=2)
lines(x=rdp5_res$size, y=rdp5_res$time,type="o",col = "black",cex = .3, lty = 3, lwd=2)
lines(x=rdp10_res$size,y=rdp10_res$time,type="o",col = "black",cex = .3, lty = 4, lwd=2)
legend(x="topleft",legend=c("sequential", "n_cores=2","n_cores=5","n_cores=10"), 
       lty= c(1,2,3,4), lwd=c(2,2,2,2), col = c("black","black","black","black"), seg.len=2, cex = 2.2,text.width = strwidth("n_cores=100000000000") )


#Bandwidths
par(mar=c(4.5, 5, 3.5, 2))
plot(x=rd_res$size,y=rd_res$bw, type="o", 
     col = "black",cex = .3, lty = 1, lwd=2,
     ylim=c(min(c(rd_res$bw, rdp10_res$bw)), max(c(rd_res$bw, rdp10_res$bw))),
     main = "(b)" , ylab="Bandwith",xlab=" Sample Size",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)

#Estimates
par(mar=c(4.5, 5, 3.5, 2))

plot(x=rd_res$size,y=rd_res$est, type="o", 
         col = "black",cex = .3, lty = 1, lwd=2,
     ylim=c(-75, 125),
     main = "(c)" , ylab="Estimates & Confidence Intervals",xlab="Sample Size",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)
lines(x=c(0,max(rdp10_res$size+1000)),y=c(0,0),type="l",col = "black",cex = .3, lty = 3, lwd=1)

lines(x=rd_res$size,y=rd_res$ci1, type="o", 
      col = "black",cex = .3, lty = 2, lwd=1 )
lines(x=rd_res$size,y=rd_res$ci2, type="o", 
      col = "black",cex = .3, lty = 2, lwd=1 )
lines(x=c(min(rdp10_res$size),max(rdp10_res$size)),y=c(0,0),type="o",col = "gray",cex = .5, lty = 3, lwd=1)
legend(x="topright",legend=c("estimates", "c. interval"), lty= c(1,2), lwd=c(2,1), col = c("black","black"), seg.len=2, cex = 2.2,text.width = strwidth("c. interval0000000000"))

dev.off()
