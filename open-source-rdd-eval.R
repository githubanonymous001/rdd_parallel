#
# rdrobust and rdd packages evaluation script
# includes simulated data generation
#
# place this script at a folder named ~/rdd_parallel
# 
# Auxiliary resource for paper:
# Brazil's Bolsa Familia and Young Adult Workers: A Parallel RDD Approach to Large Datasets

require(rdrobust)
require(rdd)
require(doMC)
require(sm)

#
# Simulated data generator
#
simulator=function(n, cutPoint=0, impact=5000, fix.stddev=4000, var.stdev=8000, xmin=-100, xmax=100,coef3=0,coef2=0.3,coef1=90,coef0=2000)
{
  set.seed(171)
  sim.data = matrix(c(NA), nrow= n, ncol=5 )
  sim.data = data.frame(sim.data)
  
  sx = rnorm(n,cutPoint,(xmax-xmin) ) 
  sim.data[,1] = ifelse(sx >= xmin &  sx <= xmax, sx, runif(n, min=xmin, max=xmax)) 
  sim.data[,3] = round(ifelse(sim.data[,1] >= cutPoint,1,0) * runif(n)) #Treatment
  sim.data[,4] = round(runif(n, min =1, max=5))
  sim.data[,5] = round(runif(n, min =1, max=5))
  sim.data[,2] = (sim.data[,1]^3)*coef3 +(sim.data[,1]^2)*coef2 + sim.data[,1]*coef1 + coef0 + 
    rnorm(n,0,fix.stddev)  + 
    ((sim.data[,1] - cutPoint)/(xmax-xmin)) * rnorm(n,0,var.stdev) +
    impact*sim.data[,3] + 
    impact*(sim.data[,4]-3) /2 +
    impact*(sim.data[,5]-3) /3
  names(sim.data) <- c('x', 'y', 'T','Z1','Z2')
  
  sim.data
}

# Generate simulated data
sim = simulator(40000)


# Extract small sub sample
samp <- sim[sample(nrow(sim),1000),]

y = samp$y
x = samp$x
z = samp$T

# Save plots in pdf file - Figure 1
pdf(file="~/sim-data-2.pdf",width=15,height=5)

par(mfrow=c(1,3))
par(mar=c(4.5, 5, 3.5, 2))


# Plot raw data 
plot(x,y, main="(a)", xlab="Running Var (X)", ylab="Outcome (Y)",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0 )

# Extract larger sub sample
samp <- sim[sample(nrow(sim),8000),]

y = samp$y
x = samp$x
z = samp$T

# Plot probable discontinuity
rdplot(x=x,y=y, title="(b)", x.label="Running Var (X)", y.label="Outcome (Y)", col.dots = "gray", col.lines = "black", type.dots=1,
       cex.main=3.2, cex.lab=2.6, cex.axis=2.0)
sm_l <- sm.regression(x[x <0], z[x<0], display="none"  )  
sm_r <- sm.regression(x[x >=0], z[x>=0], h=20, display="none"  )  

# Treatment probability - "Fuzzy Nature" of the design
plot(c(sm_l$eval.points,sm_r$eval.points),c(sm_l$estimate,sm_r$estimate),type="l", main="(c)", ylab="Treatment Assigment (W)", xlab="Running Var (X)",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)

# Close pdf file
dev.off()

#
# rdd and rdrobust evaluation over different sample sizes
#
n=40    #number of steps
k=5000  #step size

# dataframes to hold results
rd_res = data.frame(size = 1:n, time= 1:n, est=1:n, bw= 1:n,ci1=1:n,  ci2=1:n)
rdr_res = data.frame(size = 1:n, time= 1:n, est=1:n, bw= 1:n,ci1=1:n,  ci2=1:n)

#evaluation loop
for(i in 1:n ) {

  #get a simulated sample of size k*i  
  sim = simulator(k*i)

  y = sim$y
  x = sim$x
  z = sim$T

  #executes the rdd package estimation and collect results 
  rd.t <- system.time(rd<-RDestimate(y~x+z))
  rd_res$size[i] = i*k
  rd_res$time[i] = rd.t[3]
  rd_res$est[i] = rd$est[1]
  rd_res$bw[i] = rd$bw[1]
  rd_res$ci1[i] = rd$ci[1,1]
  rd_res$ci2[i] = rd$ci[1,2]

  #executes the rdrobust package estimation and collect results 
  rdr.t <- system.time(rdr<-rdrobust(y,x,fuzzy=z))
  rdr_res$size[i] = i*k
  rdr_res$time[i] = rdr.t[3]
  rdr_res$est[i] = rdr$coef[1,1]
  rdr_res$bw[i] = rdr$bws[1,1]
  rdr_res$ci1[i] = rdr$ci[1,1]
  rdr_res$ci2[i] = rdr$ci[1,2]
}

# save results for future use
save(rd_res, file="~/rdd_parallel/rd_res.rda")
save(rdr_res, file="~/rdd_parallel/rdr_res.rda")

#uncomment to retrieve previuos saved results
#load(file="~/rdd_parallel/rd_res.rda")
#load(file="~/rdd_parallel/rdr_res.rda")

# pdf file for figure 2
pdf(file="~/comparison-2.pdf",width=15,height=5)

par(mar=c(4.5, 5, 3.5, 2))

par(mfrow=c(1,3))

# plots execution time comparison
plot(x=rd_res$size,y=rd_res$time, type="l", 
     col = "dark gray",cex = .5, lty = 1, lwd=3, 
     main = "(a)" , ylab="Execution Time (s)",xlab=" Sample Size",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)
lines(x=rdr_res$size,y=rdr_res$time,type="l",col = "black",cex = .5, lty = 2, lwd=2)
legend(x="topleft",legend=c("rdd", "rdrobust"), lty= c(1,2), lwd=c(3,2), col = c("dark gray","black"),
       seg.len=2, cex = 2.2,text.width = strwidth("rdrobust0000000000"))


# plots bandwidths
plot(x=rd_res$size,y=rd_res$bw, type="l", 
     col = "dark gray",cex = .5, lty = 1, lwd=3,
     ylim=c(min(c(rd_res$bw, rdr_res$bw)), max(c(rd_res$bw, rdr_res$bw))),
     main = "(b)" , ylab="Bandwith",xlab=" Sample Size",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)

lines(x=rdr_res$size,y=rdr_res$bw, type="l", 
     col = "black",cex = .5, lty = 2, lwd=2 )
lines(x=rdr_res$size,y=rdr_res$bw,type="l",col = "black",cex = .5, lty = 2, lwd=2)
legend(x="topright",legend=c("rdd", "rdrobust"), lty= c(1,2), lwd=c(3,2), col = c("dark gray","black"),
       seg.len=2, cex = 2.2,text.width = strwidth("rdrobust0000000000"))

# plots Estimates

# target impact horizontal line
plot(x=c(0,200000),y=c(5000,5000), type="l", 
     col = "black",cex = .7, lty = 3, lwd=1,
     ylim=c(min(c(rd_res$ci1, rdr_res$ci1)), max(c(rd_res$ci2, rdr_res$ci2))),
     main = "(c)" , ylab="Estimates & Confidence Intervals",xlab=" Sample Size" ,
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)

# rdd results
lines(x=rd_res$size,y=rd_res$est, type="l", 
      col = "dark gray",cex = .5, lty = 1, lwd=3 )
lines(x=rd_res$size,y=rd_res$ci1, type="l", 
      col = "dark gray",cex = .5, lty = 1, lwd=2 )
lines(x=rd_res$size,y=rd_res$ci2, type="l", 
      col = "dark gray",cex = .5, lty = 1, lwd=2 )

# rdd results
lines(x=rdr_res$size,y=rdr_res$est, type="l", 
     col = "black",cex = .5, lty = 2, lwd=2 )
lines(x=rdr_res$size,y=rdr_res$ci1, type="l", 
      col = "black",cex = .5, lty = 2, lwd=1 )
lines(x=rdr_res$size,y=rdr_res$ci2, type="l", 
      col = "black",cex = .5, lty = 2, lwd=1 )


legend(x="topright",legend=c("rdd", "c. int.", "rdrobust","c. int."), lty= c(1,1,2,2), lwd=c(3,2,2,1), col = c("dark gray","dark gray","black","black"),
       seg.len=2, cex = 2.2,text.width = strwidth("rdrobust0000000000"))

# close pdf file for figure 2
dev.off()

