#
# Graphic analysis of Bolsa Familia Data
#
# place this script at a folder named ~/rdd_parallel
# 
# Auxiliary resource for paper:
# Brazil's Bolsa Familia and Young Adult Workers: A Parallel RDD Approach to Large Datasets

#bolsa familia data not avaliable at github
load("~/rdd_parallel/par_pessoa.rda")


#remove extreme values to have better graphics, we do not remove any values for the final estimation
ps <- pess[pess$DIAS18A>=-1000&pess$VLR_RAIS<=2000&pess$DIAS18A<=400,]

#small sub sample to not overload the scatter plot
amostra <- ps[sample(nrow(ps),15000),]
y = amostra$VLR_RAIS #outcome (max sal in 2014)
x = amostra$DIAS18A  #running var (age in days)
z = ifelse(amostra$VLR_VARIAVEL_017_2014>0,0,1)  #treatment assignment var 0-control, 1-treatment

pdf(file="~/rdd_parallel/graph-analysis-2.pdf",width=15,height=5)

par(mfrow=c(1,3))
par(mar=c(4.5, 5, 3.5, 2))

#scatter plot
plot(x,y, main="(a)", xlab="Age (days)", ylab="Formal Salary",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)

set.seed(173) #for reprodutibility

#another sub sampling to not overload the remaining plots
amostra <- ps[sample(nrow(ps),200000),]
y = amostra$VLR_RAIS
x = amostra$DIAS18A
z = ifelse(amostra$VLR_VARIAVEL_017_2014>0,0,1)

par(mar=c(4.5, 5, 3.5, 2))

#graphic tool for rdd from rdrobust
rdplot(x=x,y=y, title="(b)", x.label="Age (days)", y.label="Formal Salary",y.lim=c(0,400), col.dots = "gray", col.lines = "black", type.dots=1 ,
       cex.main=3.2, cex.lab=2.6, cex.axis=2.0)


sm_l <- sm.regression(x[x <0], z[x<0], display="none"  )  
sm_r <- sm.regression(x[x >=0], z[x>=0], h=30, display="none"  )  
par(mar=c(4.5, 5, 3.5, 2))


#treatment assignment
plot(c(sm_l$eval.points,sm_r$eval.points),c(sm_l$estimate,sm_r$estimate),type="l", main="(c)", ylab="Treatment Assigment (W)", xlab="Age (days)",
     cex.main=3.2, cex.lab=2.6, cex.axis=2.0)
dev.off()



