rm(list=ls())
cac.dat <- read.table("C:/hyoju/MC075/data/cacSPLUS.dat", sep="\t", header = T)
cac <- cac.dat$agatpm1c
age <- cac.dat$age1c
male <- factor(cac.dat$gender1, levels = c(0,1), label = c("female", "male"))
race <- factor(cac.dat$race, levels = c(1,2,3,4), label = c("white", "chinese", "black", "hispanic"))
age45 <- age - 45
zerocac <- ifelse(cac == 0, 1, 0)
logcac <- ifelse(cac > 0, log(cac), NA)
summary(logcac)

my.cac <- data.frame(cac = cac, age = age, male = male, race = race, age45 = age45, zerocac = zerocac, logcac = logcac)
summary(my.cac)
dim(my.cac)
save(my.cac, file = "C:\\hyoju\\mc075\\data\\mycac.Rdata")

rm(list=ls())
load("C:\\hyoju\\mc075\\data\\mycac.Rdata")
summary(my.cac)

wm <- my.cac$male == "male" & my.cac$race == "white"
wm1 <- my.cac$male == "male" & my.cac$race == "white" & my.cac$zerocac == 0
cm <- my.cac$male == "male" & my.cac$race == "chinese"
cm1 <- my.cac$male == "male" & my.cac$race == "chinese" & my.cac$zerocac == 0
bm <- my.cac$male == "male" & my.cac$race == "black"
bm1 <- my.cac$male == "male" & my.cac$race == "black" & my.cac$zerocac == 0
hm <- my.cac$male == "male" & my.cac$race == "hispanic"
hm1 <- my.cac$male == "male" & my.cac$race == "hispanic" & my.cac$zerocac == 0
wf <- my.cac$male == "female" & my.cac$race == "white"
wf1 <- my.cac$male == "female" & my.cac$race == "white" & my.cac$zerocac == 0
cf <- my.cac$male == "female" & my.cac$race == "chinese"
cf1 <- my.cac$male == "female" & my.cac$race == "chinese" & my.cac$zerocac == 0
bf <- my.cac$male == "female" & my.cac$race == "black"
bf1 <- my.cac$male == "female" & my.cac$race == "black" & my.cac$zerocac == 0
hf <- my.cac$male == "female" & my.cac$race == "hispanic"
hf1 <- my.cac$male == "female" & my.cac$race == "hispanic" & my.cac$zerocac == 0

##############
## lowess functions
##############

get.lowess7p <- function(x,y, ...){
	mod.p <- lowess(x,y, f=0.7, iter = 0)
	px <- sort(unique(mod.p$x))
	py <- as.numeric(tapply(mod.p$y, mod.p$x, mean))
	list(x = px, y = py)
}
get.lowess7mu <- function(x,y,...){
	mod.mu <- lowess(x,y, f=0.7, iter = 3)
	mux <- sort(unique(mod.mu$x))
	muy <- as.numeric(tapply(mod.mu$y, mod.mu$x, mean))
	resid <- y[order(x)] - mod.mu$y
	list(x = mux, y = muy, resid = resid)
}


##### White males

p.wm <- get.lowess7p(my.cac$age[wm], my.cac$zerocac[wm])
summary(p.wm)
lcac.wm <- get.lowess7mu(my.cac$age[wm1], my.cac$logcac[wm1])
summary(lcac.wm)

##### chinese male

p.cm <- get.lowess7p(my.cac$age[cm], my.cac$zerocac[cm])
lcac.cm <- get.lowess7mu(my.cac$age[cm1], my.cac$logcac[cm1])

##### black male

p.bm <- get.lowess7p(my.cac$age[bm], my.cac$zerocac[bm])
lcac.bm <- get.lowess7mu(my.cac$age[bm1], my.cac$logcac[bm1])

##### hispanic male

p.hm <- get.lowess7p(my.cac$age[hm], my.cac$zerocac[hm])
lcac.hm <- get.lowess7mu(my.cac$age[hm1], my.cac$logcac[hm1])

##### White females

p.wf <- get.lowess7p(my.cac$age[wf], my.cac$zerocac[wf])
lcac.wf <- get.lowess7mu(my.cac$age[wf1], my.cac$logcac[wf1])

##### chinese female

p.cf <- get.lowess7p(my.cac$age[cf], my.cac$zerocac[cf])
lcac.cf <- get.lowess7mu(my.cac$age[cf1], my.cac$logcac[cf1])

##### black female

p.bf <- get.lowess7p(my.cac$age[bf], my.cac$zerocac[bf])
lcac.bf <- get.lowess7mu(my.cac$age[bf1], my.cac$logcac[bf1])

##### hispanic female

p.hf <- get.lowess7p(my.cac$age[hf], my.cac$zerocac[hf])
lcac.hf <- get.lowess7mu(my.cac$age[hf1], my.cac$logcac[hf1])

##### Residuals

lcac.resid <- c(lcac.wm$resid, lcac.cm$resid, lcac.bm$resid, lcac.hm$resid, lcac.wf$resid, lcac.cf$resid, lcac.bf$resid, lcac.hf$resid)
length(lcac.resid)
table(my.cac$zerocac)


emp.p <- matrix(ncol=8, nrow=40)
colnames(emp.p) <- c("WM", "CM", "BM", "HM", "WF", "CF", "BF", "HF")

emp.p[p.wm$x[p.wm$x > 44] - 44,1] <- p.wm$y[p.wm$x > 44]
emp.p[p.cm$x[p.cm$x > 44] - 44,2] <- p.cm$y[p.cm$x > 44]
emp.p[p.bm$x[p.bm$x > 44] - 44,3] <- p.bm$y[p.bm$x > 44]
emp.p[p.hm$x[p.hm$x > 44] - 44,4] <- p.hm$y[p.hm$x > 44]
emp.p[p.wf$x[p.wf$x > 44] - 44,5] <- p.wf$y[p.wf$x > 44]
emp.p[p.cf$x[p.cf$x > 44] - 44,6] <- p.cf$y[p.cf$x > 44]
emp.p[p.bf$x[p.bf$x > 44] - 44,7] <- p.bf$y[p.bf$x > 44]
emp.p[p.hf$x[p.hf$x > 44] - 44,8] <- p.hf$y[p.hf$x > 44]

round(emp.p,2)
summary(emp.p)
emp.p[emp.p<0]
emp.p[!is.na(emp.p) & emp.p<0]

empi.p <- emp.p
empi.p[!is.na(empi.p) & empi.p<0] <- 0
empi.p[39,4] <- 0
summary(empi.p)

write.table(emp.p, file = "c:\\hyoju\\mc075\\data\\empp.csv", sep = ",")
write.table(empi.p, file = "c:\\hyoju\\mc075\\data\\empip.csv", sep = ",")

est.mu <- matrix(ncol=8, nrow=40)
colnames(est.mu) <- c("WM", "CM", "BM", "HM", "WF", "CF", "BF", "HF")

est.mu[lcac.wm$x[lcac.wm$x > 44] - 44,1] <- lcac.wm$y[lcac.wm$x > 44]
est.mu[lcac.cm$x[lcac.cm$x > 44] - 44,2] <- lcac.cm$y[lcac.cm$x > 44]
est.mu[lcac.bm$x[lcac.bm$x > 44] - 44,3] <- lcac.bm$y[lcac.bm$x > 44]
est.mu[lcac.hm$x[lcac.hm$x > 44] - 44,4] <- lcac.hm$y[lcac.hm$x > 44]
est.mu[lcac.wf$x[lcac.wf$x > 44] - 44,5] <- lcac.wf$y[lcac.wf$x > 44]
est.mu[lcac.cf$x[lcac.cf$x > 44] - 44,6] <- lcac.cf$y[lcac.cf$x > 44]
est.mu[lcac.bf$x[lcac.bf$x > 44] - 44,7] <- lcac.bf$y[lcac.bf$x > 44]
est.mu[lcac.hf$x[lcac.hf$x > 44] - 44,8] <- lcac.hf$y[lcac.hf$x > 44]
round(est.mu,2)

summary(est.mu)
esti.mu <- est.mu 
esti.mu[40,2] <- esti.mu[39,2] + (esti.mu[39,2] - esti.mu[38,2])
esti.mu[39,4] <- mean(esti.mu[38,4], esti.mu[40,4])
esti.mu[4,6] <- esti.mu[3,6]/3*2 + esti.mu[6,6]/3
esti.mu[5,6] <- esti.mu[3,6]/3 + esti.mu[6,6]/3*2
esti.mu[4,8] <- esti.mu[3,8]/2 + esti.mu[5,8]/2
summary(esti.mu)
round(esti.mu,2)

write.table(est.mu, file = "c:\\hyoju\\mc075\\data\\estmu.csv", sep = ",")
write.table(esti.mu, file = "c:\\hyoju\\mc075\\data\\estimu.csv", sep = ",")

save(empi.p, esti.mu, lcac.resid, file = "C:\\hyoju\\mc075\\data\\reslowess.Rdata")
rm(list=ls())
load("C:/hyoju/mc075/data/reslowess.Rdata")

CAC.quantiles <- function(age, female, race, cacscore = NULL, print = FALSE){
	if(race == 1 | race == 2 | race == 3 | race == 4){
		if(female == 0 | female == 1){
			p.hat <- empi.p[age-44, 4*female + race]
			if(print){
				cat("\n You are a", age,"-year-old ", c("white", "chinese", "black", "hispanic")[race], c("male", "female")[female + 1], ". \n")
				cat("\n In your age/gender/race group", 100*round(p.hat, 3), "% people have the zero CAC score. \n")
			}			
			if(!is.null(cacscore) && cacscore > 0){
				this.res <- as.vector(log(cacscore) - esti.mu[age-44, 4*female + race])
				this.q <- mean(lcac.resid < this.res)*(1-p.hat) + p.hat
				cat("\n Your CAC score of", cacscore, "corresponds to a", round(this.q*100, 0), "% quantile. \n")
			}

			at.q <- seq(0.01, 0.99, by = 0.01)
			tempzero.q <- sum(at.q < p.hat)
			CAC.q <- rep(0, length(at.q))
			if(tempzero.q < length(at.q))
				CAC.q[(tempzero.q + 1):length(at.q)] <- exp(quantile(lcac.resid, prob=(at.q[(tempzero.q + 1):length(at.q)] - p.hat)/(1 - p.hat)) + esti.mu[age-44, 4*female + race])
			if(print){
				cat("\n More quantiles \t CAC score \n")
					cat(100*at.q[25], "% \t \t \t", round(CAC.q[25],0), "\n")
					cat(100*at.q[50], "% \t \t \t", round(CAC.q[50],0), "\n")
					cat(100*at.q[75], "% \t \t \t", round(CAC.q[75],0), "\n")
					cat(100*at.q[90], "% \t \t \t", round(CAC.q[90],0), "\n")
					cat(100*at.q[95], "% \t \t \t", round(CAC.q[95],0), "\n")
				cat("\n")
			}
			return(list(p.hat, CAC.q))
		}
	}
}


CAC.quantiles(45, 0, 1)
CAC.quantiles(45, 0, 1, print=T)
CAC.quantiles(45, female = 0, race = 1, , cacscore = 100, print=T)

quantiles.wm <- matrix(ncol=40, nrow=99)
quantiles.cm <- matrix(ncol=40, nrow=99)
quantiles.bm <- matrix(ncol=40, nrow=99)
quantiles.hm <- matrix(ncol=40, nrow=99)
quantiles.wf <- matrix(ncol=40, nrow=99)
quantiles.cf <- matrix(ncol=40, nrow=99)
quantiles.bf <- matrix(ncol=40, nrow=99)
quantiles.hf <- matrix(ncol=40, nrow=99)

rownames(quantiles.wm) <- c(1:99)
rownames(quantiles.cm) <- c(1:99)
rownames(quantiles.bm) <- c(1:99)
rownames(quantiles.hm) <- c(1:99)
rownames(quantiles.wf) <- c(1:99)
rownames(quantiles.cf) <- c(1:99)
rownames(quantiles.bf) <- c(1:99)
rownames(quantiles.hf) <- c(1:99)

colnames(quantiles.wm) <- c(45:84)
colnames(quantiles.cm) <- c(45:84)
colnames(quantiles.bm) <- c(45:84)
colnames(quantiles.hm) <- c(45:84)
colnames(quantiles.wf) <- c(45:84)
colnames(quantiles.cf) <- c(45:84)
colnames(quantiles.bf) <- c(45:84)
colnames(quantiles.hf) <- c(45:84)

for(i in 45:84){
	quantiles.wm[, i-44] <- CAC.quantiles(i, female = 0, race = 1)[[2]]
	quantiles.cm[, i-44] <- CAC.quantiles(i, female = 0, race = 2)[[2]]
	quantiles.bm[, i-44] <- CAC.quantiles(i, female = 0, race = 3)[[2]]
	quantiles.hm[, i-44] <- CAC.quantiles(i, female = 0, race = 4)[[2]]
	quantiles.wf[, i-44] <- CAC.quantiles(i, female = 1, race = 1)[[2]]
	quantiles.cf[, i-44] <- CAC.quantiles(i, female = 1, race = 2)[[2]]
	quantiles.bf[, i-44] <- CAC.quantiles(i, female = 1, race = 3)[[2]]
	quantiles.hf[, i-44] <- CAC.quantiles(i, female = 1, race = 4)[[2]]
}


write.table(quantiles.wm, file = "c:\\hyoju\\mc075\\data\\Qwm.csv", sep = ",", col.names=F)
write.table(quantiles.cm, file = "c:\\hyoju\\mc075\\data\\Qcm.csv", sep = ",", col.names=F)
write.table(quantiles.bm, file = "c:\\hyoju\\mc075\\data\\Qbm.csv", sep = ",", col.names=F)
write.table(quantiles.hm, file = "c:\\hyoju\\mc075\\data\\Qhm.csv", sep = ",", col.names=F)
write.table(quantiles.wf, file = "c:\\hyoju\\mc075\\data\\Qwf.csv", sep = ",", col.names=F)
write.table(quantiles.cf, file = "c:\\hyoju\\mc075\\data\\Qcf.csv", sep = ",", col.names=F)
write.table(quantiles.bf, file = "c:\\hyoju\\mc075\\data\\Qbf.csv", sep = ",", col.names=F)
write.table(quantiles.hf, file = "c:\\hyoju\\mc075\\data\\Qhf.csv", sep = ",", col.names=F)

save(quantiles.wm, quantiles.cm, quantiles.bm, quantiles.hm, quantiles.wf, quantiles.cf, quantiles.bf, quantiles.hf, file = "C:\\hyoju\\mc075\\data\\estQ.Rdata")



rm(list=ls())
load("C:\\hyoju\\mc075\\data\\mycac.Rdata")
load("C:\\hyoju\\mc075\\data\\estQ.Rdata")
load("C:\\hyoju\\mc075\\data\\reslowess.Rdata")
ls()

wm <- my.cac$male == "male" & my.cac$race == "white"
cm <- my.cac$male == "male" & my.cac$race == "chinese"
bm <- my.cac$male == "male" & my.cac$race == "black"
hm <- my.cac$male == "male" & my.cac$race == "hispanic"
wf <- my.cac$male == "female" & my.cac$race == "white"
cf <- my.cac$male == "female" & my.cac$race == "chinese"
bf <- my.cac$male == "female" & my.cac$race == "black"
hf <- my.cac$male == "female" & my.cac$race == "hispanic"

my.plotP <- function(x,y, lowess.p, ...){
	plot(x,y, type="n", ylab="Probability of CAC > 0", xlab = "Age")
	this.y <- tapply(y,x,mean)
	this.wt <- tapply(y,x, length)
	this.wt <- this.wt/max(this.wt)*2 + 0.5
	points(sort(unique(x)), 1-this.y, cex=this.wt)
	lines(45:84, 1-lowess.p, lwd=2)
}

##### Figure 1. Women

par(las = 1, mfrow = c(2,2), mar=c(5,5,3,1), oma = c(1,1,2,1))
my.plotP(my.cac$age[wf], my.cac$zerocac[wf], empi.p[,5])
title("White (n = 1308)")
my.plotP(my.cac$age[cf], my.cac$zerocac[cf], empi.p[,6])
title("Chinese (n = 371)")
my.plotP(my.cac$age[bf], my.cac$zerocac[bf], empi.p[,7])
title("Black (n = 903)")
my.plotP(my.cac$age[hf], my.cac$zerocac[hf], empi.p[,8])
title("Hispanic (n = 669)")
title("Women", outer=T, cex.main = 1.5)

##### Figure 1. Men

par(las = 1, mfrow = c(2,2), mar=c(5,5,3,1), oma = c(1,1,2,1))
my.plotP(my.cac$age[wm], my.cac$zerocac[wm], empi.p[,1])
title("White (n = 1195)")
my.plotP(my.cac$age[cm], my.cac$zerocac[cm], empi.p[,2])
title("Chinese (n = 348)")
my.plotP(my.cac$age[bm], my.cac$zerocac[bm], empi.p[,3])
title("Black (n = 710)")
my.plotP(my.cac$age[hm], my.cac$zerocac[hm], empi.p[,4])
title("Hispanic (n = 606)")
title("Men", outer=T, cex.main = 1.5)

##### Figures 2 & 3.

ftn.emp5age <- function(x, p, age, afrom, nage, aint = 5, decimal=0){
	res <- matrix(nrow=length(p), ncol = nage)
	for(i in 1:nage)
		res[,i] <- quantile(x[age >= afrom + (i-1)*aint & age < afrom + i*aint], p = p)
	round(res,decimal)
}

ftn.emp5agewt <- function(age, afrom, nage, aint = 5){
	res <- vector()
	for(i in 1:nage)
		res[i] <- length(age[age >= afrom + (i-1)*aint & age < afrom + i*aint])
	res <- res/max(res)
	return(res)
}
ftn.emp5agewt(my.cac$age[wf], 45, 8)

my.plotQ2 <- function(x,y, index, quantiles, ymax, gray = 0.7, lwd = 2.5, ...){
	plot(y[index] ~ jitter(x[index]), type="n", ylab="CAC", xlab = "Age", ylim=c(0, ymax))
	points(seq(47, 82, by = 5), ftn.emp5age(y[index], p=c(0.5), x[index], 45, 8), pch=19, col=gray(gray), cex=1+ftn.emp5agewt(x[index], 45, 8))
	points(seq(47, 82, by = 5), ftn.emp5age(y[index], p=c(0.75), x[index], 45, 8), pch=19, col=gray(gray), cex=1+ftn.emp5agewt(x[index], 45, 8))
	points(seq(47, 82, by = 5), ftn.emp5age(y[index], p=c(0.90), x[index], 45, 8), pch=19, col=gray(gray), cex=1+ftn.emp5agewt(x[index], 45, 8))
#	points(seq(47, 82, by = 5), ftn.emp5age(y[index], p=c(0.5), x[index], 45, 8), pch=19, col=gray(gray), cex=1.5)
#	points(seq(47, 82, by = 5), ftn.emp5age(y[index], p=c(0.75), x[index], 45, 8), pch=19, col=gray(gray), cex=1.5)
#	points(seq(47, 82, by = 5), ftn.emp5age(y[index], p=c(0.90), x[index], 45, 8), pch=19, col=gray(gray), cex=1.5)
	lines(45:84, quantiles[50,], lty=1, lwd=lwd)
	lines(45:84, quantiles[75,], lty=4, lwd=lwd)
	lines(45:84, quantiles[90,], lty=5, lwd=lwd)
	legend(45, ymax , lty=c(5, 4, 1), cex = 0.8, legend = c("90%", "75%", "50%"), bty="n")
}

max(quantiles.wf[90,])
par(las = 1, mfrow = c(2,2), mar=c(5,5,3,1), oma = c(0.25,0.25,0.25,0.25))
my.plotQ2(my.cac$age, my.cac$cac, index = wf, quantiles.wf, ymax=1200)
title("White (n = 1308)")
my.plotQ2(my.cac$age, my.cac$cac, index = cf, quantiles.cf, ymax=1200)
title("Chinese (n = 371)")
my.plotQ2(my.cac$age, my.cac$cac, index = bf, quantiles.bf, ymax=1200)
title("Black (n = 903)")
my.plotQ2(my.cac$age, my.cac$cac, index = hf, quantiles.hf, ymax=1200)
title("Hispanic (n = 669)")

max(quantiles.wm[90,])
par(las = 1, mfrow = c(2,2), mar=c(5,5,3,1), oma = c(0.25,0.25,0.25,0.25))
my.plotQ2(my.cac$age, my.cac$cac, index = wm, quantiles.wm, ymax=4000)
title("White (n = 1195)")
my.plotQ2(my.cac$age, my.cac$cac, index = cm, quantiles.cm, ymax=4000)
title("Chinese (n = 348)")
my.plotQ2(my.cac$age, my.cac$cac, index = bm, quantiles.bm, ymax=4000)
title("Black (n = 710)")
my.plotQ2(my.cac$age, my.cac$cac, index = hm, quantiles.hm, ymax=4000)
title("Hispanic (n = 606)")


##### Table 2.

agegp4 <- vector()
agegp4[my.cac$age < 55] <- 1
agegp4[my.cac$age >= 55 & my.cac$age < 65] <- 2
agegp4[my.cac$age >= 65 & my.cac$age < 75] <- 3
agegp4[my.cac$age >= 75 & my.cac$age < 85] <- 4
table(agegp4)

table(my.cac$race, agegp4, my.cac$male)
round(quantiles.wf[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)
round(quantiles.cf[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)
round(quantiles.bf[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)
round(quantiles.hf[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)

round(quantiles.wm[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)
round(quantiles.cm[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)
round(quantiles.bm[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)
round(quantiles.hm[c(25, 50, 75, 90, 95), c(6, 16, 26, 36)], 0)


##### Figure 4.

par(las = 1, mar=c(5,5,3,1))

fmax90 <- max(quantiles.wf[90,],quantiles.cf[90,], quantiles.bf[90,], quantiles.hf[90,])
plot(cac ~ jitter(age), type="n", ylim = c(0, fmax90), xlab = "Age", ylab = "CAC", data = my.cac, subset=my.cac$male == "female")
lines(45:84, quantiles.wf[90,], lty = 1, lwd = 2.5)
lines(45:84, quantiles.bf[90,], lty = 2, lwd = 2.5)
lines(45:84, quantiles.hf[90,], lty = 3, lwd = 2.5*1.2)
lines(45:84, quantiles.cf[90,], lty = 4, lwd = 2.5)
legend(45, fmax90 , lty=1:4, lwd=2.5, legend = c("White", "Black", "Hispanic", "Chinese"), bty="n")
title("90th percentile of CAC, Women")

mmax90 <- max(quantiles.wm[90,],quantiles.cm[90,], quantiles.bm[90,], quantiles.hm[90,])
plot(cac ~ jitter(age), type="n", ylim = c(0, 4000), xlab = "Age", ylab = "CAC", data = my.cac, subset=my.cac$male == "male")
lines(45:84, quantiles.wm[90,], lty = 1, lwd = 2)
lines(45:84, quantiles.bm[90,], lty = 2, lwd = 2)
lines(45:84, quantiles.hm[90,], lty = 3, lwd = 2.5)
lines(45:84, quantiles.cm[90,], lty = 4, lwd = 2)
legend(45, mmax90 , lty=1:4, lwd=2.5, legend = c("White", "Black", "Hispanic", "Chinese"), bty="n")
title("90th percentile of CAC, Men")


#########################################################
##### Figure 5
#########################################################

rm(list=ls())
cac.dat <- read.table("C:/hyoju/MC075/data/cacSPLUS.dat", sep="\t", header = T)
dim(cac.dat)
names(cac.dat)

cac <- cac.dat$agatpm1c
ucac <- cac.dat$agatum1c
vol <- cac.dat$volsum1c
female <- cac.dat$gender1 == 0
male <- cac.dat$gender1 == 1

par(las=1, mar=c(5,5,1,1))
summary(lm(cac[ucac > 0]~ucac[ucac > 0]))
plot(cac ~ ucac, col=gray(0.5), ylab = "Phantom-adjusted CAC Score", xlab ="Unadjusted CAC Score", xlim=range(ucac), ylim = range(ucac))
abline(lm(cac[ucac > 0]~ucac[ucac > 0]), lwd = 2)
text(4000, 5500, "y = -2.81 + 0.98x")
summary(lm(cac[vol> 0]~vol[vol> 0]))
plot(cac ~ vol, col=gray(0.5), ylab = "Phantom-adjusted CAC Score", xlab ="Total Volume Score", xlim=range(ucac), ylim=range(ucac))
abline(lm(cac[vol> 0]~vol[vol> 0]), lwd = 2)
text(5000, 4000, "y= -1.83 + 1.27x")


#########################################################
##### Figure 6
#########################################################


rm(list=ls())
load("C:\\hyoju\\mc075\\data\\estQ.Rdata")
ls()
round(quantiles.wf[50, c(6, 16, 26, 36)], 0)
round(quantiles.wm[50, c(6, 16, 26, 36)], 0)
round(quantiles.wf[90, c(6, 16, 26, 36)], 0)
round(quantiles.wm[90, c(6, 16, 26, 36)], 0)

## 50th percentile
#MESA
x1f <- c(50,60,70,80)
x1m <- c(50,60,70,80)
y1f <- quantiles.wf[50, c(6, 16, 26, 36)]
y1m <- quantiles.wm[50, c(6, 16, 26, 36)]

#Schmermund-2005
x6f<-c(47.5,52.5,57.5,62.5,67.5,72.5)
x6m<-c(47.5,52.5,57.5,62.5,67.5,72.5)
y6f<-c(0,0,0,3.5,12.5,43.2)
y6m<-c(4.8,12.4,51.6,83.3,125.3,200.6)

par(las = 1, mar=c(5,5,3,1))
plot(c(x1f,x6f,x1m,x6m),c(y1f,y6f,y1m,y6m),xlab="Age (yrs)",ylab="Agatston CAC Score")
lines(x1f,y1f,lty=1)
lines(x6f,y6f,lty=2)
lines(x1m,y1m,lty=1)
lines(x6m,y6m,lty=2)
legend(50,350,c("MESA","HNR"),lty=1:2)
title("Estimated 50th Percentile of CAC")
text(71,250,"men")
text(75,100,"women")

## 90th percentile
#MESA
x1f <- c(50,60,70,80)
x1m <- c(50,60,70,80)
y1f <- quantiles.wf[90, c(6, 16, 26, 36)]
y1m <- quantiles.wm[90, c(6, 16, 26, 36)]

#Schmermund--2005
x6f<-c(47.5,52.5,57.5,62.5,67.5,72.5)
x6m<-c(47.5,52.5,57.5,62.5,67.5,72.5)
y6f<-c(32.7,38.2,105.2,174.0,305.3,716.6)
y6m<-c(237,266.6,440.9,853.5,1195.4,1771.8)

plot(c(x1f,x6f,x1m,x6m),c(y1f,y6f,y1m,y6m),xlab="Age (yrs)",ylab="Agatston CAC Score")
lines(x1f,y1f,lty=1)
lines(x6f,y6f,lty=2)
lines(x1m,y1m,lty=1)
lines(x6m,y6m,lty=2)
legend(50,2500,c("MESA","HNR"),lty=1:2)
title("Estimated 90th Percentile of CAC")
text(72,2100,"men")
text(75,1000,"women")



#########################################################
######## Superhealthy
#########################################################

rm(list=ls())
library(foreign)
cac.dat <- read.spss("c:/hyoju/MC075/data/superhealthycac.sav", use.value.labels=F)
cac <- cac.dat$AGATPM1C
age <- cac.dat$AGE1C
male <- factor(cac.dat$GENDER1 , levels = c(0,1), label = c("female", "male"))
race <- factor(cac.dat$RACE, levels = c(1,2,3,4), label = c("white", "chinese", "black", "hispanic"))
table(male, race)
table(zerocac)
age45 <- age - 45
zerocac <- ifelse(cac == 0, 1, 0)
logcac <- ifelse(cac > 0, log(cac), NA)
my.superhealthycac <- data.frame(cac = cac, age = age, male = male, race = race, age45 = age45, zerocac = zerocac, logcac = logcac)
summary(my.superhealthycac)
dim(my.superhealthycac)
save(my.superhealthycac, file = "C:\\hyoju\\mc075\\data\\superhealthycac.Rdata")

rm(list=ls())
load(file = "C:\\hyoju\\mc075\\data\\superhealthycac.Rdata")
dim(my.superhealthycac)
summary(my.superhealthycac)
cac <- my.superhealthycac$cac
age <- my.superhealthycac$age
male <- my.superhealthycac$male
race <- my.superhealthycac$race
age45 <- my.superhealthycac$age45
zerocac <- my.superhealthycac$zerocac
logcac <- my.superhealthycac$logcac
ls()

racegender <- 4*(male == "male") + c(race)
table(racegender)

tapply(age, racegender, length)
tapply(age[cac>0], racegender[cac>0], length)
tapply(age, racegender, range)
tapply(age[cac>0], racegender[cac>0], range)

summary(lm(zerocac ~ age + race + age*race,  subset = male == "male"))
summary(lm(logcac ~ age + race + age*race, subset = male == "male"))
summary(lm(zerocac ~ age + race + age*race,  subset = male == "female"))
summary(lm(logcac ~ age + race + age*race, subset = male == "female"))
summary(lm(zerocac ~ age + race, subset = male == "male"))
summary(lm(zerocac ~ race, subset = male == "male"))
summary(lm(zerocac ~ age + race, subset = male == "female"))
summary(lm(zerocac ~ race, subset = male == "female"))
summary(lm(logcac ~ age + race, subset = male == "male"))
summary(lm(logcac ~ race, subset = male == "male"))
summary(lm(logcac ~ age + race, subset = male == "female"))
summary(lm(logcac ~ race, subset = male == "female"))

shiftp.male <- c(0, lm(zerocac ~ race, subset = male == "male")$coef[2:4])
shiftp.female <- c(0, lm(zerocac ~ race, subset = male == "female")$coef[2:4])
shiftmu.male <- c(0, lm(logcac ~ race, subset = male == "male")$coef[2:4])
shiftmu.female <- c(0, lm(logcac ~ race, subset = male == "female")$coef[2:4])
shiftp.male
shiftp.female
shiftmu.male
shiftmu.female

shiftp <- rep(0, length(male))
shiftp[male == "male" & race == "chinese"] <- shiftp.male[2]
shiftp[male == "male" & race == "black"] <- shiftp.male[3]
shiftp[male == "male" & race == "hispanic"] <- shiftp.male[4]
shiftp[male == "female" & race == "chinese"] <- shiftp.female[2]
shiftp[male == "female" & race == "black"] <- shiftp.female[3]
shiftp[male == "female" & race == "hispanic"] <- shiftp.female[4]
shiftmu <- rep(0, length(male))
shiftmu[male == "male" & race == "chinese"] <- shiftmu.male[2]
shiftmu[male == "male" & race == "black"] <- shiftmu.male[3]
shiftmu[male == "male" & race == "hispanic"] <- shiftmu.male[4]
shiftmu[male == "female" & race == "chinese"] <- shiftmu.female[2]
shiftmu[male == "female" & race == "black"] <- shiftmu.female[3]
shiftmu[male == "female" & race == "hispanic"] <- shiftmu.female[4]

table(shiftp, race, male)
table(shiftmu, race, male)

summary(shifted.zerocac)
summary(shifted.logcac)

shifted.zerocac <- zerocac - shiftp
shifted.logcac <- logcac - shiftmu
lm(shifted.zerocac ~ race, subset = male == "male")
lm(shifted.zerocac ~ race, subset = male == "female")
lm(shifted.logcac ~ race, subset = male == "male")
lm(shifted.logcac ~ race, subset = male == "female")


get.lowess7p <- function(x,y, ...){
	mod.p <- lowess(x,y, f=0.7, iter = 0)
	px <- sort(unique(mod.p$x))
	py <- as.numeric(tapply(mod.p$y, mod.p$x, mean))
	list(x = px, y = py)
}
get.lowess7mu <- function(x,y,...){
	mod.mu <- lowess(x,y, f=0.7, iter = 3)
	mux <- sort(unique(mod.mu$x))
	muy <- as.numeric(tapply(mod.mu$y, mod.mu$x, mean))
	resid <- y[order(x)] - mod.mu$y
	list(x = mux, y = muy, resid = resid)
}
emp.p <- matrix(ncol=8, nrow=40)
colnames(emp.p) <- c("WM", "CM", "BM", "HM", "WF", "CF", "BF", "HF")
rownames(emp.p) <- 45:84
a <- get.lowess7p(age[male=="male"], shifted.zerocac[male=="male"])
b <- get.lowess7p(age[male=="female"], shifted.zerocac[male=="female"])
a
b
emp.p[a$x[a$x > 44] - 44,1] <- a$y[a$x > 44]
emp.p[a$x[a$x > 44] - 44,2] <- a$y[a$x > 44] + shiftp.male[2]
emp.p[a$x[a$x > 44] - 44,3] <- a$y[a$x > 44] + shiftp.male[3]
emp.p[a$x[a$x > 44] - 44,4] <- a$y[a$x > 44] + shiftp.male[4]
emp.p[b$x[b$x > 44] - 44,5] <- b$y[b$x > 44]
emp.p[b$x[b$x > 44] - 44,6] <- b$y[b$x > 44] + shiftp.female[2]
emp.p[b$x[b$x > 44] - 44,7] <- b$y[b$x > 44] + shiftp.female[3]
emp.p[b$x[b$x > 44] - 44,8] <- b$y[b$x > 44] + shiftp.female[4]
round(emp.p,2)
emp.p[emp.p > 0.99] <- 0.99
round(emp.p,2)
rm(a, b)

a <- get.lowess7mu(age[male=="male" & zerocac == 0], shifted.logcac[male=="male" & zerocac == 0])
b <- get.lowess7mu(age[male=="female" & zerocac == 0], shifted.logcac[male=="female" & zerocac == 0])
est.mu <- matrix(ncol=8, nrow=40)
colnames(est.mu) <- c("WM", "CM", "BM", "HM", "WF", "CF", "BF", "HF")
rownames(est.mu) <- 45:84
est.mu[a$x[a$x > 44] - 44,1] <- a$y[a$x > 44]
est.mu[a$x[a$x > 44] - 44,2] <- a$y[a$x > 44] + shiftmu.male[2]
est.mu[a$x[a$x > 44] - 44,3] <- a$y[a$x > 44] + shiftmu.male[3]
est.mu[a$x[a$x > 44] - 44,4] <- a$y[a$x > 44] + shiftmu.male[4]
est.mu[b$x[b$x > 44] - 44,5] <- b$y[b$x > 44]
est.mu[b$x[b$x > 44] - 44,6] <- b$y[b$x > 44] + shiftmu.female[2]
est.mu[b$x[b$x > 44] - 44,7] <- b$y[b$x > 44] + shiftmu.female[3]
est.mu[b$x[b$x > 44] - 44,8] <- b$y[b$x > 44] + shiftmu.female[4]
round(est.mu,2)
lcac.resid <- c(a$resid, b$resid)
rm(a, b)
length(lcac.resid)
CAC.quantiles <- function(age, female, race, cacscore = NULL, print = FALSE){
	if(race == 1 | race == 2 | race == 3 | race == 4){
		if(female == 0 | female == 1){
			p.hat <- emp.p[age-44, 4*female + race]
			if(print){
				cat("\n You are a", age,"-year-old ", c("white", "chinese", "black", "hispanic")[race], c("male", "female")[female + 1], ". \n")
				cat("\n In your age/gender/race group", 100*round(p.hat, 3), "% people have the zero CAC score. \n")
			}			
			if(!is.null(cacscore) && cacscore > 0){
				this.res <- as.vector(log(cacscore) - est.mu[age-44, 4*female + race])
				this.q <- mean(lcac.resid < this.res)*(1-p.hat) + p.hat
				cat("\n Your CAC score of", cacscore, "corresponds to a", round(this.q*100, 0), "% quantile. \n")
			}

			at.q <- seq(0.01, 0.99, by = 0.01)
			tempzero.q <- sum(at.q < p.hat)
			CAC.q <- rep(0, length(at.q))
			if(tempzero.q < length(at.q))
				CAC.q[(tempzero.q + 1):length(at.q)] <- exp(quantile(lcac.resid, prob=(at.q[(tempzero.q + 1):length(at.q)] - p.hat)/(1 - p.hat)) + est.mu[age-44, 4*female + race])
			if(print){
				cat("\n More quantiles \t CAC score \n")
					cat(100*at.q[25], "% \t \t \t", round(CAC.q[25],0), "\n")
					cat(100*at.q[50], "% \t \t \t", round(CAC.q[50],0), "\n")
					cat(100*at.q[75], "% \t \t \t", round(CAC.q[75],0), "\n")
					cat(100*at.q[90], "% \t \t \t", round(CAC.q[90],0), "\n")
					cat(100*at.q[95], "% \t \t \t", round(CAC.q[95],0), "\n")
				cat("\n")
			}
			return(list(p.hat, CAC.q))
		}
	}
}
healthy.quantiles.wm <- matrix(ncol=30, nrow=99)
healthy.quantiles.cm <- matrix(ncol=30, nrow=99)
healthy.quantiles.bm <- matrix(ncol=30, nrow=99)
healthy.quantiles.hm <- matrix(ncol=30, nrow=99)
rownames(healthy.quantiles.wm) <- c(1:99)
rownames(healthy.quantiles.cm) <- c(1:99)
rownames(healthy.quantiles.bm) <- c(1:99)
rownames(healthy.quantiles.hm) <- c(1:99)
colnames(healthy.quantiles.wm) <- 45:74
colnames(healthy.quantiles.cm) <- 45:74
colnames(healthy.quantiles.bm) <- 45:74
colnames(healthy.quantiles.hm) <- 45:74

for(i in 45:74){
	healthy.quantiles.wm[, i-44] <- CAC.quantiles(i, female = 0, race = 1)[[2]]
	healthy.quantiles.cm[, i-44] <- CAC.quantiles(i, female = 0, race = 2)[[2]]
	healthy.quantiles.bm[, i-44] <- CAC.quantiles(i, female = 0, race = 3)[[2]]
	healthy.quantiles.hm[, i-44] <- CAC.quantiles(i, female = 0, race = 4)[[2]]
}

healthy.quantiles.wf <- matrix(ncol=35, nrow=99)
healthy.quantiles.cf <- matrix(ncol=35, nrow=99)
healthy.quantiles.bf <- matrix(ncol=35, nrow=99)
healthy.quantiles.hf <- matrix(ncol=35, nrow=99)
rownames(healthy.quantiles.wf) <- c(1:99)
rownames(healthy.quantiles.cf) <- c(1:99)
rownames(healthy.quantiles.bf) <- c(1:99)
rownames(healthy.quantiles.hf) <- c(1:99)
colnames(healthy.quantiles.wf) <- 45:79
colnames(healthy.quantiles.cf) <- 45:79
colnames(healthy.quantiles.bf) <- 45:79
colnames(healthy.quantiles.hf) <- 45:79
for(i in 45:79){
	healthy.quantiles.wf[, i-44] <- CAC.quantiles(i, female = 1, race = 1)[[2]]
	healthy.quantiles.cf[, i-44] <- CAC.quantiles(i, female = 1, race = 2)[[2]]
	healthy.quantiles.bf[, i-44] <- CAC.quantiles(i, female = 1, race = 3)[[2]]
	healthy.quantiles.hf[, i-44] <- CAC.quantiles(i, female = 1, race = 4)[[2]]
}

save(healthy.quantiles.wm, healthy.quantiles.cm, healthy.quantiles.bm, healthy.quantiles.hm, healthy.quantiles.wf, healthy.quantiles.cf, healthy.quantiles.bf, healthy.quantiles.hf, file = "C:\\hyoju\\mc075\\data\\superhealthy.estQ.Rdata")


###### Table 3. superhealthy

rm(list=ls())
load(file = "C:\\hyoju\\mc075\\data\\superhealthycac.Rdata")
load(file = "C:\\hyoju\\mc075\\data\\superhealthy.estQ.Rdata")
dim(my.superhealthycac)
table(my.superhealthycac$zerocac)
table(my.superhealthycac$male)

agegp4 <- vector()
agegp4[my.superhealthycac$age < 55] <- 1
agegp4[my.superhealthycac$age >= 55 & my.superhealthycac$age < 65] <- 2
agegp4[my.superhealthycac$age >= 65 & my.superhealthycac$age < 75] <- 3
agegp4[my.superhealthycac$age >= 75 & my.superhealthycac$age < 85] <- 4
table(agegp4)

table(my.superhealthycac$race, agegp4, my.superhealthycac$male)

round(healthy.quantiles.wf[c(25, 50, 75, 90), c(6, 16, 26)], 0)
round(healthy.quantiles.cf[c(25, 50, 75, 90), c(6, 16, 26)], 0)
round(healthy.quantiles.bf[c(25, 50, 75, 90), c(6, 16, 26)], 0)
round(healthy.quantiles.hf[c(25, 50, 75, 90), c(6, 16, 26)], 0)
round(healthy.quantiles.wm[c(25, 50, 75, 90), c(6, 16)], 0)
round(healthy.quantiles.cm[c(25, 50, 75, 90), c(6, 16)], 0)
round(healthy.quantiles.bm[c(25, 50, 75, 90), c(6, 16)], 0)
round(healthy.quantiles.hm[c(25, 50, 75, 90), c(6, 16)], 0)












