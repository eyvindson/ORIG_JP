

################## NOTE HERE USING CONSTANT CLIMATE
# Using scand. parameters for reporting
### THE LAST YEAR OF CALCULATION 2012

yr <- 2019
yr.VANHA <- 2018

years <- c("X1990","X1991","X1992","X1993","X1994","X1995","X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018",'X2019')

path <- '/hsan2/khk/ghg/'

#Input paths:
ypath <- paste(path,yr,'/soil/functions/yasso07/',sep="")
funpath <- paste(path,yr,'/soil/functions/',sep="")
nfidata <- paste(path,yr,'/soil/nfidata/',sep="")
draindata <- paste(path,yr,'/trees/drain/remaining/litter/lulucf/',sep="")
climpath <- paste(path,yr,'/soil/weather/',sep="")

undpath <- paste(path,yr,'/soil/understorey/litter/',sep="")

areas <- paste(path,yr,'/areas/',sep="")

##Output paths:

figpath <- paste(path,yr,'/soil/mineral/remaining/figs/', sep="")
resultspath <- paste(path,yr,'/soil/mineral/remaining/results/', sep="")
crf <- paste(path,yr,'/crf/', sep="")
NIR <- paste(path,yr,'/NIR/',sep="")


source(paste(ypath,"yasso07.R",sep=""))
dyn.load(paste(ypath,"yasso07.so",sep=""))

### Scandinavian new, added 7.5.2012
a.ska <- c( -0.5172509,-3.551512,-0.3458914,-0.2660175,0.044852223,0.0029265443,0.9779027,0.6373951,0.3124745,0.018712098,	0.022490378,0.011738963,0.00099046889,0.3361765,0.041966144,0.089885026,0.089501545,-0.0022709155,0.17,-0.0015,0.17,-0.0015,	0.17,-0.0015,0,-2.935411,0,101.8253, 260,-0.080983594,-0.315179,-0.5173524,0, 0,-0.00024180325,0.0015341907, 101.8253,260,-0.5391662,1.18574,-0.2632936,0,0,0)


####
# 1. CLIMATE DATA
####
climate =  read.table(paste(climpath,"GHG_climate_1960_2019.csv",sep=""), header = TRUE, sep=",") 


###
# 2. Spin-up climate, averages for 1961-1990
##
init.climate=climate$Year>=1960 & climate$Year<=1990
spin.pre.sf <- mean(climate$Rainfall_south[init.climate])
spin.pre.nf <- mean(climate$Rainfall_north[init.climate])
spin.tem.sf <-  mean(climate$meanT_south[init.climate])
spin.tem.nf <- mean(climate$meanT_north[init.climate])
spin.amp.sf <- mean(climate$Ampli_south[init.climate])
spin.amp.nf <- mean(climate$Ampli_north[init.climate])

clim.sf <- c(spin.tem.sf,spin.pre.sf,spin.amp.sf)
clim.nf <- c(spin.tem.nf,spin.pre.nf,spin.amp.nf)

## here climate for 1971->
vvvv=1990:yr
spin7012.pre.sf =matrix(,length(vvvv),1)
spin7012.pre.nf =matrix(,length(vvvv),1)
spin7012.tem.sf=matrix(,length(vvvv),1)
spin7012.tem.nf=matrix(,length(vvvv),1)
spin7012.amp.sf =matrix(,length(vvvv),1)
spin7012.amp.nf=matrix(,length(vvvv),1)

for (i in 1:length(vvvv)) {
  spin7012.tem.nf[i] = mean(climate$meanT_north[climate$Year>=vvvv[i]-29 & climate$Year<=vvvv[i]])
  spin7012.amp.nf[i] = mean(climate$Ampli_north[climate$Year>=vvvv[i]-29 & climate$Year<=vvvv[i]])
  spin7012.pre.nf[i] = mean(climate$Rainfall_north[climate$Year>=vvvv[i]-29 & climate$Year<=vvvv[i]])
  
  spin7012.tem.sf[i] = mean(climate$meanT_south[climate$Year>=vvvv[i]-29 & climate$Year<=vvvv[i]])
  spin7012.amp.sf [i] = mean(climate$Ampli_south[climate$Year>=vvvv[i]-29 & climate$Year<=vvvv[i]])
  spin7012.pre.sf[i] = mean(climate$Rainfall_south[climate$Year>=vvvv[i]-29 & climate$Year<=vvvv[i]])
}


# model.climate=climate$Year>=1971 & climate$Year<=yr
# spin7012.pre.sf <- mean(climate$Rainfall_south[model.climate])
# spin7012.pre.nf <- mean(climate$Rainfall_north[model.climate])
# spin7012.tem.sf <-  mean(climate$meanT_south[model.climate])
# spin7012.tem.nf <- mean(climate$meanT_north[model.climate])
# spin7012.amp.sf <- mean(climate$Ampli_south[model.climate])
# spin7012.amp.nf <- mean(climate$Ampli_north[model.climate])


clim7012.sf <- t(matrix(c(spin7012.tem.sf,spin7012.pre.sf,spin7012.amp.sf),nrow=3,byrow=TRUE))
clim7012.nf <- t(matrix(c(spin7012.tem.nf,spin7012.pre.nf,spin7012.amp.nf),nrow=3,byrow=TRUE))

c.sf=matrix(rep(clim7012.sf[1,],18),nrow=18,byrow=TRUE)
c.nf=matrix(rep(clim7012.nf[1,],15),nrow=15,byrow=TRUE)

clim7012.sf=rbind(c.sf,clim7012.sf)
clim7012.nf=rbind(c.nf,clim7012.nf)


#######################################
# 3. Spin-up litter and Spin-up runs  #
#######################################

# Southern Finland

nwl.sf.nfi6 <-  c(read.csv(paste(nfidata,"nfi6/litter/nwl.sf.nfi6.csv", sep=""), header=TRUE))
fwl.sf.nfi6 <-  c(read.csv(paste(nfidata,"nfi6/litter/fwl.sf.nfi6.csv", sep=""), header=TRUE))

und.sf.nfi6 <-  c(read.csv(paste(undpath,"und.nwl.SF.min.csv", sep=""), header=TRUE))

log.nwl.SF.min <-  read.csv(paste(draindata,"log.nwl.SF.min.csv", sep=""), header=TRUE)
log.fwl.SF.min <-  read.csv(paste(draindata,"log.fwl.SF.min.csv", sep=""), header=TRUE)
log.cwl.SF.min <-  read.csv(paste(draindata,"log.cwl.SF.min.csv", sep=""), header=TRUE)

nat.nwl.SF.min <-  read.csv(paste(draindata,"nat.nwl.SF.min.csv", sep=""), header=TRUE)
nat.fwl.SF.min <-  read.csv(paste(draindata,"nat.fwl.SF.min.csv", sep=""), header=TRUE)
nat.cwl.SF.min <-  read.csv(paste(draindata,"nat.cwl.SF.min.csv", sep=""), header=TRUE)

# for loggings and natural mortality SF spin-up is an average of 1970-1976

log.nwl.SF.min.spin <- colMeans(log.nwl.SF.min[1:7,])
log.fwl.SF.min.spin <- colMeans(log.fwl.SF.min[1:7,])
log.cwl.SF.min.spin <- colMeans(log.cwl.SF.min[1:7,])

nat.nwl.SF.min.spin <- colMeans(nat.nwl.SF.min[1:7,])
nat.fwl.SF.min.spin <- colMeans(nat.fwl.SF.min[1:7,])
nat.cwl.SF.min.spin <- colMeans(nat.cwl.SF.min[1:7,])

###    

# summing up similar carbon inputs

nwl <- nwl.sf.nfi6$x+und.sf.nfi6$x+log.nwl.SF.min.spin+nat.nwl.SF.min.spin 
fwl <- fwl.sf.nfi6$x+log.fwl.SF.min.spin+nat.fwl.SF.min.spin 
cwl <- log.cwl.SF.min.spin+nat.cwl.SF.min.spin

nwl.spin.sf=c(nwl,0)
fwl.spin.sf=c(fwl,0)
cwl.spin.sf=c(cwl,0)

##    sum(fwl, cwl, nwl)   gives  2.47169 tons C /ha (reasonable)

##  ACTUAL SPIN-UP SOUTH!!

t <- 10000 #time in years 

cl <- clim.sf #(T_mean[C],P_anual[mm], T_amplitude[C])
init <- c(0,0,0,0,0) #Mass per compartments (A, W, E, N, H)
inf <- c(nwl, 0)
s <- 0 #Diameter of logs in cm. 0.0 for nonwoody litter
z <- c(0,0,0,0,0)

spin.nwl.sf.s <- yasso07(a.ska, t, cl, init, inf, s, z)

# fwl
inf <- c(fwl, 0)
s <- 2 #Diameter of logs in cm. 2 for finewoody litter
spin.fwl.sf.s <- yasso07(a.ska, t, cl, init, inf, s, z)

inf <- c(cwl, 0)
s <- 15 #Diameter of logs in cm. 10 for woody litter
spin.cwl.sf.s <- yasso07(a.ska, t, cl, init, inf, s, z)

spin.sf <- rbind(spin.nwl.sf.s, spin.fwl.sf.s, spin.cwl.sf.s)
#Copy this to yass07.conversion.mineral-2011.r to init.sf for afforested CL (+100yrs of agriculture)
spin.sf.sum <- colSums(spin.sf)

write.csv(spin.sf.sum, (paste(resultspath,"spin.sf.sum.csv", sep="")))



# Northern Finland, note starting from NFI6

nwl.nf.nfi6 <-  c(read.csv(paste(nfidata,"nfi6/litter/nwl.nf.nfi6.csv", sep=""), header=TRUE))
fwl.nf.nfi6 <-  c(read.csv(paste(nfidata,"nfi6/litter/fwl.nf.nfi6.csv", sep=""), header=TRUE))

und.nf.nfi6 <-  c(read.csv(paste(undpath,"und.nwl.NF.min.csv", sep=""), header=TRUE))


log.nwl.NF.min <-  read.csv(paste(draindata,"log.nwl.NF.min.csv", sep=""), header=TRUE)
log.fwl.NF.min <-  read.csv(paste(draindata,"log.fwl.NF.min.csv", sep=""), header=TRUE)
log.cwl.NF.min <-  read.csv(paste(draindata,"log.cwl.NF.min.csv", sep=""), header=TRUE)

nat.nwl.NF.min <-  read.csv(paste(draindata,"nat.nwl.NF.min.csv", sep=""), header=TRUE)
nat.fwl.NF.min <-  read.csv(paste(draindata,"nat.fwl.NF.min.csv", sep=""), header=TRUE)
nat.cwl.NF.min <-  read.csv(paste(draindata,"nat.cwl.NF.min.csv", sep=""), header=TRUE)

# for loggings and natural mortality NF spin-up is an average of 1975-77 ?

log.nwl.NF.min.spin <- colMeans(log.nwl.NF.min[1:3,])
log.fwl.NF.min.spin <- colMeans(log.fwl.NF.min[1:3,])
log.cwl.NF.min.spin <- colMeans(log.cwl.NF.min[1:3,])

nat.nwl.NF.min.spin <- colMeans(nat.nwl.NF.min[1:3,])
nat.fwl.NF.min.spin <- colMeans(nat.fwl.NF.min[1:3,])
nat.cwl.NF.min.spin <- colMeans(nat.cwl.NF.min[1:3,])

###    
#  sum(nat.nwl.NF.min.spin, nat.fwl.NF.min.spin, nat.cwl.NF.min.spin)
#  sum(log.nwl.NF.min.spin, log.fwl.NF.min.spin, log.cwl.NF.min.spin)
###

###   
#  plot(seq(1, dim(log.nwl.NF.min)[1], by=1), log.nwl.NF.min[,1], type="l")  
###


# summing up similar carbon inputs

nwl <- nwl.nf.nfi6$x+und.nf.nfi6$x+log.nwl.NF.min.spin+nat.nwl.NF.min.spin 
fwl <- fwl.nf.nfi6$x+log.fwl.NF.min.spin+nat.fwl.NF.min.spin 
cwl <- log.cwl.NF.min.spin+nat.cwl.NF.min.spin

nwl.spin.nf=c(nwl,0)
fwl.spin.nf=c(fwl,0)
cwl.spin.nf=c(cwl,0)

##    sum(fwl, cwl, nwl)   gives   1.855517  tons C /ha (reasonable)

##  ACTUAL SPIN-UP NORTH!!

t <- 10000 #time in years
cl <- clim.nf #(T_mean[C],P_anual[mm], T_amplitude[C])
init <- c(0,0,0,0,0) #Mass per compartments (A, W, E, N, H)
inf <- c(nwl, 0)
s <- 0 #Diameter of logs in cm. 0.0 for nonwoody litter
z <- c(0,0,0,0,0)

spin.nwl.nf.s <- yasso07(a.ska, t, cl, init, inf, s, z)
                                        # fwl
inf <- c(fwl, 0)
s <- 2 #Diameter of logs in cm. 2 for finewoody litter

spin.fwl.nf.s <- yasso07(a.ska, t, cl, init, inf, s, z)

inf <- c(cwl, 0)
s <- 15 #Diameter of logs in cm. 10 for woody litter

spin.cwl.nf.s <- yasso07(a.ska, t, cl, init, inf, s, z)

spin.nf <- rbind(spin.nwl.nf.s, spin.fwl.nf.s, spin.cwl.nf.s)
#Copy this to yass07.conversion.mineral-2011.r to init.nf for afforested CL (+100yrs of agriculture)
spin.nf.sum <- colSums(spin.nf)

write.csv(spin.nf.sum, (paste(resultspath,"spin.nf.sum.csv", sep="")))


## printing C inputs for the LUC-calculation ######
FL.spin.litter=cbind(nwl.spin.nf,fwl.spin.nf,cwl.spin.nf,nwl.spin.sf,fwl.spin.sf,cwl.spin.sf)
rownames(FL.spin.litter)=cbind("A","W","E","N","H")
colnames(FL.spin.litter)=cbind("nwl.NF","fwl.NF","cwl.NF","nwl.SF","fwl.SF","cwl.SF")
write.table(FL.spin.litter, paste(path,yr,"/soil/results/","FL.soil.init.CInput.csv", sep=""), sep=" ", row.names = TRUE, col.names = TRUE, quote=FALSE)
########


##################################################
# 4. Simulation of time series  SOUTHERN FINLAND #
##################################################


### 1 Southern Finland!!!

# first tree litter for 1972->, living trees !!!

nwl.sf.nfi7 <-  c(read.csv(paste(nfidata,"nfi7/litter/nwl.sf.nfi7.csv", sep=""), header=TRUE))
fwl.sf.nfi7 <-  c(read.csv(paste(nfidata,"nfi7/litter/fwl.sf.nfi7.csv", sep=""), header=TRUE))

nwl.sf.nfi90 <-  (read.csv(paste(nfidata,"nfi10/litter/nwl.sf.1990",yr,".csv", sep=""), header=TRUE))
fwl.sf.nfi90 <-  (read.csv(paste(nfidata,"nfi10/litter/fwl.sf.1990",yr,".csv", sep=""), header=TRUE))




## interpolation of times series

naat <- matrix(NA, nrow = 10, ncol=4)
nwl.sf <- rbind(nwl.sf.nfi6$x, naat[1:6,],nwl.sf.nfi7$x, naat, nwl.sf.nfi90)
index <- seq(1,dim(nwl.sf)[1], by=1)
nwl.sf.a <- approx(index,nwl.sf[,1],index)$y 
nwl.sf.w <- approx(index,nwl.sf[,2],index)$y
nwl.sf.e <- approx(index,nwl.sf[,3],index)$y
nwl.sf.n <- approx(index,nwl.sf[,4],index)$y 
nwl.tr <- cbind(nwl.sf.a,nwl.sf.w,nwl.sf.e,nwl.sf.n)

fwl.sf <- rbind(fwl.sf.nfi6$x, naat[1:6,],fwl.sf.nfi7$x, naat, fwl.sf.nfi90)
fwl.sf.a <- approx(index,fwl.sf[,1],index)$y 
fwl.sf.w <- approx(index,fwl.sf[,2],index)$y
fwl.sf.e <- approx(index,fwl.sf[,3],index)$y
fwl.sf.n <- approx(index,fwl.sf[,4],index)$y 
fwl.tr <- cbind(fwl.sf.a,fwl.sf.w,fwl.sf.e,fwl.sf.n)

## adding litters from loggings & mortality & understorey
und.a <- rep(und.sf.nfi6$x[1], (yr-1971)) #note: (yr-1971) for 2012
und.w <- rep(und.sf.nfi6$x[2], (yr-1971))
und.e <- rep(und.sf.nfi6$x[3], (yr-1971))
und.n <- rep(und.sf.nfi6$x[4], (yr-1971))
und.mat <- cbind(und.a,und.w, und.e,und.n)

nwl <- nwl.tr+log.nwl.SF.min[3:dim(log.nwl.SF.min)[1], ]+nat.nwl.SF.min[3:dim(nat.nwl.SF.min)[1], ]+und.mat
fwl <- fwl.tr+log.fwl.SF.min[3:dim(log.fwl.SF.min)[1], ]+nat.fwl.SF.min[3:dim(nat.fwl.SF.min)[1], ]
cwl <- log.cwl.SF.min[3:dim(log.cwl.SF.min)[1], ]+nat.cwl.SF.min[3:dim(nat.cwl.SF.min)[1], ]



# for the figures 
nwl.sf <- nwl
fwl.sf <- fwl
cwl.sf <- cwl

litter.sf <- cbind(rowSums(nwl), rowSums(fwl), rowSums(cwl))
write.csv(litter.sf, (paste(resultspath,"litter.sf.",yr,".csv", sep="")))

litter.sf.awenh <- cbind(nwl, fwl, cwl)
write.csv(litter.sf.awenh, (paste(resultspath,"litter.sf.awenh.csv", sep="")))


# second total litter for 1972-
year.sf <- seq(1972,yr, by=1)
num <- length(year.sf)  ## note here one row for spin-up

nw.c.s <- matrix(nrow=num, ncol=5)
fw.c.s <- matrix(nrow=num, ncol=5)
cw.c.s <- matrix(nrow=num, ncol=5)

## here FOR LOOPS !
source(paste(ypath,"yasso07.R",sep=""))
dyn.load(paste(ypath,"yasso07.so",sep=""))

t <- 1
s <- 0
z <- c(0,0,0,0,0)
for(i in 1:(num)) {
  if (i==1) {init.s <- spin.nwl.sf.s} else {init.s <- nw.c.s[(i-1),]}
  inf <-  c(nwl[i,],0)
  cl <- clim7012.sf[i,]
  #cl <- c(tem.sf[(11+i),1],pre.sf[(11+i),1],amp.sf[(11+i),1])
  nw.c.s[(i), ] <- round(yasso07(a.ska, t, cl, init.s, inf, s, z),5)
 
}

t <- 1
s <- 2
z <- c(0,0,0,0,0)
for(i in 1:(num)) {
  if (i==1) {init.s <- spin.fwl.sf.s} else {init.s <- fw.c.s[(i-1),]}
  inf <-  c(fwl[i,],0)
  cl <- clim7012.sf[i,]
  #cl <- c(tem.sf[(11+i),1],pre.sf[(11+i),1],amp.sf[(11+i),1])
  fw.c.s[(i), ] <- round(yasso07(a.ska, t, cl, init.s, inf, s, z),5)
}

t <- 1
s <- 15
z <- c(0,0,0,0,0)
for(i in 1:(num)) {
  if (i==1) {init.s <- spin.cwl.sf.s} else {init.s <- cw.c.s[(i-1),]}
  inf <-  c(cwl[i,],0)
  cl <- clim7012.sf[i,]
  #cl <- c(tem.sf[(11+i),1],pre.sf[(11+i),1],amp.sf[(11+i),1])
  cw.c.s[(i), ] <- round(yasso07(a.ska, t, cl, init.s, inf, s, z),5)
}


soil.sf.awenh <- cbind(nw.c.s, fw.c.s, cw.c.s)
write.csv(soil.sf.awenh, (paste(resultspath,"soil.sf.awenh.csv", sep="")))
###############
soil.c.s <- nw.c.s+fw.c.s+cw.c.s
d.soil.c.sf.s <-  diff(rowSums(soil.c.s))

id <- seq(1,length(d.soil.c.sf.s), by=1)
#################################
#### averaging for 5y floating mean
d.soil.c.sf.s.out <- rep(0,times=max(id))


for(i in 1:(max(id))){
if (i==1) {
d.soil.c.sf.s.out[i] <- mean(d.soil.c.sf.s[(i):(i+2)])
}
if (i==2) {
d.soil.c.sf.s.out[i] <- mean(d.soil.c.sf.s[(i-1):(i+2)])
}
if (i>2 & i<max(id)-1) {
d.soil.c.sf.s.out[i] <- mean(d.soil.c.sf.s[(i-2):(i+2)])
}
if (i==max(id)-1) {
d.soil.c.sf.s.out[i] <- mean(d.soil.c.sf.s[(i-2):(i+1)])
} 
if (i==max(id)) {
d.soil.c.sf.s.out[i] <- mean(d.soil.c.sf.s[(i-2):(i)])
}
}

write.csv(d.soil.c.sf.s.out, (paste(resultspath,"d.soil.c.tot.sf.s.mean.csv", sep="")))

write.csv(d.soil.c.sf.s.out, (paste(NIR,"appendix/Appendix6f_min_sf.csv", sep="")))

## data for MTT, average of 1990-2011
#Scandinavian parameters

  forest.soil <-  nw.c.s[19:(yr-1972),]+fw.c.s[19:(yr-1972),]+cw.c.s[19:(yr-1972),]
 forest.soil.sf.ave <- colMeans(forest.soil)

 names(forest.soil.sf.ave) <- c('A','W','E','N','H')

write.table(forest.soil.sf.ave, paste(crf,"/mtt/forest.soil.sf.ave.csv", sep=""), sep=" ", row.names = TRUE, col.names = TRUE, quote=FALSE)


### Here stocks for uncertainty estimation

# soil.sf.1989.annual <- rbind(nw.c[18,],fw.c[18,],cw.c[18,])
# write.csv(soil.sf.1989.annual, (paste(uncertainty,"soil.sf.1989.annual.csv", sep="")),row.names=FALSE,col.names=FALSE)


##################################################
# 5. Simulation of time series  NORTHERN FINLAND #
##################################################


### Northern Finland!!!

nwl.nf.nfi7 <-  c(read.csv(paste(nfidata,"nfi7/litter/nwl.nf.nfi7.csv", sep=""), header=TRUE))
fwl.nf.nfi7 <-  c(read.csv(paste(nfidata,"nfi7/litter/fwl.nf.nfi7.csv", sep=""), header=TRUE))

nwl.nf.nfi90 <-  (read.csv(paste(nfidata,"nfi10/litter/nwl.nf.1990",yr,".csv", sep=""), header=TRUE))
fwl.nf.nfi90 <-  (read.csv(paste(nfidata,"nfi10/litter/fwl.nf.1990",yr,".csv", sep=""), header=TRUE))
# first tree litter for 1975-2011, living trees !!!
nwl.nf.nfi6 <-  c(read.csv(paste(nfidata,"nfi6/litter/nwl.nf.nfi6.csv", sep=""), header=TRUE))
fwl.nf.nfi6 <-  c(read.csv(paste(nfidata,"nfi6/litter/fwl.nf.nfi6.csv", sep=""), header=TRUE))


## interpolation of times series

naat <- matrix(NA, nrow = 6, ncol=4)
naat2 <- matrix(NA, nrow = 7, ncol=4)
nwl.nf <- rbind(nwl.nf.nfi6$x, naat2, nwl.nf.nfi7$x, naat, nwl.nf.nfi90 )
index <- seq(1,dim(nwl.nf)[1], by=1)
nwl.nf.a <- approx(index,nwl.nf[,1],index)$y 
nwl.nf.w <- approx(index,nwl.nf[,2],index)$y
nwl.nf.e <- approx(index,nwl.nf[,3],index)$y
nwl.nf.n <- approx(index,nwl.nf[,4],index)$y 
nwl.tr <- cbind(nwl.nf.a,nwl.nf.w,nwl.nf.e,nwl.nf.n)

fwl.nf <- rbind(fwl.nf.nfi6$x, naat2, fwl.nf.nfi7$x, naat, fwl.nf.nfi90 )
fwl.nf.a <- approx(index,fwl.nf[,1],index)$y 
fwl.nf.w <- approx(index,fwl.nf[,2],index)$y
fwl.nf.e <- approx(index,fwl.nf[,3],index)$y
fwl.nf.n <- approx(index,fwl.nf[,4],index)$y 
fwl.tr <- cbind(fwl.nf.a,fwl.nf.w,fwl.nf.e,fwl.nf.n)

## adding litters from loggings & mortality & understorey
und.a <- rep(und.nf.nfi6$x[1], (yr-1973))   # NOTE !! 38 for 2012   
und.w <- rep(und.nf.nfi6$x[2], (yr-1973))
und.e <- rep(und.nf.nfi6$x[3], (yr-1973))
und.n <- rep(und.nf.nfi6$x[4], (yr-1973))
und.mat <- cbind(und.a,und.w, und.e,und.n)

nwl <- nwl.tr+log.nwl.NF.min+nat.nwl.NF.min+und.mat
fwl <- fwl.tr+log.fwl.NF.min+nat.fwl.NF.min
cwl <- log.cwl.NF.min+nat.cwl.NF.min


# for the figures 
nwl.nf <- nwl
fwl.nf <- fwl
cwl.nf <- cwl

litter.nf <- cbind(rowSums(nwl), rowSums(fwl), rowSums(cwl))
write.csv(litter.nf, (paste(resultspath,"litter.nf.",yr,".csv", sep="")))

litter.nf.awenh <- cbind(nwl, fwl, cwl)
write.csv(litter.nf.awenh, (paste(resultspath,"litter.nf.awenh.csv", sep="")))

# second total litter for 1975-2011
year.nf <- seq(1975,yr, by=1)
num <- length(year.nf)  ## note here one row for spin-up

nw.c.s <- matrix(nrow=num, ncol=5)
fw.c.s <- matrix(nrow=num, ncol=5)
cw.c.s <- matrix(nrow=num, ncol=5)


## here FOR LOOPS !
source(paste(ypath,"yasso07.R",sep=""))
dyn.load(paste(ypath,"yasso07.so",sep=""))

t <- 1
s <- 0
z <- c(0,0,0,0,0)
for(i in 1:(num)) {

  if (i==1) {init.s <- spin.nwl.nf.s} else {init.s <- nw.c.s[(i-1),]}

  inf <-  c(nwl[i,],0)
  cl <- clim7012.nf[i,]
  #cl <- c(tem.nf[(14+i),1],pre.nf[(14+i),1],amp.nf[(14+i),1])
  nw.c.s[(i), ] <- round(yasso07(a.ska, t, cl, init.s, inf, s, z),5)
 
}

t <- 1
s <- 2
z <- c(0,0,0,0,0)

for(i in 1:(num)) {

  if (i==1) {init.s <- spin.fwl.nf.s} else {init.s <- fw.c.s[(i-1),]}

  inf <-  c(fwl[i,],0)
  cl <- clim7012.nf[i,]
  #cl <- c(tem.nf[(14+i),1],pre.nf[(14+i),1],amp.nf[(14+i),1])
  fw.c.s[(i), ] <- round(yasso07(a.ska, t, cl, init.s, inf, s, z),5)
 
}

t <- 1
s <- 15
z <- c(0,0,0,0,0)


for(i in 1:(num)) {
  if (i==1) {init.s <- spin.cwl.nf.s} else {init.s <- cw.c.s[(i-1),]}

  inf <-  c(cwl[i,],0)
  cl <- clim7012.nf[i,]
  #cl <- c(tem.nf[(14+i),1],pre.nf[(14+i),1],amp.nf[(14+i),1])
  cw.c.s[(i), ] <- round(yasso07(a.ska, t, cl, init.s, inf, s, z),5)
 
}




soil.nf.awenh <- cbind(nw.c.s, fw.c.s, cw.c.s)
write.csv(soil.nf.awenh, (paste(resultspath,"soil.nf.awenh.csv", sep="")))
###############


soil.c.s <- nw.c.s+fw.c.s+cw.c.s

d.soil.c.nf.s <-  diff(rowSums(soil.c.s))



id <- seq(1,length(d.soil.c.nf.s), by=1)
#################################
#### averaging for 5y floating mean
d.soil.c.nf.s.out <- rep(0,times=max(id))


for(i in 1:(max(id))){
if (i==1) {
d.soil.c.nf.s.out[i] <- mean(d.soil.c.nf.s[(i):(i+2)])
}
if (i==2) {
d.soil.c.nf.s.out[i] <- mean(d.soil.c.nf.s[(i-1):(i+2)])
}
if (i>2 & i<max(id)-1) {
d.soil.c.nf.s.out[i] <- mean(d.soil.c.nf.s[(i-2):(i+2)])
}
if (i==max(id)-1) {
d.soil.c.nf.s.out[i] <- mean(d.soil.c.nf.s[(i-2):(i+1)])
} 
if (i==max(id)) {
d.soil.c.nf.s.out[i] <- mean(d.soil.c.nf.s[(i-2):(i)])
}
}

write.csv(d.soil.c.nf.s.out, (paste(resultspath,"d.soil.c.tot.nf.s.mean.csv", sep="")))

write.csv(d.soil.c.nf.s.out, (paste(NIR,"appendix/Appendix6f_min_nf.csv", sep="")))


## data for MTT, average of 1990-2011

# #
  forest.soil <-  nw.c.s[16:(yr-1974),]+fw.c.s[16:(yr-1974),]+cw.c.s[16:(yr-1974),]
  forest.soil.nf.ave <- colMeans(forest.soil)
  names(forest.soil.nf.ave) <- c('A','W','E','N','H')

write.table(forest.soil.nf.ave, paste(crf,"/mtt/forest.soil.nf.ave.csv", sep=""), sep=" ", row.names = TRUE, col.names = TRUE, quote=FALSE)

## printing C inputs for the LUC-calculation ######
FL.litter=cbind(colMeans(nwl.nf),colMeans(fwl.nf),colMeans(cwl.nf),colMeans(nwl.sf),colMeans(fwl.sf),colMeans(cwl.sf))
FL.litter=rbind(FL.litter,c(0,0,0,0,0,0))
rownames(FL.litter)=cbind("A","W","E","N","H")
colnames(FL.litter)=cbind("nwl.NF","fwl.NF","cwl.NF","nwl.SF","fwl.SF","cwl.SF")
write.table(FL.litter, paste(path,yr,"/soil/results/","FL.soil.CInput.csv", sep=""), sep=" ", row.names = TRUE, col.names = TRUE, quote=FALSE)
########

##############################################
###
### Reading in land areas, estimating total emissions for Finland, 
### writing output to crf and NIR folders
##############################################



lulucfareas <-  read.table(paste(areas,"lulucf/results/lulucf_rem_all.txt", sep=""), header=TRUE)

lu.sf <- subset(lulucfareas, (lulucfareas$region==1 & lulucfareas$ipcc==1 & lulucfareas$soil==1)) #note: check!
#lu.nf <- lulucfareas[25,] #note: check!
lu.nf <- subset(lulucfareas, (lulucfareas$region==2 & lulucfareas$ipcc==1 & lulucfareas$soil==1))




rep.years <- seq(1990,yr)
nobs <- length(rep.years)

################ Scandinavian parameters  ### to be used in the reporting !!!

lu.emis.sf.s <- lu.sf[years]*d.soil.c.sf.s.out[(length(d.soil.c.sf.s.out)-nobs+1):length(d.soil.c.sf.s.out)]
lu.emis.nf.s <- lu.nf[years]*d.soil.c.nf.s.out[(length(d.soil.c.nf.s.out)-nobs+1):length(d.soil.c.nf.s.out)]

lu.emis.s <- lu.emis.sf.s+lu.emis.nf.s
lu.sink.co2.s <-  lu.emis.s*(44/12)

LU5A1.mineral.soil <- (round(lu.emis.s/1000,3))
rownames(LU5A1.mineral.soil) <- "#fl.rem.fl# {33B17FCF-CEF0-4C52-A083-41E77975CC17}"



write.table(LU5A1.mineral.soil, paste(crf,"LU4A1_FL-FL_Mineral_soil.csv", sep=""), sep=" ", row.names = TRUE, col.names = FALSE, quote=FALSE)

