#5.10.2012: Updated to 2011

### Taken to the hsan2 25.10.2010
#######################
##############   15.10.2009 yasso model for drained organic soils

#Modified from 2009 inventory to 2010 inventory, 24.5.2011, PPuo
# taken to be used for inventory 10.10.2011 Aleh
# NOTE THIS IS FOR UNFCCC REPORTING!!

yr <- 2018
yr.VANHA <- 2017

path <- '/hsan2/khk/ghg/'

#Input paths:

funpath <- paste(path,yr,'/soil/functions/',sep="")
nfidata <- paste(path,yr,'/soil/nfidata/',sep="")
draindata <- paste(path,yr,'/trees/drain/remaining/litter/lulucf/',sep="")
figpath <- paste(path,yr,'/soil/organic/remaining/figs/', sep="")
resultspath <- paste(path,'/development/organicsoils/results/', sep="")
undpath <- paste(path,yr,'/soil/understorey/litter/',sep="")
areas <- paste(path,yr,'/areas/',sep="")
crf <- paste(path,yr,'/crf/',sep="")
nir <- paste(path,yr,'/NIR/',sep="")


# Southern Finland

# UND
und.org.abv <-  (read.csv(paste(undpath,"und.org.abv.csv", sep=""), header=TRUE))
und.org.bel <-  (read.csv(paste(undpath,"und.org.bel.csv", sep=""), header=TRUE))

# TRE

#NOTE: Here 2011!
nwl.sf.nfi9013.org.abv <-  (read.csv(paste(nfidata,"nfi10/litter/nwl.sf.1990",yr,".org.abv.csv", sep=""), header=TRUE))
fwl.sf.nfi9013.org.abv <-  (read.csv(paste(nfidata,"nfi10/litter/fwl.sf.1990",yr,".org.abv.csv", sep=""), header=TRUE))
nwl.sf.nfi9013.org.bel <-  (read.csv(paste(nfidata,"nfi10/litter/nwl.sf.1990",yr,".org.bel.csv", sep=""), header=TRUE))
fwl.sf.nfi9013.org.bel <-  (read.csv(paste(nfidata,"nfi10/litter/fwl.sf.1990",yr,".org.bel.csv", sep=""), header=TRUE))


#LOG
log.nwl.SF.org.abv <-  read.csv(paste(draindata,"log.nwl.SF.org.abv.csv", sep=""), header=TRUE)
log.nwl.SF.org.bel <-  read.csv(paste(draindata,"log.nwl.SF.org.bel.csv", sep=""), header=TRUE)
log.fwl.SF.org.abv <-  read.csv(paste(draindata,"log.fwl.SF.org.abv.csv", sep=""), header=TRUE)
log.fwl.SF.org.bel <-  read.csv(paste(draindata,"log.fwl.SF.org.bel.csv", sep=""), header=TRUE)
log.cwl.SF.org.abv <-  read.csv(paste(draindata,"log.cwl.SF.org.abv.csv", sep=""), header=TRUE)

#NAT
nat.nwl.SF.org.abv <-  read.csv(paste(draindata,"nat.nwl.SF.org.abv.csv", sep=""), header=TRUE)
nat.nwl.SF.org.bel <-  read.csv(paste(draindata,"nat.nwl.SF.org.bel.csv", sep=""), header=TRUE)
nat.fwl.SF.org.abv <-  read.csv(paste(draindata,"nat.fwl.SF.org.abv.csv", sep=""), header=TRUE)
nat.fwl.SF.org.bel <-  read.csv(paste(draindata,"nat.fwl.SF.org.bel.csv", sep=""), header=TRUE)
nat.cwl.SF.org.abv <-  read.csv(paste(draindata,"nat.cwl.SF.org.abv.csv", sep=""), header=TRUE)

## DOM
dom.sf <- c(0,0,0,0,0,0,0,0,rep(0.00754106277747484,(yr-1997)))
# based on NFI9 and NFI10 permanent sample plots !

#####################
# 3b. Northern Finland
#####################

# UND
und.org.abv <-  (read.csv(paste(undpath,"und.org.abv.csv", sep=""), header=TRUE))
und.org.bel <-  (read.csv(paste(undpath,"und.org.bel.csv", sep=""), header=TRUE))

# TRE
nwl.nf.nfi9013.org.abv <-  (read.csv(paste(nfidata,'nfi10/litter/nwl.nf.1990',yr,'.org.abv.csv', sep=""), header=TRUE))
fwl.nf.nfi9013.org.abv <-  (read.csv(paste(nfidata,'nfi10/litter/fwl.nf.1990',yr,'.org.abv.csv', sep=""), header=TRUE))
nwl.nf.nfi9013.org.bel <-  (read.csv(paste(nfidata,'nfi10/litter/nwl.nf.1990',yr,'.org.bel.csv', sep=""), header=TRUE))
fwl.nf.nfi9013.org.bel <-  (read.csv(paste(nfidata,'nfi10/litter/fwl.nf.1990',yr,'.org.bel.csv', sep=""), header=TRUE))

#LOG
log.nwl.NF.org.abv <-  read.csv(paste(draindata,"log.nwl.NF.org.abv.csv", sep=""), header=TRUE)
log.nwl.NF.org.bel <-  read.csv(paste(draindata,"log.nwl.NF.org.bel.csv", sep=""), header=TRUE)
log.fwl.NF.org.abv <-  read.csv(paste(draindata,"log.fwl.NF.org.abv.csv", sep=""), header=TRUE)
log.fwl.NF.org.bel <-  read.csv(paste(draindata,"log.fwl.NF.org.bel.csv", sep=""), header=TRUE)
log.cwl.NF.org.abv <-  read.csv(paste(draindata,"log.cwl.NF.org.abv.csv", sep=""), header=TRUE)

#NAT
nat.nwl.NF.org.abv <-  read.csv(paste(draindata,"nat.nwl.NF.org.abv.csv", sep=""), header=TRUE)
nat.nwl.NF.org.bel <-  read.csv(paste(draindata,"nat.nwl.NF.org.bel.csv", sep=""), header=TRUE)
nat.fwl.NF.org.abv <-  read.csv(paste(draindata,"nat.fwl.NF.org.abv.csv", sep=""), header=TRUE)
nat.fwl.NF.org.bel <-  read.csv(paste(draindata,"nat.fwl.NF.org.bel.csv", sep=""), header=TRUE)
nat.cwl.NF.org.abv <-  read.csv(paste(draindata,"nat.cwl.NF.org.abv.csv", sep=""), header=TRUE)

### DOM
# based on NFI9 and NFI10 permanent sample plot data
dom.nf <- c(0,0,0,0,0,0,0,0,0,0,0,0, rep(0.00720002212248889,(yr-2001)))

###Annual litter flux to belowground

nwl.bel <- nwl.sf.nfi9013.org.bel+log.nwl.SF.org.bel[21:dim(log.nwl.SF.org.bel)[1], ]+nat.nwl.SF.org.bel[21:dim(nat.nwl.SF.org.bel)[1], ]+t(matrix(und.org.bel))
fwl.bel <- fwl.sf.nfi9013.org.bel+log.fwl.SF.org.bel[21:dim(log.fwl.SF.org.bel)[1], ]+nat.fwl.SF.org.bel[21:dim(nat.fwl.SF.org.bel)[1], ]



litter.bel.sf <- rowSums(nwl.bel+fwl.bel)

#### Annual emissions from drained peat sites  g C m-2 a-1 to ton C /ha (note CARBON)
### new emission values from Ojanen et al.2013

#Rhtkg <- 425.7*0.01
#Mtkg <- 312.1*0.01
#Ptkg <- 242.3*0.01
#Vatkg <- 218.9*0.01
#Jatkg <- 185.2*0.01

Rhtkg <- 475.3*0.01
Mtkg <- 439.6*0.01
Ptkg <- 351.8*0.01
Vatkg <- 291.4*0.01
Jatkg <- 185.2*0.01

#Rhtkg.emis.sf <- (litter.bel.sf-Rhtkg)+dom.sf
#Mtkg.emis.sf <- (litter.bel.sf-Mtkg)+dom.sf
#Ptkg.emis.sf <- (litter.bel.sf-Ptkg)+dom.sf
#Vatkg.emis.sf <- (litter.bel.sf-Vatkg)+dom.sf
#Jatkg.emis.sf <- (litter.bel.sf-Jatkg)+dom.sf

#adding above-gound litter 
Rhtkg.emis.sf <- (litter.bel.sf+litter.abv.sf-Rhtkg)+dom.sf
Mtkg.emis.sf <- (litter.bel.sf+litter.abv.sf-Mtkg)+dom.sf
Ptkg.emis.sf <- (litter.bel.sf+litter.abv.sf-Ptkg)+dom.sf
Vatkg.emis.sf <- (litter.bel.sf+litter.abv.sf-Vatkg)+dom.sf
Jatkg.emis.sf <- (litter.bel.sf+litter.abv.sf-Jatkg)+dom.sf
## Writing tkg emissions

tkg.emiss.sf <- rbind(Rhtkg.emis.sf,Mtkg.emis.sf,Ptkg.emis.sf,Vatkg.emis.sf,Jatkg.emis.sf)
write.csv(tkg.emiss.sf, (paste(resultspath,"tkg.emiss.sf.abovelitter.csv", sep="")))
# NIR appendix
#write.csv(tkg.emiss.sf, (paste(nir,"appendix/Appendix6f_org_sf.csv", sep="")))

###Annual litter flux to belowground

nwl.bel <- nwl.nf.nfi9013.org.bel+log.nwl.NF.org.bel[9:dim(log.nwl.NF.org.bel)[1], ]+nat.nwl.NF.org.bel[9:dim(nat.nwl.NF.org.bel)[1], ]+t(matrix(und.org.bel))
fwl.bel <- fwl.nf.nfi9013.org.bel+log.fwl.NF.org.bel[9:dim(log.fwl.NF.org.bel)[1], ]+nat.fwl.NF.org.bel[9:dim(nat.fwl.NF.org.bel)[1], ]


litter.bel.nf <- rowSums(nwl.bel+fwl.bel)

#### Annual emissions from drined peat sites  g C m-2 a-1 to ton C /ha (note CARBON)
### new emission values from Ojanen et al.2013

#Rhtkg <- 425.7*0.01
#Mtkg <- 312.1*0.01
#Ptkg <- 242.3*0.01
#Vatkg <- 218.9*0.01
#Jatkg <- 185.2*0.01

Rhtkg <- 475.3*0.01
Mtkg <- 439.6*0.01
Ptkg <- 351.8*0.01
Vatkg <- 291.4*0.01
Jatkg <- 185.2*0.01

#Rhtkg.emis.sf <- (litter.bel.sf-Rhtkg)+dom.sf
#Mtkg.emis.sf <- (litter.bel.sf-Mtkg)+dom.sf
#Ptkg.emis.sf <- (litter.bel.sf-Ptkg)+dom.sf
#Vatkg.emis.sf <- (litter.bel.sf-Vatkg)+dom.sf
#Jatkg.emis.sf <- (litter.bel.sf-Jatkg)+dom.sf

#adding above-gound litter 
Rhtkg.emis.sf <- (litter.bel.sf+litter.abv.nf-Rhtkg)+dom.sf
Mtkg.emis.sf <- (litter.bel.sf+litter.abv.nf-Mtkg)+dom.sf
Ptkg.emis.sf <- (litter.bel.sf+litter.abv.nf-Ptkg)+dom.sf
Vatkg.emis.sf <- (litter.bel.sf+litter.abv.nf-Vatkg)+dom.sf
Jatkg.emis.sf <- (litter.bel.sf+litter.abv.nf-Jatkg)+dom.sf


## 
tkg.emiss.nf <- rbind(Rhtkg.emis.nf,Mtkg.emis.nf,Ptkg.emis.nf,Vatkg.emis.nf,Jatkg.emis.nf)
write.csv(tkg.emiss.nf, (paste(resultspath,"tkg.emiss.nf.csv", sep="")))
# NIR appendix
#write.csv(tkg.emiss.nf, (paste(nir,"appendix/Appendix6f_org_nf.csv", sep="")))
##################################################
# 6. MAKING SOME GRAPHS                          #
##################################################



##############################################
###
### Reading in land areas, estimating total emissions for Finland, 
### writing output to crf and NIR folders
##############################################


lulucfareas <-  read.table(paste(areas,"lulucf/results/lulucf_classes_all.txt", sep=""), header=TRUE)
# according to the vegetation class
lulucfareaskpy <-  read.table(paste(areas,"lulucf/results/lulucf_rem_kptyy.txt", sep=""), header=TRUE)



lu.sf <- subset(lulucfareaskpy, (lulucfareaskpy$region==1 & lulucfareaskpy$kptyy %in% c(1,3,4,5,6)))
lu.nf <- subset(lulucfareaskpy, (lulucfareaskpy$region==2 & lulucfareaskpy$kptyy %in% c(1,3,4,5,6)))

rep.years <- seq(1990,yr)
nobs <- length(rep.years)

emis.sf.lu <- tkg.emiss.sf * lu.sf[,5:dim(lu.sf)[2]]
emis.nf.lu <- tkg.emiss.nf * lu.nf[,5:dim(lu.nf)[2]]

emis.lu <- colSums(emis.sf.lu+emis.nf.lu)
emis.lu.co2 <- emis.lu*(44/12)


## export to CRF folder !!!
## CO2 emissions Gg C for forest land remaining forest land

LU4A21.organic.soil <- t(round(emis.lu/1000,3))
rownames(LU4A21.organic.soil) <- "#orga.soil,ktC# {1632C1F2-832E-48D5-BA76-AA1DFAA643DC}"

write.table(LU4A21.organic.soil, paste(resultspath,"LU4A1_FL-FL_Organic_soil.csv", sep=""), sep=" ", row.names = TRUE, col.names = FALSE, quote=FALSE)



#plot(seq(1990,yr), LU4A21.organic.soil, type="l", col="blue", ylab="carbon stock change Gg C", main="Carbon stock change of drained organic soils, Finland", xlab="years")
dev.print(file=paste(figpath,"emissions.drained.org.lulucf.eps", sep=""), height=12, width=10, horizontal=FALSE)


