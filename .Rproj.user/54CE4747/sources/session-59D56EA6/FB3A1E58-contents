############

#LULUCF

#########

#Updated for 2018 inventory 26.6.2019 Paula

source('Z:/data/d4/projects/khk/ghg/2019/config/paths.r')
source('/hsan2/khk/ghg/2019/config/parameters.r')


#############################################
# INPUT PATHS:
#############################################

# functions for biomass, volume and litter
path.func <- paste(path,yr,'/soil/functions/',sep="")
#litterfall
source(paste(path.func,"litter.r", sep=""))
#Statistics for total drain:
path.drain <- paste(path,yr,'/trees/drain/remaining/data/',sep="")
#Deforestation drain:
path.defor <- paste(path,yr,'/trees/drain/converted/defor/output/',sep="") 
#Afforestation drain
path.afor <- paste(path,yr,'/trees/drain/converted/afor/data/',sep="") 
#befs
path.bef <- paste(path,yr,'/trees/drain/remaining/BEF/',sep="")
#fineroot befs:
path.nfi7.bef <- paste(path,yr,'/soil/nfidata/nfi7/BEF/',sep="")

#############################################
#OUTPUT PATHS:
#############################################
figpath <-  paste(path,yr,'/trees/drain/remaining/figs/',sep="")
path.litter <- paste(path,yr,'/trees/drain/remaining/litter/lulucf/',sep="")

##############################################
#Read in drain data
##############################################


drain.div <-  read.csv(paste(path.drain,"Drain.divison.csv", sep=""), header=TRUE, as.is=TRUE)

drain.fore.cent <-  read.table(paste(path.drain,"Drain.For.Cent.csv", sep=""), sep=",", header=TRUE, as.is=TRUE)
colnames(drain.fore.cent) <- c("MK","SPEC", c(1986:2014)) #From 2015 onwards use maakunta

drain.maak <- read.table(paste(path.drain, "Drain.maak.csv", sep=""), sep=",", header=TRUE)
colnames(drain.maak) <- c("year","region","Scots pine","Norway spruce","Birch")

drain.hist <-  read.csv(paste(path.drain,"Drain.historical.csv", sep=""), header=TRUE, as.is=TRUE)

NatMort.shares <-  read.csv(paste(path.drain, "Natmort.shares.NFI10.csv", sep=""), header=TRUE, as.is=TRUE)

drain.energy <-  read.csv(paste(path.drain, "Drain.energy_v2.csv", sep=""), header=TRUE, as.is=TRUE) #v2 korjattu pienemmaksi hukkapuuta #1998-2020
#drain.energy <-  read.csv(paste(path.drain, "Drain.energy.csv", sep=""), header=TRUE, as.is=TRUE)

drain.energy.imp <-  read.table(paste(path.drain, "Energy.wood.imports.csv", sep=""), sep=";", header=FALSE, as.is=TRUE) #1990-2019
drain.energy.exp <-  read.table(paste(path.drain, "Energy.wood.exports.csv", sep=""),  sep=";", header=FALSE, as.is=TRUE) #1990-2019

drain.energy.species <-  read.table(paste(path.drain, "Energy.wood.stem.species.csv", sep=""),  sep=",", header=TRUE, as.is=TRUE) #1990-2019


##############################################
###  ENERGY WOOD CALCULATION
##############################################

# here removing the effect of net imports and exports
# unit 1000m3

drain.energy.net = drain.energy.imp[9:dim(drain.energy.imp)[1], 2]-drain.energy.exp[9:dim(drain.energy.exp)[1], 2]

drain.energy[,4] <- drain.energy[,4] - drain.energy.net


##Read in annual data on SF/NF ratio.
chips.cons <-  read.csv(paste(path.drain, "Forest.chips.consumption_v2.csv", sep=""), header=TRUE, as.is=TRUE) # 2003-2019
#chips.cons <-  read.csv(paste(path.drain, "Forest.chips.consumption.csv", sep=""), header=TRUE, as.is=TRUE) # 2003-2019



#Calculate ratios of SF/(SF+NF) consumption 1998-2019 (1998-2002 from 2003 data)
chips.tot <- rbind (chips.cons[1,], chips.cons[1,],chips.cons[1,], chips.cons[1,], chips.cons[1,], chips.cons)

chips.ratio <- matrix(0, nrow=dim(chips.tot)[1], ncol= 3)
colnames(chips.ratio) <- c( "stumps", "log.res", "stems")

chips.ratio[,1] <- chips.tot[,2]/(chips.tot[,2]+chips.tot[,3]) #stumps
chips.ratio[,2] <- chips.tot[,4]/(chips.tot[,4]+chips.tot[,5]) #log.res
chips.ratio[,3] <- chips.tot[,6]/(chips.tot[,6]+chips.tot[,7]) #stems


# harvest residues, 75% for Spruce sites !!

NS.rat <- 0.75

Spruce.dens <- 0.385  # Hakkila
Pine.dens <- 0.39  # Hakkila

#Stumps
# note unit conversion to mill m3 and further to mill tonnes					
stumps.SF <- c(rep(0,28),chips.ratio[,1]*drain.energy[,2]*Spruce.dens/1000) 
stumps.NF <- c(rep(0,28),((1-chips.ratio[,1])*drain.energy[,2]*Spruce.dens/1000))

# harvesting slash below
harves.SF.NS <-  c(rep(0,28),chips.ratio[,2]*drain.energy[,3]*Spruce.dens/1000)*NS.rat
harves.NF.NS <-  c(rep(0,28),(1-chips.ratio[,2])*drain.energy[,3]*Spruce.dens/1000)*NS.rat

harves.SF.SP <-  c(rep(0,28),chips.ratio[,2]*drain.energy[,3]*Pine.dens/1000)*(1-NS.rat)
harves.NF.SP <-  c(rep(0,28),(1-chips.ratio[,2])*drain.energy[,3]*Pine.dens/1000)*(1-NS.rat)


# stem residues below
# note unit conversion to mill m3 and further to mill tonnes
wastew.SF <- c(rep(0,28),chips.ratio[,3]*drain.energy[,4]*Spruce.dens/1000)
wastew.NF <- c(rep(0,28),(1-chips.ratio[,3])*drain.energy[,4]*Spruce.dens/1000)

##############################################################
#Natural mortality for calculating loggings from total drain
##############################################################

### here file for natural mortality, and making the time series

years <- seq(1970,yr,by=1)
natmort <- c((rep(1.16,10)),(rep(1.27,10)), drain.div$Nat.mort)
natural.mort <- cbind(years,natmort)
sf.nat.mort <- matrix(0,nrow=dim(natural.mort)[1], ncol=6)
sf.nat.mort[,1] <- natural.mort[,2]*NatMort.shares[1,4]
sf.nat.mort[,2] <- natural.mort[,2]*NatMort.shares[2,4]
sf.nat.mort[,3] <- natural.mort[,2]*NatMort.shares[3,4]
sf.nat.mort[,4] <- natural.mort[,2]*NatMort.shares[9,4]
sf.nat.mort[,5] <- natural.mort[,2]*NatMort.shares[10,4]
sf.nat.mort[,6] <- natural.mort[,2]*NatMort.shares[11,4]
colnames(sf.nat.mort) <- c("pine.min","spruce.min","birch.min","pine.org","spruce.org","birch.org")


nf.nat.mort <- matrix(0,nrow=dim(natural.mort)[1], ncol=6)
nf.nat.mort[,1] <- natural.mort[,2]*NatMort.shares[5,4]
nf.nat.mort[,2] <- natural.mort[,2]*NatMort.shares[6,4]
nf.nat.mort[,3] <- natural.mort[,2]*NatMort.shares[7,4]
nf.nat.mort[,4] <- natural.mort[,2]*NatMort.shares[13,4]
nf.nat.mort[,5] <- natural.mort[,2]*NatMort.shares[14,4]
nf.nat.mort[,6] <- natural.mort[,2]*NatMort.shares[15,4]
colnames(nf.nat.mort) <- c("pine.min","spruce.min","birch.min","pine.org","spruce.org","birch.org")

##############################################
# historical drain
##############################################

###### Here modifying historical drain to SF & NF

# North Finland == Kainuu, Pohjois-Pohjanmaa,Koillis-Suomi, Lappi
reg <- ifelse(drain.hist$Pmlnro<16,1,2)   # 

manty <- aggregate(drain.hist$Manty, by=list(year=drain.hist$Vuosi, region=reg), sum)
kuusi <- aggregate(drain.hist$Kuusi, by=list(year=drain.hist$Vuosi, region=reg), sum)
koivu <- aggregate(drain.hist$Koivu, by=list(year=drain.hist$Vuosi, region=reg), sum)

hist.drain <- cbind((c(seq(1953,1996,by=1),(seq(1953,1996,by=1)))),(c(rep(1,44),(rep(2,44)))),manty$x,kuusi$x, koivu$x)
colnames(hist.drain) <- c("year","region","Scots pine","Norway spruce","Birch")


############################################
## More recent drain here (1986-2014)
############################################

reg2 <-  ifelse(drain.fore.cent$MK<11,1,2)   # 

drain.fc <-matrix(0,nrow=6,ncol=dim(drain.fore.cent)[2])  # empty matrix for for loop (amount of years = number of columns)

end <- (dim(drain.fore.cent)[2]-2)
for(i in 1:end) {
  drain.fc[,(i+2)] <- (aggregate(drain.fore.cent[,(i+2)], by=list(species=drain.fore.cent$SPEC, region=reg2),sum))$x
}

drain.fc[,1] <- c(1,1,1,2,2,2)
drain.fc[,2] <- c(1,2,3,1,2,3)

drain.data <- t(drain.fc)
years2 <- seq(1986,2014, by=1)
drain.data2a <- as.data.frame((drain.data[3:(end+2),1:3])/1000)
drain.data2b <- as.data.frame((drain.data[3:(end+2),4:6])/1000)
regions <- c(rep(1,dim(drain.data2a)[1]),rep(2,dim(drain.data2a)[1]))

drain.data2 <- cbind((c(years2,years2)),regions, rbind(drain.data2a,drain.data2b))
colnames(drain.data2) <- c("year","region","Scots pine","Norway spruce","Birch")


###########################################################
### COMBINING DRAIN data (historical -1985 + recent 1986-)
###########################################################

start.SF <- hist.drain[18:33,]
start.NF <- hist.drain[62:77,]

end.SF <- drain.data2[1:(2014 -1986+1),]
end.NF <- drain.data2[(2014-1986+2):dim(drain.data2)[1],] 

#drain.SF <- rbind(start.SF,end.SF,drain.maak[1,]) #drain.maak for 2015-
#drain.NF <- rbind(start.NF,end.NF,drain.maak[2,])

drain.SF <- rbind(start.SF,end.SF,drain.maak[drain.maak$region==1,]) #drain.maak for 2015-
drain.NF <- rbind(start.NF,end.NF,drain.maak[drain.maak$region==2,])


########################################################
### Deforestation volumes
########################################################

def <-  read.table(paste(path.defor, "LULUCF_deforestation_vol.dat", sep=""), header=TRUE, as.is=TRUE) #m3
def <- def[,-(1:3)]/1000000 #mill m3
def <- t(def)
colnames(def) <- c('sf.min.sp','sf.min.ns','sf.min.b', 'sf.org.sp','sf.org.ns','sf.org.b', 'nf.min.sp','nf.min.ns','nf.min.b', 'nf.org.sp','nf.org.ns','nf.org.b')



########################################################
### Afforestation drain volumes
########################################################

aff <-  read.table(paste(path.afor, "UNFCCC_afor_volume.csv", sep=""), header=TRUE, sep="") #m3
aff <- aff[,-(1:3)]/1000000 #mill m3
aff <- t(aff)
colnames(aff) <- c('sf.min.sp','sf.min.ns','sf.min.b', 'sf.org.sp','sf.org.ns','sf.org.b', 'nf.min.sp','nf.min.ns','nf.min.b', 'nf.org.sp','nf.org.ns','nf.org.b')



#Sum up def and aff
affdef <- aff+def

def <- affdef #just to ease things up, def includes aff


#def <- rbind(def, def[dim(def)[1],]) ###POISTA TAMA KUN UUDET LUVUT


##############################################################
#Shares of logging on UNDRAINED organic soils, to be subtracted
##############################################################

pine.undrained.logg.SF <- 1-0.007
spruce.undrained.logg.SF <- 1-0.007
birch.undrained.logg.SF <- 1-0.013

pine.undrained.logg.NF <- 1-0.011
spruce.undrained.logg.NF <- 1-0.039
birch.undrained.logg.NF <- 1-0.054

##########################################################################
#Shares of logging on mineral and organic soils 
#########################################################################
#(from NFI10 permanent plots, from Antti Ihalainen 10/2011)
#pine.min.logg.SF <- 0.870
#spruce.min.logg.SF <- 0.887
#birch.min.logg.SF <- 0.811

#pine.min.logg.NF <- 0.867
#spruce.min.logg.NF <- 0.795
#birch.min.logg.NF <- 0.538


pine.min.logg.SF.1 <- 0.870
spruce.min.logg.SF.1 <- 0.887
birch.min.logg.SF.1 <- 0.811

pine.min.logg.NF.1 <- 0.867
spruce.min.logg.NF.1 <- 0.795
birch.min.logg.NF.1 <- 0.538

#from 2008-2009 to 2018-2019 (from Helena Henttonen 10/2020)
pine.min.logg.SF.2 <- 0.81
spruce.min.logg.SF.2 <- 0.89
birch.min.logg.SF.2 <- 0.82

pine.min.logg.NF.2 <- 0.78
spruce.min.logg.NF.2 <- 0.84
birch.min.logg.NF.2 <- 0.65

#Lets make a time series, before 2009 use NFI10, after that NFI11+NFI12

pine.min.logg.SF <- c(rep(pine.min.logg.SF.1,39),rep(pine.min.logg.SF.2,yr-2009+1))
spruce.min.logg.SF <- c(rep(spruce.min.logg.SF.1,39),rep(spruce.min.logg.SF.2,yr-2009+1))
birch.min.logg.SF <- c(rep(birch.min.logg.SF.1,39),rep(birch.min.logg.SF.2,yr-2009+1))

pine.min.logg.NF <- c(rep(pine.min.logg.NF.1,39),rep(pine.min.logg.NF.2,yr-2009+1))
spruce.min.logg.NF <- c(rep(spruce.min.logg.NF.1,39),rep(spruce.min.logg.NF.2,yr-2009+1))
birch.min.logg.NF <- c(rep(birch.min.logg.NF.1,39),rep(birch.min.logg.NF.2,yr-2009+1))

###########################################
### Estimating loggins
###########################################

pine.loggings.SF <- drain.SF[,3] - ( sf.nat.mort[,1] +  sf.nat.mort[,4])
spruce.loggings.SF <- drain.SF[,4] - ( sf.nat.mort[,2] +  sf.nat.mort[,5])
birch.loggings.SF <- drain.SF[,5] - ( sf.nat.mort[,3] +  sf.nat.mort[,6])

pine.loggings.NF <- drain.NF[,3] - ( nf.nat.mort[,1] +  nf.nat.mort[,4])
spruce.loggings.NF <- drain.NF[,4] - ( nf.nat.mort[,2] +  nf.nat.mort[,5])
birch.loggings.NF <- drain.NF[,5] - ( nf.nat.mort[,3] +  nf.nat.mort[,6])

#Loggings for biomass separately
pine.loggings.SF.bm <- pine.loggings.SF
spruce.loggings.SF.bm <- spruce.loggings.SF
birch.loggings.SF.bm <- birch.loggings.SF

pine.loggings.NF.bm <- pine.loggings.NF
spruce.loggings.NF.bm <- spruce.loggings.NF
birch.loggings.NF.bm <- birch.loggings.NF

##################################################################
#Subtract loggings on undrained organic soils FOR litter only!!!
#################################################################

#Ei tehda tata viela, koska ojittamattomilta soilta kerataan myos energiapuuta

#pine.loggings.SF <- pine.loggings.SF*pine.undrained.logg.SF
#spruce.loggings.SF <- spruce.loggings.SF*spruce.undrained.logg.SF
#birch.loggings.SF <- birch.loggings.SF*birch.undrained.logg.SF

#pine.loggings.NF <- pine.loggings.NF*pine.undrained.logg.NF
#spruce.loggings.NF <- spruce.loggings.NF*spruce.undrained.logg.NF
#birch.loggings.NF <- birch.loggings.NF*birch.undrained.logg.NF

##############################################################################
#Divide to mineral/drained organic, subtract deforestation and afforestation
##############################################################################
###Litter###
pine.loggings.SF.min <- pine.loggings.SF*pine.min.logg.SF - def[,'sf.min.sp']
pine.loggings.SF.org <- pine.loggings.SF*(1-pine.min.logg.SF) - def[,'sf.org.sp']

spruce.loggings.SF.min <- spruce.loggings.SF*spruce.min.logg.SF - def[,'sf.min.ns']
spruce.loggings.SF.org <- spruce.loggings.SF*(1-spruce.min.logg.SF) - def[,'sf.org.ns']

birch.loggings.SF.min <- birch.loggings.SF*birch.min.logg.SF - def[,'sf.min.b']
birch.loggings.SF.org <- birch.loggings.SF*(1-birch.min.logg.SF) - def[,'sf.org.b']

pine.loggings.NF.min <- pine.loggings.NF*pine.min.logg.NF - def[,'nf.min.sp']
pine.loggings.NF.org <- pine.loggings.NF*(1-pine.min.logg.NF) - def[,'nf.org.sp']

spruce.loggings.NF.min <- spruce.loggings.NF*spruce.min.logg.NF - def[,'nf.min.ns']
spruce.loggings.NF.org <- spruce.loggings.NF*(1-spruce.min.logg.NF) - def[,'nf.org.ns']

birch.loggings.NF.min <- birch.loggings.NF*birch.min.logg.NF - def[,'nf.min.b']
birch.loggings.NF.org <- birch.loggings.NF*(1-birch.min.logg.NF) - def[,'nf.org.b']

####Biomass####
pine.loggings.SF.min.bm <- pine.loggings.SF.bm*pine.min.logg.SF - def[,'sf.min.sp']
pine.loggings.SF.org.bm <- pine.loggings.SF.bm*(1-pine.min.logg.SF) - def[,'sf.org.sp']

spruce.loggings.SF.min.bm <- spruce.loggings.SF.bm*spruce.min.logg.SF - def[,'sf.min.ns']
spruce.loggings.SF.org.bm <- spruce.loggings.SF.bm*(1-spruce.min.logg.SF) - def[,'sf.org.ns']

birch.loggings.SF.min.bm <- birch.loggings.SF.bm*birch.min.logg.SF - def[,'sf.min.b']
birch.loggings.SF.org.bm <- birch.loggings.SF.bm*(1-birch.min.logg.SF) - def[,'sf.org.b']

pine.loggings.NF.min.bm <- pine.loggings.NF.bm*pine.min.logg.NF - def[,'nf.min.sp']
pine.loggings.NF.org.bm <- pine.loggings.NF.bm*(1-pine.min.logg.NF) - def[,'nf.org.sp']

spruce.loggings.NF.min.bm <- spruce.loggings.NF.bm*spruce.min.logg.NF - def[,'nf.min.ns']
spruce.loggings.NF.org.bm <- spruce.loggings.NF.bm*(1-spruce.min.logg.NF) - def[,'nf.org.ns']

birch.loggings.NF.min.bm <- birch.loggings.NF.bm*birch.min.logg.NF - def[,'nf.min.b']
birch.loggings.NF.org.bm <- birch.loggings.NF.bm*(1-birch.min.logg.NF) - def[,'nf.org.b']



#log.p <- apply(cbind(pine.loggings.SF.min,pine.loggings.NF.min,pine.loggings.SF.org,pine.loggings.NF.org), 1, sum)
#log.s <- apply(cbind(spruce.loggings.SF.min,spruce.loggings.NF.min,spruce.loggings.SF.org,spruce.loggings.NF.org), 1, sum)
#log.b <- apply(cbind(birch.loggings.SF.min,birch.loggings.NF.min,birch.loggings.SF.org,birch.loggings.NF.org), 1, sum)

#log <- apply(cbind(log.p,log.s,log.b), 1, sum)

### NOTE that loggings here equals drain - natural mortality
#    sum of loggins matches with data provided by Y.Sevola (2.7.2009 Aleksi Lehtonen)
##   sum of loggins matches with data provided by Y.Sevola (18.8.2010 Aleksi Lehtonen)



#############
###   Check the sum of loggings, if it is okay / or not ???


################################
#  BEFs for loggins
################################

BEF.log <-  read.table(paste(path.bef,"loggbef.dat", sep=""),col.names=c('region','soil','species','compartment','bef'))
BEF.log2 <- subset(BEF.log, soil<3 & region<3)
BEF.log2 <- BEF.log2[order(BEF.log2$species,BEF.log2$soil,BEF.log2$region),]

### BEF.logg[order(BEF.logg$species,BEF.logg$soil,BEF.logg$region),]

## from SAS code
#    bm=ru;bosite=1;output;
#    bm=kuo;bosite=2;output;
#    bm=ran;bosite=3;output; oksat  / branches
#    bm=fol;bosite=4;output;
#    bm=ran5;bosite=5;output; kuolleet oksat / dead branches
#    bm=kanto;bosite=6;output;
#    bm=juuret;bosite=7;output;
#    bm=totabove+totbelow;bosite=8;output;

BEF.ste <- subset(BEF.log2, compartment==1)
BEF.bar <- subset(BEF.log2, compartment==2)
BEF.br <- subset(BEF.log2, compartment==3)
BEF.fl <- subset(BEF.log2, compartment==4)
BEF.stu <- subset(BEF.log2, compartment==6)
BEF.roo <- subset(BEF.log2, compartment==7)

loggings <- rbind(pine.loggings.SF.min,pine.loggings.NF.min,pine.loggings.SF.org,pine.loggings.NF.org,spruce.loggings.SF.min,spruce.loggings.NF.min,spruce.loggings.SF.org,spruce.loggings.NF.org, birch.loggings.SF.min,birch.loggings.NF.min,birch.loggings.SF.org,birch.loggings.NF.org)

loggings.bm <- rbind(pine.loggings.SF.min.bm,pine.loggings.NF.min.bm,pine.loggings.SF.org.bm,pine.loggings.NF.org.bm,spruce.loggings.SF.min.bm,spruce.loggings.NF.min.bm,spruce.loggings.SF.org.bm,spruce.loggings.NF.org.bm, birch.loggings.SF.min.bm,birch.loggings.NF.min.bm,birch.loggings.SF.org.bm,birch.loggings.NF.org.bm)

##################################################
#wastewood ratios
##################################################


ww <- 0.09 ## based on the recent stats (Yrjo Sevola) #Used before 2004

#Here new wastewood ratios, used 2004 ->
#Based on nfi10 (Antti Ihalainen, 19.8.2011)
ww.sp.SF <- 0.056
ww.ns.SF <- 0.053	
ww.b.SF <- 0.319	

ww.sp.NF <- 0.062
ww.ns.NF <- 0.064
ww.b.NF <- 0.309

#gather up all ww ratios in right order
ww.all <- c(ww.sp.SF, ww.sp.NF, ww.sp.SF, ww.sp.NF, ww.ns.SF, ww.ns.NF, ww.ns.SF, ww.ns.NF, ww.b.SF, ww.b.NF, ww.b.SF, ww.b.NF)


############################################################
### Calculate litter files from for loggings 1970 ->
############################################################
### output is millions of tons of biomass
### NFI6 -> 1976 NFI7 -> 1986 NFI8 -> 1994 NFI -> 2003

### BEFs are applied
loggings.branches <- loggings*BEF.br$bef
loggings.foliage <- loggings*BEF.fl$bef
loggings.stump <- loggings*BEF.stu$bef
loggings.stem <- ww*loggings*(BEF.ste$bef+BEF.bar$bef)
#2004 ->
loggings.stem[,35:dim(loggings)[2]] <- ww.all * loggings[ ,35:dim(loggings)[2] ] *(BEF.ste$bef+BEF.bar$bef)
###NOTE: Include the whole stem for biomass drain #########
loggings.stem.juha <- loggings*(BEF.ste$bef+BEF.bar$bef)

loggings.roots <-loggings*BEF.roo$bef

####           plot((seq(1,39, by=1)), loggings.branches[12,], type="l")

### fineroot BEFs are from NFI7
BEF.nfi7.SF <-  read.csv(paste(path.nfi7.bef,"Bef.SF.csv", sep=""), header=TRUE, as.is=TRUE)
BEF.nfi7.NF <-  read.csv(paste(path.nfi7.bef,"Bef.NF.csv", sep=""), header=TRUE, as.is=TRUE)
BEF.nfi7.SF$alue=1
BEF.nfi7.NF$alue=2

BEF.fr <- subset(rbind(BEF.nfi7.SF,BEF.nfi7.NF), select=c(alar,ojitus,plaji,alue,BEF.fr))
BEF.fr <- BEF.fr[order(BEF.fr$plaji,BEF.fr$alar,BEF.fr$alue),]

loggings.fineroot <- loggings*BEF.fr$BEF.fr


#####################################################
###  BIOMASS of above- and below-ground
#####################################################

BEF.logg.above=BEF.log[BEF.log$region<3 & BEF.log$soil<3 & BEF.log$compartment==8,] #above
BEF.logg.below=BEF.log[BEF.log$region<3 & BEF.log$soil<3 & BEF.log$compartment==9,] #below

BEF.logg.above=BEF.logg.above[order(BEF.logg.above$species,BEF.logg.above$soil,BEF.logg.above$region),]
BEF.logg.below=BEF.logg.below[order(BEF.logg.below$species,BEF.logg.below$soil,BEF.logg.below$region),]

logg.above=BEF.logg.above$bef*loggings.bm[,21:dim(loggings.bm)[2]] # 1990-yr species,soil,region

logg.below=BEF.logg.below$bef*loggings.bm[,21:dim(loggings.bm)[2]] # 1990-yr species,soil,region



##################################################
# Remove the energywood from logging residues & remove loggings on undrained org soils
##################################################
#(NOTE divison to  mineral soils & org soils)

loggings.stump.origi <- loggings.stump
loggings.branches.origi <- loggings.branches
loggings.stem.origi <- loggings.stem

#Tree species ratios for stem harvest
#Based on energy wood tree species

sp.rat.sf <- drain.energy.species$SF.pine
sp.rat.nf <- drain.energy.species$NF.pine

ns.rat.sf <- drain.energy.species$SF.spruce
ns.rat.nf <- drain.energy.species$NF.spruce

bl.rat.sf <- drain.energy.species$SF.broadl
bl.rat.nf <- drain.energy.species$NF.broadl


sp.sf <- c(rep((1-NS.rat),20), sp.rat.sf )
sp.nf <- c(rep((1-NS.rat),20), sp.rat.nf )

ns.sf <- c(rep((NS.rat),20), ns.rat.sf )
ns.nf <- c(rep((NS.rat),20), ns.rat.nf )

bl.sf <- c(rep(0,20), bl.rat.sf)
bl.nf <- c(rep(0,20), bl.rat.nf)


# stumps

loggings.stump[1,] <- loggings.stump[1,] *pine.undrained.logg.SF
loggings.stump[2,] <- loggings.stump[2,] *pine.undrained.logg.NF
loggings.stump[3,] <- loggings.stump[3,] *pine.undrained.logg.SF
loggings.stump[4,] <- loggings.stump[4,] *pine.undrained.logg.NF

loggings.stump[5,] <- (loggings.stump[5,] - (stumps.SF*spruce.min.logg.SF))*spruce.undrained.logg.SF
loggings.stump[6,] <- (loggings.stump[6,] - (stumps.NF*spruce.min.logg.NF))*spruce.undrained.logg.NF
loggings.stump[7,] <- (loggings.stump[7,] - stumps.SF*(1-spruce.min.logg.SF))*spruce.undrained.logg.SF
loggings.stump[8,] <- (loggings.stump[8,] - stumps.NF*(1-spruce.min.logg.NF))*spruce.undrained.logg.NF

loggings.stump[9,] <- loggings.stump[9,] *birch.undrained.logg.SF
loggings.stump[10,] <- loggings.stump[10,] *birch.undrained.logg.NF
loggings.stump[11,] <- loggings.stump[11,] *birch.undrained.logg.SF
loggings.stump[12,] <- loggings.stump[12,] *birch.undrained.logg.NF
#harvest slash

loggings.branches[1,] <- (loggings.branches[1,] - harves.SF.SP*pine.min.logg.SF)*pine.undrained.logg.SF
loggings.branches[2,] <- (loggings.branches[2,] - harves.NF.SP*pine.min.logg.NF)*pine.undrained.logg.NF
loggings.branches[3,] <- (loggings.branches[3,] - harves.SF.SP*(1-pine.min.logg.SF))*pine.undrained.logg.SF
loggings.branches[4,] <- (loggings.branches[4,] - harves.NF.SP*(1-pine.min.logg.NF))*pine.undrained.logg.NF

loggings.branches[5,] <- (loggings.branches[5,] - harves.SF.NS*spruce.min.logg.SF)*spruce.undrained.logg.SF
loggings.branches[6,] <- (loggings.branches[6,] - harves.NF.NS*spruce.min.logg.NF)*spruce.undrained.logg.NF
loggings.branches[7,] <- (loggings.branches[7,] - harves.SF.NS*(1-spruce.min.logg.SF))*spruce.undrained.logg.SF
loggings.branches[8,] <- (loggings.branches[8,] - harves.NF.NS*(1-spruce.min.logg.NF))*spruce.undrained.logg.NF

loggings.branches[9,] <- loggings.branches[9,]*birch.undrained.logg.SF
loggings.branches[10,] <- loggings.branches[10,] *birch.undrained.logg.NF
loggings.branches[11,] <- loggings.branches[11,] *birch.undrained.logg.SF
loggings.branches[12,] <- loggings.branches[12,] *birch.undrained.logg.NF


# wastewood

loggings.stem[1,] <- (loggings.stem[1,] - wastew.SF*(sp.sf)*pine.min.logg.SF)*pine.undrained.logg.SF
loggings.stem[2,] <- (loggings.stem[2,] - wastew.NF*(sp.nf)*pine.min.logg.NF)*pine.undrained.logg.NF
loggings.stem[3,] <- (loggings.stem[3,] - wastew.SF*(sp.sf)*(1-pine.min.logg.SF))*pine.undrained.logg.SF
loggings.stem[4,] <- (loggings.stem[4,] - wastew.NF*(sp.nf)*(1-pine.min.logg.NF))*pine.undrained.logg.NF

loggings.stem[5,] <- (loggings.stem[5,] - wastew.SF*(ns.sf)*spruce.min.logg.SF)*spruce.undrained.logg.SF
loggings.stem[6,] <- (loggings.stem[6,] - wastew.NF*(ns.nf)*spruce.min.logg.NF)*spruce.undrained.logg.NF
loggings.stem[7,] <- (loggings.stem[7,] - wastew.SF*(ns.sf)*(1-spruce.min.logg.SF))*spruce.undrained.logg.SF
loggings.stem[8,] <- (loggings.stem[8,] - wastew.NF*(ns.nf)*(1-spruce.min.logg.NF)) *spruce.undrained.logg.NF        

loggings.stem[9,] <- (loggings.stem[9,] - wastew.SF*(bl.sf)*birch.min.logg.SF)*birch.undrained.logg.SF
loggings.stem[10,] <- (loggings.stem[10,] - wastew.NF*(bl.nf)*birch.min.logg.NF)*birch.undrained.logg.NF
loggings.stem[11,] <- (loggings.stem[11,] - wastew.SF*(bl.sf)*(1-birch.min.logg.SF))*birch.undrained.logg.SF
loggings.stem[12,] <- (loggings.stem[12,] - wastew.NF*(bl.nf)*(1-birch.min.logg.NF)) *birch.undrained.logg.NF        



### CONVERSION TO CARBON HAS BEEN DONE HERE

### 1. SF mineral soils
log.branches.SF.min <- branches.AWEN(loggings.branches[1,])+ branches.AWEN(loggings.branches[5,])+ branches.AWEN(loggings.branches[9,])

log.foliage.SF.min <- foliage.AWEN(loggings.foliage[1,],1)+ foliage.AWEN(loggings.foliage[5,],2)+ foliage.AWEN(loggings.foliage[9,],3)

log.stump.SF.min <- stem.AWEN(loggings.stump[1,],1)+ stem.AWEN(loggings.stump[5,],2)+ stem.AWEN(loggings.stump[9,],3)

log.roots.SF.min <- stem.AWEN(loggings.roots[1,],1)+ stem.AWEN(loggings.roots[5,],2)+ stem.AWEN(loggings.roots[9,],3)

log.fineroot.SF.min <- fineroot.AWEN(loggings.fineroot[1,],1)+ fineroot.AWEN(loggings.fineroot[5,],2)+ fineroot.AWEN(loggings.fineroot[9,],3)

log.stem.SF.min <- stem.AWEN(loggings.stem[1,],1)+ stem.AWEN(loggings.stem[5,],2)+ stem.AWEN(loggings.stem[9,],3)

log.nwl.SF.min <- carbon((log.foliage.SF.min+log.fineroot.SF.min))
log.fwl.SF.min <- carbon((log.branches.SF.min+log.roots.SF.min))
log.cwl.SF.min <- carbon((log.stump.SF.min+log.stem.SF.min))

### 2. NF mineral soils
log.branches.NF.min <- branches.AWEN(loggings.branches[2,])+ branches.AWEN(loggings.branches[6,])+ branches.AWEN(loggings.branches[10,])

log.foliage.NF.min <- foliage.AWEN(loggings.foliage[2,],1)+ foliage.AWEN(loggings.foliage[6,],2)+ foliage.AWEN(loggings.foliage[10,],3)

log.stump.NF.min <- stem.AWEN(loggings.stump[2,],1)+ stem.AWEN(loggings.stump[6,],2)+ stem.AWEN(loggings.stump[10,],3)

log.roots.NF.min <- stem.AWEN(loggings.roots[2,],1)+ stem.AWEN(loggings.roots[6,],2)+ stem.AWEN(loggings.roots[10,],3)

log.fineroot.NF.min <- fineroot.AWEN(loggings.fineroot[2,],1)+ fineroot.AWEN(loggings.fineroot[6,],2)+ fineroot.AWEN(loggings.fineroot[10,],3)

log.stem.NF.min <- stem.AWEN(loggings.stem[2,],1)+ stem.AWEN(loggings.stem[6,],2)+ stem.AWEN(loggings.stem[10,],3)

log.nwl.NF.min <- carbon((log.foliage.NF.min+log.fineroot.NF.min))
log.fwl.NF.min <- carbon((log.branches.NF.min+log.roots.NF.min))
log.cwl.NF.min <- carbon((log.stump.NF.min+log.stem.NF.min))


### 3. SF organic soils
log.branches.SF.org <- branches.AWEN(loggings.branches[3,])+ branches.AWEN(loggings.branches[7,])+ branches.AWEN(loggings.branches[11,])

log.foliage.SF.org <- foliage.AWEN(loggings.foliage[3,],1)+ foliage.AWEN(loggings.foliage[7,],2)+ foliage.AWEN(loggings.foliage[11,],3)

log.stump.SF.org <- stem.AWEN(loggings.stump[3,],1)+ stem.AWEN(loggings.stump[7,],2)+ stem.AWEN(loggings.stump[11,],3)

log.roots.SF.org <- stem.AWEN(loggings.roots[3,],1)+ stem.AWEN(loggings.roots[7,],2)+ stem.AWEN(loggings.roots[11,],3)

log.fineroot.SF.org <- fineroot.AWEN(loggings.fineroot[3,],1)+ fineroot.AWEN(loggings.fineroot[7,],2)+ fineroot.AWEN(loggings.fineroot[11,],3)

log.stem.SF.org <- stem.AWEN(loggings.stem[3,],1)+ stem.AWEN(loggings.stem[7,],2)+ stem.AWEN(loggings.stem[11,],3)

log.nwl.SF.org.abv <- carbon((log.foliage.SF.org))
log.nwl.SF.org.bel <- carbon((log.fineroot.SF.org))

log.fwl.SF.org.abv <- carbon((log.branches.SF.org))
log.fwl.SF.org.bel <- carbon((log.roots.SF.org))

log.cwl.SF.org.abv <- carbon((log.stump.SF.org+log.stem.SF.org))


### 4. NF organic soils
log.branches.NF.org <- branches.AWEN(loggings.branches[4,])+ branches.AWEN(loggings.branches[8,])+ branches.AWEN(loggings.branches[12,])

log.foliage.NF.org <- foliage.AWEN(loggings.foliage[4,],1)+ foliage.AWEN(loggings.foliage[8,],2)+ foliage.AWEN(loggings.foliage[12,],3)

log.stump.NF.org <- stem.AWEN(loggings.stump[4,],1)+ stem.AWEN(loggings.stump[8,],2)+ stem.AWEN(loggings.stump[12,],3)

log.roots.NF.org <- stem.AWEN(loggings.roots[4,],1)+ stem.AWEN(loggings.roots[8,],2)+ stem.AWEN(loggings.roots[12,],3)

log.fineroot.NF.org <- fineroot.AWEN(loggings.fineroot[4,],1)+ fineroot.AWEN(loggings.fineroot[8,],2)+ fineroot.AWEN(loggings.fineroot[12,],3)

log.stem.NF.org <- stem.AWEN(loggings.stem[4,],1)+ stem.AWEN(loggings.stem[8,],2)+ stem.AWEN(loggings.stem[12,],3)


log.nwl.NF.org.abv <- carbon((log.foliage.NF.org))
log.nwl.NF.org.bel <- carbon((log.fineroot.NF.org))

log.fwl.NF.org.abv <- carbon((log.branches.NF.org))
log.fwl.NF.org.bel <- carbon((log.roots.NF.org))

log.cwl.NF.org.abv <- carbon((log.stump.NF.org+log.stem.NF.org))


############################################
#  AREA DATA
############################################
#### Here a piece to get estimates per area unit, source AFF 220 and sas run

# 1. FAO mineral soils, 2. FAO organic soils
nfi7.area.sf <- c(88023.08+1719.51, 26877.44+4547.31)*100 #  here SF, unit (ha)
nfi7.area.sf.org <- nfi7.area.sf
nfi7.area.sf[2] <- nfi7.area.sf[2]*0.707  # drained 70.7 % from total AFF 220 p.11
nfi7.area.2 <- c(20394.75+54.88, 12247.51+4998.73)*100 #  here reg 2, unit (ha)
nfi7.area.3 <- c(14491.39+634.50, 2757.92+3899.54)*100 #  here reg 3, unit (ha)
nfi7.area.4 <- c(22767.73+722.57, 5717.65+6879.58)*100 #  here reg 4, unit (ha)
nfi7.area.5 <- c(7357.81 +6811.06, 13.21+1301.85)*100 #  here reg 5, unit (ha)

nfi7.area.nf <- colSums(rbind(nfi7.area.2,nfi7.area.3,nfi7.area.4,nfi7.area.5))
nfi7.area.nf.org <- nfi7.area.nf
nfi7.area.nf[2] <- nfi7.area.nf[2]*0.365  # drained 36.5 % from total AFF 220 p.11
# this has been checked 1.10.2009

nfi6.area.sf <- c(9056, (((11312+910)-9056)*0.639))*1000  # based on the CIFF 93.6, 63,9% drained!!!
nfi6.area.sf.org <- c(9056, (((11312+910)-9056)))*1000  #

# Remove undrained areas from data, DONE 5.10.2009


#Need areas of mineral and drained:
areas <- read.table(paste(path.areas.lulucf,"lulucf_rem_kptyy.txt", sep=""), header=TRUE, as.is=TRUE)

areas.sf.min <- t(subset(areas, region==1 & ipcc==1 & soil==1))
areas.nf.min <- t(subset(areas, region==2 & ipcc==1 & soil==1))
areas.sf.dra <- t(subset(areas, region==1 & ipcc==1 & soil==2 & kptyy==16))
areas.nf.dra <- t(subset(areas, region==2 & ipcc==1 & soil==2 & kptyy==16))
areas.sf.und <- t(subset(areas, region==1 & ipcc==1 & soil==2 & kptyy==0))
areas.nf.und <- t(subset(areas, region==2 & ipcc==1 & soil==2 & kptyy==0))

row.names(areas.sf.min) <- NULL
row.names(areas.nf.min) <- NULL
row.names(areas.sf.dra) <- NULL
row.names(areas.nf.dra) <- NULL
row.names(areas.sf.und) <- NULL
row.names(areas.nf.und) <- NULL


# Here interpolating land areas, separate for SF/NF, min / org.

years <- seq(1972, yr, by=1)
sf.min <- c(nfi6.area.sf[1], rep(NA,6), nfi7.area.sf[1], rep(NA,10), areas.sf.min[-(1:4)]) ## SF NFI6 1972, NFI7 1979
sf.org <- c(nfi6.area.sf[2], rep(NA,6), nfi7.area.sf[2], rep(NA,10), areas.sf.dra[-(1:4)])
sf.org.tot <- c(nfi6.area.sf.org[2], rep(NA,6), nfi7.area.sf.org[2], rep(NA,10), colSums(rbind(areas.sf.dra[-(1:4)],areas.sf.und[-(1:4)])))

nf.min <- c(sum(nfi7.area.nf[1]), rep(NA,6), areas.nf.min[-(1:4)]) ## NF NFI7 1983
nf.org <- c(sum(nfi7.area.nf[2]), rep(NA,6), areas.nf.dra[-(1:4)]) ## NF NFI7 1983
nf.org.tot <- c(sum(nfi7.area.nf.org[2]), rep(NA,6), colSums(rbind(areas.nf.dra[-(1:4)],areas.nf.und[-(1:4)]))) ## NF NFI7 1983

index <- seq(along=sf.min)
sf.min <- approx(index,sf.min,index)$y   # here interpolating the time series
sf.org <- approx(index,sf.org,index)$y
sf.org.tot <- approx(index,sf.org.tot,index)$y 

index <- seq(along=nf.min)
nf.min <- approx(index,nf.min,index)$y   # here interpolating the time series
nf.org <- approx(index,nf.org,index)$y   # here interpolating the time series
nf.org.tot <- approx(index,nf.org.tot,index)$y

## par(mfrow=c(2,2))
##   plot( years[12:37], nf.org, ylim=c(0,11200000))
##   plot( years, sf.org, ylim=c(0,11200000))

# Here estimating the input to soil per land area !!! UNIT tons C per ha (unit above mill ton of C )
# note, loggings time series starts from 1970, for 70 & 71 the area of 1972 was used 
sf.min7071 <- sf.min[1]
sf.min.area <- c(sf.min7071,sf.min7071,sf.min)

#Lasketaan hakkuun karike per hehtaari
log.nwl.SF.min.ha <- (log.nwl.SF.min*1000000)/sf.min.area  #tons c ha
log.fwl.SF.min.ha <- (log.fwl.SF.min*1000000)/sf.min.area
log.cwl.SF.min.ha <- (log.cwl.SF.min*1000000)/sf.min.area

sf.org7071 <- sf.org[1]
sf.org.area <- c(sf.org7071,sf.org7071,sf.org)

sf.org.tot7071 <- sf.org.tot[1]
sf.org.tot.area <- c(sf.org.tot7071,sf.org.tot7071,sf.org.tot)   #

log.nwl.SF.org.abv.ha <- (log.nwl.SF.org.abv*1000000)/sf.org.area  #tons c ha
log.nwl.SF.org.bel.ha <- (log.nwl.SF.org.bel*1000000)/sf.org.area
log.fwl.SF.org.abv.ha <- (log.fwl.SF.org.abv*1000000)/sf.org.area  #tons c ha
log.fwl.SF.org.bel.ha <- (log.fwl.SF.org.bel*1000000)/sf.org.area
log.cwl.SF.org.abv.ha <- (log.cwl.SF.org.abv*1000000)/sf.org.area


nf.min82 <- nf.min[1]
nf.min.area <- c(nf.min82,nf.min)
nf.min.area.whole <- c(rep(nf.min82,7),nf.min.area)

log.nwl.NF.min.ha.whole <- (log.nwl.NF.min[6:dim(log.nwl.NF.min)[1],]*1000000)/nf.min.area.whole  #tons c ha
log.fwl.NF.min.ha.whole <- (log.fwl.NF.min[6:dim(log.fwl.NF.min)[1],]*1000000)/nf.min.area.whole
log.cwl.NF.min.ha.whole <- (log.cwl.NF.min[6:dim(log.cwl.NF.min)[1],]*1000000)/nf.min.area.whole

nf.org82 <- nf.org[1]
nf.org.area <- c(nf.org82,nf.org)

nf.org.tot82 <- nf.org.tot[1]
nf.org.tot.area <- c(nf.org.tot82,nf.org.tot)

log.nwl.NF.org.abv.ha <- (log.nwl.NF.org.abv[13:dim(log.nwl.NF.org.abv)[1],]*1000000)/nf.org.area  #tons c ha
log.nwl.NF.org.bel.ha <- (log.nwl.NF.org.bel[13:dim(log.nwl.NF.org.bel)[1],]*1000000)/nf.org.area
log.fwl.NF.org.abv.ha <- (log.fwl.NF.org.abv[13:dim(log.fwl.NF.org.abv)[1],]*1000000)/nf.org.area  #tons c ha
log.fwl.NF.org.bel.ha <- (log.fwl.NF.org.bel[13:dim(log.fwl.NF.org.bel)[1],]*1000000)/nf.org.area
log.cwl.NF.org.abv.ha <- (log.cwl.NF.org.abv[13:dim(log.cwl.NF.org.abv)[1],]*1000000)/nf.org.area

#################
#########################################
# WRITING Yasso Inputs to file, nwl, fwl, cwl for south and for north, also above-/belowground


write.csv(log.nwl.SF.min.ha, (paste(path.litter,"log.nwl.SF.min.csv", sep="")), row.names = FALSE)
write.csv(log.fwl.SF.min.ha, (paste(path.litter,"log.fwl.SF.min.csv", sep="")), row.names = FALSE)
write.csv(log.cwl.SF.min.ha, (paste(path.litter,"log.cwl.SF.min.csv", sep="")), row.names = FALSE)

write.csv(log.nwl.SF.org.abv.ha, (paste(path.litter,"log.nwl.SF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(log.nwl.SF.org.bel.ha, (paste(path.litter,"log.nwl.SF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(log.fwl.SF.org.abv.ha, (paste(path.litter,"log.fwl.SF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(log.fwl.SF.org.bel.ha, (paste(path.litter,"log.fwl.SF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(log.cwl.SF.org.abv.ha, (paste(path.litter,"log.cwl.SF.org.abv.csv", sep="")), row.names = FALSE)

write.csv(log.nwl.NF.min.ha.whole, (paste(path.litter,"log.nwl.NF.min.csv", sep="")), row.names = FALSE)
write.csv(log.fwl.NF.min.ha.whole, (paste(path.litter,"log.fwl.NF.min.csv", sep="")), row.names = FALSE)
write.csv(log.cwl.NF.min.ha.whole, (paste(path.litter,"log.cwl.NF.min.csv", sep="")), row.names = FALSE)

write.csv(log.nwl.NF.org.abv.ha, (paste(path.litter,"log.nwl.NF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(log.nwl.NF.org.bel.ha, (paste(path.litter,"log.nwl.NF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(log.fwl.NF.org.abv.ha, (paste(path.litter,"log.fwl.NF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(log.fwl.NF.org.bel.ha, (paste(path.litter,"log.fwl.NF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(log.cwl.NF.org.abv.ha, (paste(path.litter,"log.cwl.NF.org.abv.csv", sep="")), row.names = FALSE)


###############################################
## NATURAL MORTALITY
###############################################


# here making estimates of Yasso input from natural mortality

### HERE making litter files from for natural mortality 1970 ->
### output is millions of tons of biomass


# 1. modifying data to be more easily used


natmort <- rbind(sf.nat.mort[,1],nf.nat.mort[,1],sf.nat.mort[,4],nf.nat.mort[,4],sf.nat.mort[,2],nf.nat.mort[,2],sf.nat.mort[,5],nf.nat.mort[,5],sf.nat.mort[,3],nf.nat.mort[,3],sf.nat.mort[,6],nf.nat.mort[,6])

#### HERE MAKING BEFs for Natural mortality

#  BEFs for natmort
BEF.nat <-  read.table(paste(path.bef,"nmortbef.dat", sep=""),col.names=c('region','soil','species','compartment','bef'))

BEF.nat2 <- subset(BEF.nat, soil<3 & region<3)
BEF.nat2 <- BEF.nat2[order(BEF.nat2$species,BEF.nat2$soil,BEF.nat2$region),]

### BEF.natg[order(BEF.natg$species,BEF.natg$soil,BEF.natg$region),]

## from SAS code
#    bm=ru;bosite=1;output;
#    bm=kuo;bosite=2;output;
#    bm=ran;bosite=3;output; oksat  / branches
#    bm=fol;bosite=4;output;
#    bm=ran5;bosite=5;output; kuolleet oksat / dead branches
#    bm=kanto;bosite=6;output;
#    bm=juuret;bosite=7;output;
#    bm=totabove+totbelow;bosite=8;output;

BEF.ste.nat <- subset(BEF.nat2, compartment==1)
BEF.bar.nat <- subset(BEF.nat2, compartment==2)
BEF.br.nat <- subset(BEF.nat2, compartment==3)
BEF.fl.nat <- subset(BEF.nat2, compartment==4)
BEF.stu.nat <- subset(BEF.nat2, compartment==6)
BEF.roo.nat <- subset(BEF.nat2, compartment==7)
# for fineroots using NFI7 ratios
BEF.fr.nat <- BEF.fr


natmort.branches <- natmort*BEF.br.nat$bef

natmort.foliage <-natmort*BEF.fl.nat$bef

# note natural mortality of all stems included
natmort.stump <-natmort*BEF.stu.nat$bef

natmort.stem <- natmort*(BEF.ste.nat$bef+BEF.bar.nat$bef)

natmort.roots <-natmort*BEF.roo.nat$bef

## Note, natural mortality includes also stem & bark litter
natmort.fineroot <-natmort*BEF.fr.nat$BEF.fr

#####################################################
##  Above- and below-ground biomass
#####################################################

BEF.nmort.above=BEF.nat[BEF.nat$region<3 & BEF.nat$soil<3 & BEF.nat$compartment==8,]
BEF.nmort.below=BEF.nat[BEF.nat$region<3 & BEF.nat$soil<3 & BEF.nat$compartment==9,]

BEF.nmort.above=BEF.nmort.above[order(BEF.nmort.above$species,BEF.nmort.above$soil,BEF.nmort.above$region),]
BEF.nmort.below=BEF.nmort.below[order(BEF.nmort.below$species,BEF.nmort.below$soil,BEF.nmort.below$region),]

nmort.above=BEF.nmort.above$bef*natmort[,21:dim(natmort)[2]] # species,soil,region
nmort.below=BEF.nmort.below$bef*natmort[,21:dim(natmort)[2]] # species,soil,region



### 1. SF mineral soils
nat.branches.SF.min <- branches.AWEN(natmort.branches[1,])+ branches.AWEN(natmort.branches[5,])+ branches.AWEN(natmort.branches[9,])

nat.foliage.SF.min <- foliage.AWEN(natmort.foliage[1,],1)+ foliage.AWEN(natmort.foliage[5,],2)+ foliage.AWEN(natmort.foliage[9,],3)

nat.stump.SF.min <- stem.AWEN(natmort.stump[1,],1)+ stem.AWEN(natmort.stump[5,],2)+ stem.AWEN(natmort.stump[9,],3)

nat.roots.SF.min <- stem.AWEN(natmort.roots[1,],1)+ stem.AWEN(natmort.roots[5,],2)+ stem.AWEN(natmort.roots[9,],3)

nat.fineroot.SF.min <- fineroot.AWEN(natmort.fineroot[1,],1)+ fineroot.AWEN(natmort.fineroot[5,],2)+ fineroot.AWEN(natmort.fineroot[9,],3)

nat.stem.SF.min <- stem.AWEN(natmort.stem[1,],1)+ stem.AWEN(natmort.stem[5,],2)+ stem.AWEN(natmort.stem[9,],3)


nat.nwl.SF.min <- carbon((nat.foliage.SF.min+nat.fineroot.SF.min))
nat.fwl.SF.min <- carbon((nat.branches.SF.min+nat.roots.SF.min))
nat.cwl.SF.min <- carbon((nat.stump.SF.min+nat.stem.SF.min))


### 2. NF mineral soils
nat.branches.NF.min <- branches.AWEN(natmort.branches[2,])+ branches.AWEN(natmort.branches[6,])+ branches.AWEN(natmort.branches[10,])

nat.foliage.NF.min <- foliage.AWEN(natmort.foliage[2,],1)+ foliage.AWEN(natmort.foliage[6,],2)+ foliage.AWEN(natmort.foliage[10,],3)

nat.stump.NF.min <- stem.AWEN(natmort.stump[2,],1)+ stem.AWEN(natmort.stump[6,],2)+ stem.AWEN(natmort.stump[10,],3)

nat.roots.NF.min <- stem.AWEN(natmort.roots[2,],1)+ stem.AWEN(natmort.roots[6,],2)+ stem.AWEN(natmort.roots[10,],3)

nat.fineroot.NF.min <- fineroot.AWEN(natmort.fineroot[2,],1)+ fineroot.AWEN(natmort.fineroot[6,],2)+ fineroot.AWEN(natmort.fineroot[10,],3)

nat.stem.NF.min <- stem.AWEN(natmort.stem[2,],1)+ stem.AWEN(natmort.stem[6,],2)+ stem.AWEN(natmort.stem[10,],3)

nat.nwl.NF.min <- carbon((nat.foliage.NF.min+nat.fineroot.NF.min))
nat.fwl.NF.min <- carbon((nat.branches.NF.min+nat.roots.NF.min))
nat.cwl.NF.min <- carbon((nat.stump.NF.min+nat.stem.NF.min))


### 3. SF organic soils
nat.branches.SF.org <- branches.AWEN(natmort.branches[3,])+ branches.AWEN(natmort.branches[7,])+ branches.AWEN(natmort.branches[11,])

nat.foliage.SF.org <- foliage.AWEN(natmort.foliage[3,],1)+ foliage.AWEN(natmort.foliage[7,],2)+ foliage.AWEN(natmort.foliage[11,],3)

nat.stump.SF.org <- stem.AWEN(natmort.stump[3,],1)+ stem.AWEN(natmort.stump[7,],2)+ stem.AWEN(natmort.stump[11,],3)

nat.roots.SF.org <- stem.AWEN(natmort.roots[3,],1)+ stem.AWEN(natmort.roots[7,],2)+ stem.AWEN(natmort.roots[11,],3)

nat.fineroot.SF.org <- fineroot.AWEN(natmort.fineroot[3,],1)+ fineroot.AWEN(natmort.fineroot[7,],2)+ fineroot.AWEN(natmort.fineroot[11,],3)

nat.stem.SF.org <- stem.AWEN(natmort.stem[3,],1)+ stem.AWEN(natmort.stem[7,],2)+ stem.AWEN(natmort.stem[11,],3)

nat.nwl.SF.org.abv <- carbon((nat.foliage.SF.org))
nat.fwl.SF.org.bel <- carbon((nat.roots.SF.org))
nat.nwl.SF.org.bel <- carbon((nat.fineroot.SF.org))
nat.fwl.SF.org.abv <- carbon((nat.branches.SF.org))
nat.cwl.SF.org.abv <- carbon((nat.stump.SF.org+nat.stem.SF.org))


### 4. NF organic soils
nat.branches.NF.org <- branches.AWEN(natmort.branches[4,])+ branches.AWEN(natmort.branches[8,])+ branches.AWEN(natmort.branches[12,])

nat.foliage.NF.org <- foliage.AWEN(natmort.foliage[4,],1)+ foliage.AWEN(natmort.foliage[8,],2)+ foliage.AWEN(natmort.foliage[12,],3)

nat.stump.NF.org <- stem.AWEN(natmort.stump[4,],1)+ stem.AWEN(natmort.stump[8,],2)+ stem.AWEN(natmort.stump[12,],3)

nat.roots.NF.org <- stem.AWEN(natmort.roots[4,],1)+ stem.AWEN(natmort.roots[8,],2)+ stem.AWEN(natmort.roots[12,],3)

nat.fineroot.NF.org <- fineroot.AWEN(natmort.fineroot[4,],1)+ fineroot.AWEN(natmort.fineroot[8,],2)+ fineroot.AWEN(natmort.fineroot[12,],3)

nat.stem.NF.org <- stem.AWEN(natmort.stem[4,],1)+ stem.AWEN(natmort.stem[8,],2)+ stem.AWEN(natmort.stem[12,],3)

nat.nwl.NF.org.abv <- carbon((nat.foliage.NF.org))
nat.fwl.NF.org.bel <- carbon((nat.roots.NF.org))
nat.nwl.NF.org.bel <- carbon((nat.fineroot.NF.org))
nat.fwl.NF.org.abv <- carbon((nat.branches.NF.org))
nat.cwl.NF.org.abv <- carbon((nat.stump.NF.org+nat.stem.NF.org))


############################################################
## Natural mortality per area unit (ton C per ha), 
############################################################
#note that area for organic soils includes also undrained org. soils


nat.nwl.SF.min.ha <- (nat.nwl.SF.min*1000000)/sf.min.area  #tons c ha
nat.fwl.SF.min.ha <- (nat.fwl.SF.min*1000000)/sf.min.area
nat.cwl.SF.min.ha <- (nat.cwl.SF.min*1000000)/sf.min.area

# nat.nwl.NF.min.ha <- (nat.nwl.NF.min[13:40,]*1000000)/nf.min.area  #tons c ha
# nat.fwl.NF.min.ha <- (nat.fwl.NF.min[13:40,]*1000000)/nf.min.area
# nat.cwl.NF.min.ha <- (nat.cwl.NF.min[13:40,]*1000000)/nf.min.area


# Here time series since NFI6
nat.nwl.NF.min.ha.whole <- (nat.nwl.NF.min[6:dim(nat.nwl.NF.min)[1],]*1000000)/nf.min.area.whole  #tons c ha
nat.fwl.NF.min.ha.whole <- (nat.fwl.NF.min[6:dim(nat.fwl.NF.min)[1],]*1000000)/nf.min.area.whole
nat.cwl.NF.min.ha.whole <- (nat.cwl.NF.min[6:dim(nat.cwl.NF.min)[1],]*1000000)/nf.min.area.whole

nat.nwl.SF.org.abv.ha <- (nat.nwl.SF.org.abv*1000000)/sf.org.tot.area  #tons c ha
nat.fwl.SF.org.abv.ha <- (nat.fwl.SF.org.abv*1000000)/sf.org.tot.area
nat.nwl.SF.org.bel.ha <- (nat.nwl.SF.org.bel*1000000)/sf.org.tot.area  #tons c ha
nat.fwl.SF.org.bel.ha <- (nat.fwl.SF.org.bel*1000000)/sf.org.tot.area
nat.cwl.SF.org.abv.ha <- (nat.cwl.SF.org.abv*1000000)/sf.org.tot.area

nat.nwl.NF.org.abv.ha <- (nat.nwl.NF.org.abv[13:dim(nat.nwl.NF.org.abv)[1],]*1000000)/nf.org.tot.area  #tons c ha
nat.fwl.NF.org.abv.ha <- (nat.fwl.NF.org.abv[13:dim(nat.fwl.NF.org.abv)[1],]*1000000)/nf.org.tot.area
nat.nwl.NF.org.bel.ha <- (nat.nwl.NF.org.bel[13:dim(nat.nwl.NF.org.bel)[1],]*1000000)/nf.org.tot.area  #tons c ha
nat.fwl.NF.org.bel.ha <- (nat.fwl.NF.org.bel[13:dim(nat.fwl.NF.org.bel)[1],]*1000000)/nf.org.tot.area
nat.cwl.NF.org.abv.ha <- (nat.cwl.NF.org.abv[13:dim(nat.cwl.NF.org.abv)[1],]*1000000)/nf.org.tot.area


#Year 2012 natmort
#sum(nat.nwl.SF.org.abv[43,],nat.nwl.SF.org.bel[43,], nat.fwl.SF.org.abv[43,], nat.fwl.SF.org.bel[43,],nat.cwl.SF.org.abv[43,], nat.nwl.NF.org.abv[43,], nat.nwl.NF.org.bel[43,], nat.fwl.NF.org.abv[43,], nat.fwl.NF.org.bel[43,],nat.cwl.NF.org.abv[43,], nat.nwl.SF.min[43,], nat.fwl.SF.min[43,], nat.cwl.SF.min[43,], nat.nwl.NF.min[43,], nat.fwl.NF.min[43,], nat.cwl.NF.min[43,])
#1.870495


#########################################   
#  NATURAL MORTALITY
#########################################


# WRITING Yasso Inputs to file (unit mill ton of C ), both DOM, SOM for south and for north


write.csv(nat.nwl.SF.min.ha, (paste(path.litter,"nat.nwl.SF.min.csv", sep="")), row.names = FALSE)
write.csv(nat.fwl.SF.min.ha, (paste(path.litter,"nat.fwl.SF.min.csv", sep="")), row.names = FALSE)
write.csv(nat.cwl.SF.min.ha, (paste(path.litter,"nat.cwl.SF.min.csv", sep="")), row.names = FALSE)

write.csv(nat.nwl.SF.org.abv.ha, (paste(path.litter,"nat.nwl.SF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(nat.fwl.SF.org.abv.ha, (paste(path.litter,"nat.fwl.SF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(nat.nwl.SF.org.bel.ha, (paste(path.litter,"nat.nwl.SF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(nat.fwl.SF.org.bel.ha, (paste(path.litter,"nat.fwl.SF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(nat.cwl.SF.org.abv.ha, (paste(path.litter,"nat.cwl.SF.org.abv.csv", sep="")), row.names = FALSE)

# write.csv(nat.nwl.NF.min.ha, (paste(path.litter,"nat.nwl.NF.min.csv", sep="")), row.names = FALSE)
# write.csv(nat.fwl.NF.min.ha, (paste(path.litter,"nat.fwl.NF.min.csv", sep="")), row.names = FALSE)
# write.csv(nat.cwl.NF.min.ha, (paste(path.litter,"nat.cwl.NF.min.csv", sep="")), row.names = FALSE)

write.csv(nat.nwl.NF.min.ha.whole, (paste(path.litter,"nat.nwl.NF.min.csv", sep="")), row.names = FALSE)
write.csv(nat.fwl.NF.min.ha.whole, (paste(path.litter,"nat.fwl.NF.min.csv", sep="")), row.names = FALSE)
write.csv(nat.cwl.NF.min.ha.whole, (paste(path.litter,"nat.cwl.NF.min.csv", sep="")), row.names = FALSE)

write.csv(nat.nwl.NF.org.abv.ha, (paste(path.litter,"nat.nwl.NF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(nat.fwl.NF.org.abv.ha, (paste(path.litter,"nat.fwl.NF.org.abv.csv", sep="")), row.names = FALSE)
write.csv(nat.nwl.NF.org.bel.ha, (paste(path.litter,"nat.nwl.NF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(nat.fwl.NF.org.bel.ha, (paste(path.litter,"nat.fwl.NF.org.bel.csv", sep="")), row.names = FALSE)
write.csv(nat.cwl.NF.org.abv.ha, (paste(path.litter,"nat.cwl.NF.org.abv.csv", sep="")), row.names = FALSE)



##########################################
#Printing
##########################################

years90 <- 1990:yr
years <- 1972:yr

drain.uusi.above=logg.above+nmort.above
drain.uusi.below=logg.below+nmort.below

drain.uusi.above=(drain.uusi.above[1:4,]+drain.uusi.above[5:8,]+drain.uusi.above[9:12,])[c(1,3,2,4),]*.5*1000
drain.uusi.below=(drain.uusi.below[1:4,]+drain.uusi.below[5:8,]+drain.uusi.below[9:12,])[c(1,3,2,4),]*.5*1000

drain.uusi.above=cbind(rep(1,4), c(1,1,2,2),rep(1,4),c(1,2,1,2),drain.uusi.above)
drain.uusi.below=cbind(rep(2,4), c(1,1,2,2),rep(1,4),c(1,2,1,2),drain.uusi.below)

colnames(drain.uusi.above)=c('abv.bel','region','old','soil',years90)
colnames(drain.uusi.below)=c('abv.bel', 'region','old','soil',years90)

#Combine above and below for drawing a test picture
drain.uusi <- rbind(drain.uusi.above, drain.uusi.below)

drain.uusi.sum <- drain.uusi.above + drain.uusi.below
drain.uusi.sum[,'region']<- c(1,1,2,2)
drain.uusi.sum[,'soil']<- c(1,2,1,2)

#Print
write.table(drain.uusi[,c('abv.bel','region', 'old', 'soil',years90)],paste(path.results,'CRF4_FL-FL_biomass_losses.dat',sep=""), row=F,col=T,quote=F)

#Print directly to crf folder to be imported to crf reporter
#UID E1B2A0CF-5F9F-445E-9715-C274A0CD4A26

crf<-matrix(0,1,length(years90))
rownames(crf) <- c('E1B2A0CF-5F9F-445E-9715-C274A0CD4A26')
colnames(crf) <- c(years90)

crf[1,] <- round( colSums(-drain.uusi[,-(1:4)]),3)

crf[crf==0 ]<- 'NA'

write.table(crf,paste(path.crf,'LU4A1_FL-FL_living_biomass_losses_trees.csv',sep=""), sep=' ', quote=F, col.names=F)



#Draw a graph
postscript((paste(figpath,"drain_lulucf.eps",sep="")), horizontal=F,width=4,height=4)


matplot(years90,t(drain.uusi.sum[,-(1:4)]/1000),type='l',lty=1,
        col=c('red','blue')[drain.uusi.sum[,'region']],lwd=c(3,1)[drain.uusi.sum[,'soil']],bty='n',pty='i',xlab='year',ylab='Drain, Mtonnes',ylim=range(0,drain.uusi.sum[,-(1:4)]/1000))
legend('center',legend=c('South','North','mineral','organic'),col=c('red','blue','red','blue'),lwd=c(rep(1,2),2,rep(1,2)),lty=c(rep(1,5),2),bty='n',ncol=2,cex=.6)
dev.off()


#######################
#######################
### GRAPH about logging residuals

#par(mfrow=c(2,2))
postscript((paste(figpath,"Litter.from.loggings_lulucf.eps",sep="")), horizontal=F,width=10,height=6)

plot(years, colSums(loggings[,3:dim(loggings)[2]]), type="l", ylab="loggings mill. cubics")
text(1983,65, "loggings in Finland")
plot(years, colSums(loggings.branches.origi[,3:dim(loggings)[2]]), type="l", col="red", ylab="branches Tg biomass")
lines(years, colSums(loggings.branches[,3:dim(loggings)[2]]))
text(1980,6, "branch litter")
plot(years, colSums(loggings.stump.origi[,3:dim(loggings)[2]]), type="l", col="red", ylab="stumps Tg biomass")
lines(years, colSums(loggings.stump[,3:dim(loggings)[2]]))
text(1980,2.4, "stump litter")
plot(years, colSums(loggings.stem.origi[,3:dim(loggings)[2]]), type="l", col="red", ylab="stems Tg biomass")
lines(years, colSums(loggings.stem[,3:dim(loggings)[2]]))
text(1980,2.4, "stem litter")
dev.off()


postscript((paste(figpath,"Litter.from.natmort_lulucf.eps",sep="")), horizontal=F,width=10,height=6)

plot(years, colSums(natmort[,3:dim(loggings)[2]]), type="l", ylab="natmort mill. cubics")
text(1983,4, "natmort in Finland")
plot(years, colSums(natmort.branches[,3:dim(loggings)[2]]), type="l", col="red", ylab="branches Tg biomass")
text(1980,0.4, "branch litter")
plot(years, colSums(natmort.stump[,3:dim(loggings)[2]]), type="l", col="red", ylab="stumps Tg biomass")
text(1980,0.15, "stump litter")
plot(years, colSums(natmort.stem[,3:dim(loggings)[2]]), type="l", col="red", ylab="stems Tg biomass")
text(1980,1.5, "stem litter")

dev.off()


##########################
##########################
### making table about drain in m3


pine.min <- pine.loggings.NF.min.bm+pine.loggings.SF.min.bm+ sf.nat.mort[,1]+nf.nat.mort[,1]
spruce.min <- spruce.loggings.NF.min.bm+spruce.loggings.SF.min.bm+ sf.nat.mort[,2]+nf.nat.mort[,2]
birch.min <- birch.loggings.NF.min.bm+birch.loggings.SF.min.bm+ sf.nat.mort[,3]+nf.nat.mort[,3]

pine.org <- pine.loggings.NF.org.bm+pine.loggings.SF.org.bm+ sf.nat.mort[,4]+nf.nat.mort[,4]
spruce.org <- spruce.loggings.NF.org.bm+spruce.loggings.SF.org.bm+ sf.nat.mort[,5]+nf.nat.mort[,5]
birch.org <- birch.loggings.NF.org.bm+birch.loggings.SF.org.bm+ sf.nat.mort[,6]+nf.nat.mort[,6]

table <- cbind(pine.min,spruce.min,birch.min,pine.org,spruce.org,birch.org)
table.nir <- round(cbind(table, rowSums(table)),1)[21:dim(table)[1],]
write.csv(table.nir, file=paste(path.nir,"Appendix_6c_drain.table.csv",sep=""))

###########################
#### Making table about fellings


table2 <- rbind(pine.loggings.SF.min.bm,spruce.loggings.SF.min.bm,birch.loggings.SF.min.bm,pine.loggings.SF.org.bm,spruce.loggings.SF.org.bm,birch.loggings.SF.org.bm,pine.loggings.NF.min.bm,spruce.loggings.NF.min.bm,birch.loggings.NF.min.bm,pine.loggings.NF.org.bm,spruce.loggings.NF.org.bm,birch.loggings.NF.org.bm)

output <- table2[,dim(table2)[2]]

#write.csv(output, file=paste(path.nir,"LU4A1_table_7.2-12.csv",sep="")) ###MIK? T?M? ON?


#new drain table:
#note: crf tables in Gg C, need to convert to Tg biomass for NIR

drain.nir.ab <-logg.above+nmort.above
drain.nir.be <- logg.below+nmort.below
drain.nir.ab <- cbind(rep(1,12), rep(c(1,2),6), rep(c(1,1,2,2),3), c(1,1,1,1,2,2,2,2,3,3,3,3), drain.nir.ab) #AB/BEL, SF/NF, MIN/ORG, SPECIES

drain.nir.be <- cbind(rep(2,12), rep(c(1,2),6), rep(c(1,1,2,2),3), c(1,1,1,1,2,2,2,2,3,3,3,3), drain.nir.be) #AB/BEL, SF/NF, MIN/ORG, SPECIES

drain.nir <- rbind(drain.nir.ab, drain.nir.be)
colnames(drain.nir)=c('abv.bel','region','soil','species',years90)
drain.nir.min = drain.nir[drain.nir[,3]==1,]
drain.nir.org = drain.nir[drain.nir[,3]==2,]

nir.min <-drain.nir.min[order(drain.nir.min[,2],drain.nir.min[,1],drain.nir.min[,4]),]
nir.org <- drain.nir.org[order(drain.nir.org[,2],drain.nir.org[,1],drain.nir.org[,4]),]

nir.min <- round(t(nir.min[,-(1:4)]),2)
nir.org <- round(t(nir.org[,-(1:4)]),2)

colnames(nir.min) <- c('s.ab.sp', 's.ab.ns', 's.ab.bl', 's.be.sp', 's.be.ns', 's.be.bl', 'n.ab.sp', 'n.ab.ns', 'n.ab.bl', 'n.be.sp', 'n.be.ns', 'n.be.bl')

colnames(nir.org) <- c('s.ab.sp', 's.ab.ns', 's.ab.bl', 's.be.sp', 's.be.ns', 's.be.bl', 'n.ab.sp', 'n.ab.ns', 'n.ab.bl', 'n.be.sp', 'n.be.ns', 'n.be.bl')

nir.min <- cbind(nir.min, rowSums(nir.min))
nir.org <- cbind(nir.org, rowSums(nir.org))

#write.csv(nir.min, file=paste(path.nir,"/appendix/Table_bm_min_App_NOT_NIR.csv",sep=""))
#write.csv(nir.org, file=paste(path.nir,"/appendix/Table_bm_org_App_NOT_NIR.csv",sep=""))

###################################################
#QAQC: Compare to last year's tables. Draw a graph:

#Read old data:
path.old <- paste(path,(yr-1),'/results/', sep="")
drain.old <- read.table(paste(path.old,"CRF4_FL-FL_biomass_losses.dat", sep=""), sep=" ",header=TRUE)

#Draw graphs:
y.old <- years90[-length(years90)] #1990-2016
y.new <- years90

#par(mfrow=c(4,2))
postscript((paste(figpath,"Comparison.lulucf.litter.eps",sep="")), horizontal=F,width=10,height=6)

leg <- c("old", "new")
cols <- c("blue", "green")
lines <- c(2,1)
plot(y.new,drain.uusi[1,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Above-ground South Mineral", lwd=2)
lines(y.old,drain.old[1,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)
legend(2000,11000, leg, col=cols,lty=lines, lwd=1, bty="n", cex=0.9)

plot(y.new,drain.uusi[2,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Above-ground South Organic", lwd=2)
lines(y.old,drain.old[2,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)

plot(y.new,drain.uusi[3,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Above-ground North Mineral", lwd=2)
lines(y.old,drain.old[3,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)

plot(y.new,drain.uusi[4,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Above-ground North Organic", lwd=2)
lines(y.old,drain.old[4,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)

plot(y.new,drain.uusi[5,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Below-ground South Mineral", lwd=2)
lines(y.old,drain.old[5,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)

plot(y.new,drain.uusi[6,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Below-ground South Organic", lwd=2)
lines(y.old,drain.old[6,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)

plot(y.new,drain.uusi[7,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Below-ground North Mineral", lwd=2)
lines(y.old,drain.old[7,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)

plot(y.new,drain.uusi[8,5:dim(drain.uusi)[2]], type="l", ylab="Drain", xlab="Years", col="green", main="Below-ground North Organic", lwd=2)

lines(y.old,drain.old[8,5:dim(drain.old)[2]], col="blue", lty=2, lwd=2)

dev.off()


##################################
### For the uncertainty estimation
##################################



write.table(cbind(data.frame(sn=rep(1:2,6),soil=rep(rep(1:2,each=2),3),species=rep(1:3,each=4)),loggings),paste(path, yr, "/uncertainty/trees/volLogg.dat", sep=""),row=F,quote=F)

