
#4.10.2017 Updated for 2016 by Paula
#2.10.2014 Updated for 2013 by Aleksi
#26.8.2013 Updated for 2012 by Aleksi
# Checked against previous year data

### this file makes litterfall estimates in AWEN components from NFI data
# modified from 2009 inventory to 2010, 24.5.2011 PPuo
# to be used in the ghg inventory 27.9.2011

year <- 2019
y.old <- 2018 ## #note this is old

funpath <-  paste('Z:/d4/projects/khk/ghg/',year,'/soil/functions/', sep="") 
outpath <-   paste('Z:/d4/projects/khk/ghg/',year,'/soil/nfidata/nfi10/litter/', sep="")
trees  <-  paste('Z:/d4/projects/khk/ghg/',year,'/trees/stock/remaining/', sep="")

source(paste(funpath,"litter.r",sep=""))
source("Work/biomass.R")
source(paste(funpath,"uncertainty.r",sep=""))


############
# 2. Organic soils !!!
############

## Note, here abovegroung- and belowground litter is needed separately!!!!


## reading in mean biomass from Juha

sp.sf.org <- read.csv(paste(trees,"forestMeanPineSouthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
ns.sf.org <- read.csv(paste(trees,"forestMeanSpruceSouthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
br.sf.org <- read.csv(paste(trees,"forestMeanDecidSouthDrained.dat", sep=""), header=TRUE, as.is=TRUE)

sp.sf.org.mark <- read.csv(paste(trees,"Marklund/forestMeanPineSouthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
ns.sf.org.mark <- read.csv(paste(trees,"Marklund/forestMeanSpruceSouthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
br.sf.org.mark <- read.csv(paste(trees,"Marklund/forestMeanDecidSouthDrained.dat", sep=""), header=TRUE, as.is=TRUE)

#### HERE IS LITTER MASS in C!!!!

#pine
sp.sf.org.fol <- carbon(foliage.litter(sp.sf.org$foliage,1,1,0))
sp.sf.org.bra <- carbon(branch.litter(sp.sf.org$liveBranch,1))
sp.sf.org.stu <- carbon(stump.litter(sp.sf.org$stump,1))
sp.sf.org.roo <- carbon(root.litter(sp.sf.org$roots,1))
sp.sf.org.bar <- carbon(bark.litter(sp.sf.org$stemwood+sp.sf.org$bark,1)) ## ste,o.b.
sp.sf.org.fr <- carbon(fineroot.litter(fineroots(sp.sf.org.mark$foliage,1)))
#spruce
ns.sf.org.fol <- carbon(foliage.litter(ns.sf.org$foliage,2,1,0))
ns.sf.org.bra <- carbon(branch.litter(ns.sf.org$liveBranch,2))
ns.sf.org.stu <- carbon(stump.litter(ns.sf.org$stump,2))
ns.sf.org.roo <- carbon(root.litter(ns.sf.org$roots,2))
ns.sf.org.bar <- carbon(bark.litter(ns.sf.org$stemwood+ns.sf.org$bark,2)) ## ste,o.b.
ns.sf.org.fr <- carbon(fineroot.litter(fineroots(ns.sf.org.mark$foliage,2)))
#birch
br.sf.org.fol <- carbon(foliage.litter(br.sf.org$foliage,3,1,0))
br.sf.org.bra <- carbon(branch.litter(br.sf.org$liveBranch,3))
br.sf.org.stu <- carbon(stump.litter(br.sf.org$stump,3))
br.sf.org.roo <- carbon(root.litter(br.sf.org$roots,3))
br.sf.org.bar <- carbon(bark.litter(br.sf.org$stemwood+br.sf.org$bark,3)) ## ste,o.b.
br.sf.org.fr <- carbon(fineroot.litter(fineroots(br.sf.org$foliage,3)))

###
# Northern Finland
###
sp.nf.org <- read.csv(paste(trees,"forestMeanPineNorthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
ns.nf.org <- read.csv(paste(trees,"forestMeanSpruceNorthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
br.nf.org <- read.csv(paste(trees,"forestMeanDecidNorthDrained.dat", sep=""), header=TRUE, as.is=TRUE)

sp.nf.org.mark <- read.csv(paste(trees,"Marklund/forestMeanPineNorthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
ns.nf.org.mark <- read.csv(paste(trees,"Marklund/forestMeanSpruceNorthDrained.dat", sep=""), header=TRUE, as.is=TRUE)
br.nf.org.mark <- read.csv(paste(trees,"Marklund/forestMeanDecidNorthDrained.dat", sep=""), header=TRUE, as.is=TRUE)

#pine
sp.nf.org.fol <- carbon(foliage.litter(sp.nf.org$foliage,1,2,0))
sp.nf.org.bra <- carbon(branch.litter(sp.nf.org$liveBranch,1))
sp.nf.org.stu <- carbon(stump.litter(sp.nf.org$stump,1))
sp.nf.org.roo <- carbon(root.litter(sp.nf.org$roots,1))
sp.nf.org.bar <- carbon(bark.litter(sp.nf.org$stemwood+sp.nf.org$bark,1)) ## ste,o.b.
sp.nf.org.fr <- carbon(fineroot.litter(fineroots(sp.nf.org.mark$foliage,1)))
#spruce
ns.nf.org.fol <- carbon(foliage.litter(ns.nf.org$foliage,2,2,0))
ns.nf.org.bra <- carbon(branch.litter(ns.nf.org$liveBranch,2))
ns.nf.org.stu <- carbon(stump.litter(ns.nf.org$stump,2))
ns.nf.org.roo <- carbon(root.litter(ns.nf.org$roots,2))
ns.nf.org.bar <- carbon(bark.litter(ns.nf.org$stemwood+ns.nf.org$bark,2)) ## ste,o.b.
ns.nf.org.fr <- carbon(fineroot.litter(fineroots(ns.nf.org.mark$foliage,2)))
#birch
br.nf.org.fol <- carbon(foliage.litter(br.nf.org$foliage,3,2,0))
br.nf.org.bra <- carbon(branch.litter(br.nf.org$liveBranch,3))
br.nf.org.stu <- carbon(stump.litter(br.nf.org$stump,3))
br.nf.org.roo <- carbon(root.litter(br.nf.org$roots,3))
br.nf.org.bar <- carbon(bark.litter(br.nf.org$stemwood+br.nf.org$bark,3)) ## ste,o.b.
br.nf.org.fr <- carbon(fineroot.litter(fineroots(br.nf.org$foliage,3)))


##### Then converting litter to AWEN units

sp.sf.fol.awen <- foliage.AWEN(sp.sf.org.fol, 1) # foliage
sp.sf.bra.awen <- branches.AWEN(sp.sf.org.bra) # branches
sp.sf.stu.awen <- stem.AWEN(sp.sf.org.stu, 1) # stump
sp.sf.roo.awen <- branches.AWEN(sp.sf.org.roo) #roots
sp.sf.bar.awen <- branches.AWEN(sp.sf.org.bar) # bark
sp.sf.fr.awen <- fineroot.AWEN(sp.sf.org.fr, 1) # fineroot

ns.sf.fol.awen <- foliage.AWEN(ns.sf.org.fol, 2)
ns.sf.bra.awen <- branches.AWEN(ns.sf.org.bra)
ns.sf.stu.awen <- stem.AWEN(ns.sf.org.stu, 2)
ns.sf.roo.awen <- branches.AWEN(ns.sf.org.roo)
ns.sf.bar.awen <- branches.AWEN(ns.sf.org.bar)
ns.sf.fr.awen <- fineroot.AWEN(ns.sf.org.fr, 2)

br.sf.fol.awen <- foliage.AWEN(br.sf.org.fol, 3)
br.sf.bra.awen <- branches.AWEN(br.sf.org.bra)
br.sf.stu.awen <- stem.AWEN(br.sf.org.stu, 3)
br.sf.roo.awen <- branches.AWEN(br.sf.org.roo)
br.sf.bar.awen <- branches.AWEN(br.sf.org.bar)
br.sf.fr.awen <- fineroot.AWEN(br.sf.org.fr, 3)

nwl.sf.org.abv <- sp.sf.fol.awen+ns.sf.fol.awen+br.sf.fol.awen
nwl.sf.org.bel <- sp.sf.fr.awen+ns.sf.fr.awen+br.sf.fr.awen

fwl.sf.org.abv <- sp.sf.bra.awen+sp.sf.stu.awen+sp.sf.bar.awen+ns.sf.bra.awen+ns.sf.bar.awen+br.sf.bra.awen+br.sf.stu.awen+br.sf.bar.awen ## note, no bark litter from Spruce
fwl.sf.org.bel <- sp.sf.roo.awen+ns.sf.roo.awen+br.sf.roo.awen ## note, no bark litter from Spruce


write.csv(nwl.sf.org.abv, paste(outpath,"nwl.sf.1990",year,".org.abv.csv", sep="") , row.names = FALSE, quote = FALSE)
write.csv(nwl.sf.org.bel, paste(outpath,"nwl.sf.1990",year,".org.bel.csv", sep="") , row.names = FALSE, quote = FALSE)
write.csv(fwl.sf.org.abv, paste(outpath,"fwl.sf.1990",year,".org.abv.csv", sep="") , row.names = FALSE, quote = FALSE)
write.csv(fwl.sf.org.bel, paste(outpath,"fwl.sf.1990",year,".org.bel.csv", sep="") , row.names = FALSE, quote = FALSE)

sp.nf.fol.awen <- foliage.AWEN(sp.nf.org.fol, 1)
sp.nf.bra.awen <- branches.AWEN(sp.nf.org.bra)
sp.nf.stu.awen <- stem.AWEN(sp.nf.org.stu, 1)
sp.nf.roo.awen <- branches.AWEN(sp.nf.org.roo)
sp.nf.bar.awen <- branches.AWEN(sp.nf.org.bar)
sp.nf.fr.awen <- fineroot.AWEN(sp.nf.org.fr, 1)

ns.nf.fol.awen <- foliage.AWEN(ns.nf.org.fol, 2)
ns.nf.bra.awen <- branches.AWEN(ns.nf.org.bra)
ns.nf.stu.awen <- stem.AWEN(ns.nf.org.stu, 2)
ns.nf.roo.awen <- branches.AWEN(ns.nf.org.roo)
ns.nf.bar.awen <- branches.AWEN(ns.nf.org.bar)
ns.nf.fr.awen <- fineroot.AWEN(ns.nf.org.fr, 2)

br.nf.fol.awen <- foliage.AWEN(br.nf.org.fol, 3)
br.nf.bra.awen <- branches.AWEN(br.nf.org.bra)
br.nf.stu.awen <- stem.AWEN(br.nf.org.stu, 3)
br.nf.roo.awen <- branches.AWEN(br.nf.org.roo)
br.nf.bar.awen <- branches.AWEN(br.nf.org.bar)
br.nf.fr.awen <- fineroot.AWEN(br.nf.org.fr, 3)


nwl.nf.org <- sp.nf.fol.awen+sp.nf.fr.awen+ns.nf.fol.awen+ns.nf.fr.awen+br.nf.fol.awen+br.nf.fr.awen

fwl.nf.org <- sp.nf.bra.awen+sp.nf.stu.awen+sp.nf.roo.awen+sp.nf.bar.awen+ns.nf.bra.awen+ns.nf.roo.awen+ns.nf.bar.awen+br.nf.bra.awen+br.nf.stu.awen+br.nf.roo.awen+br.nf.bar.awen ## note, no bark litter from Spruce

write.csv(nwl.nf.org, paste(outpath,"nwl.nf.1990",year,".org.csv", sep="") , row.names = FALSE, quote = FALSE)
write.csv(fwl.nf.org, paste(outpath,"fwl.nf.1990",year,".org.csv", sep="") , row.names = FALSE, quote = FALSE)


nwl.nf.org.abv <- sp.nf.fol.awen+ns.nf.fol.awen+br.nf.fol.awen
nwl.nf.org.bel <- sp.nf.fr.awen+ns.nf.fr.awen+br.nf.fr.awen

fwl.nf.org.abv <- sp.nf.bra.awen+sp.nf.stu.awen+sp.nf.bar.awen+ns.nf.bra.awen+ns.nf.bar.awen+br.nf.bra.awen+br.nf.stu.awen+br.nf.bar.awen ## note, no bark litter from Spruce
fwl.nf.org.bel <- sp.nf.roo.awen+ns.nf.roo.awen+br.nf.roo.awen ## note, no bark litter from Spruce

write.csv(nwl.nf.org.abv, paste(outpath,"nwl.nf.1990",year,".org.abv.csv", sep="") , row.names = FALSE, quote = FALSE)
write.csv(nwl.nf.org.bel, paste(outpath,"nwl.nf.1990",year,".org.bel.csv", sep="") , row.names = FALSE, quote = FALSE)
write.csv(fwl.nf.org.abv, paste(outpath,"fwl.nf.1990",year,".org.abv.csv", sep="") , row.names = FALSE, quote = FALSE)
write.csv(fwl.nf.org.bel, paste(outpath,"fwl.nf.1990",year,".org.bel.csv", sep="") , row.names = FALSE, quote = FALSE)

