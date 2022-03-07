

year<-2019
yr <- 2019


path <- 'Z:/d4/projects/khk/ghg/'


################################################
#    General output
################################################

path.crf <- paste('/hsan2/khk/ghg/',year,'/crf/',sep="")
path.nir <- paste('/hsan2/khk/ghg/',year,'/NIR/',sep="")

#This path for detailed results, other use than crf reporter
path.results <- paste('/hsan2/khk/ghg/',year,'/results/',sep="")

path.eu529 <- paste('/hsan2/khk/ghg/',year,'/EU529/',sep="")

################################################
#  Areas
################################################

areaspath <- paste('/hsan2/khk/ghg/',year,'/areas/',sep="")
path.areas.lulucf <- paste(path,yr,'/areas/lulucf/results/',sep="")
path.areas.kp <- paste(path,yr,'/areas/kplulucf/results/',sep="")

path.areas.eu529 <- paste(path,yr,'/areas/EU529/',sep="")

################################################
#  Biomass
################################################

###Drain





#################################################
##  Soil paths
#################################################

#yasso07path is used to locate yasso07.so dynamic library in load_yasso07.r
yasso07path <- paste('/hsan2/khk/ghg/r',year,'/soil/functions/yasso07/',sep="")
funpath <-  paste('/hsan2/khk/ghg/',year,'/soil/functions/',sep="")
nfidata <- paste('/hsan2/khk/ghg/',year,'/soil/nfidata/',sep="")

#figures output directory
#figpath <- './'

#emission factors output directory
#efspath <- './'
climpath <- paste('/hsan2/khk/ghg/',year,'/soil/weather/output/',sep="")
undpath <- paste('/hsan2/khk/ghg/',year,'/soil/understorey/litter/',sep="")
#Yasso parameter set 
ypath <- paste('/hsan2/khk/ghg/',year,'/soil/functions/yasso07/',sep="")   
juhapath <- paste('/hirsi/hsan2/khk/ghg/',year,'/trees/stock/converted/',sep="")


