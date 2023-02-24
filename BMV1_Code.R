#Packages Required
install.packages("tidyverse")
library(tidyverse)
library(dplyr)#for correlation matrix
library(biomod2)
library(raster)

#Stacking soil and climate data
setwd("C:/Users//paude/Desktop/BioMod2Try/try")
rlist=list.files(pattern="tif$",full.names = TRUE)
s_xvar=stack(rlist)
names(s_xvar)

#Projecting all to WGS_1984 becasue biomod doesn't work in NAD..
r1=raster("D:/sdm/current_projection/Current_projection.grd")#reference for WGS projection
r1

r2=raster("C:/Users/paude/Desktop/BioMod2Try/try/clay.tif")#trial if it works

r2

clay1=projectRaster(r2, crs = r1)
clay1


s_xvar1=projectRaster(s_xvar, crs = clay1)#projecting stack downscaled variables along with soil


#Correlation Maxtix
presvals_all<-cbind(raster::extract(s_xvar1,Aspen[,c("longitude","lattitude")]),Aspen[,c("longitude","lattitude")])
cor_aspen_all<-presvals_all%>% select(-c("longitude","lattitude"))%>%cor()
cor_aspen_all
write.csv(cor_aspen_all,"C:/Users/paude/Desktop/BioMod2Try/aspen_cor.csv")

#Response varaible
Aspen <- read.csv("C:/Users/paude/Desktop/BioMod2Try/aspen.csv")
myRespName <- "Aspen"
myResp <- as.numeric(DataAspen[,myRespName])
myRespXY <- DataAspen[,c("longitude","lattitude")]


Explvar=raster::stack(s_xvar1$clay,s_xvar1$CMD,s_xvar1$MAT)
myExpl=raster::stack(Explvar)


presvals_all<-cbind(raster::extract(s_xvar1,Aspen[,c("longitude","lattitude")]),Aspen[,c("longitude","lattitude")])

cor_aspen_all<-presvals_all%>% select(-c("longitude","lattitude"))%>%cor()


myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName, )
myBiomodData
plot(myBiomodData)                               
myBiomodOption <- BIOMOD_ModelingOptions()
Aspen_models <-
  BIOMOD_Modeling(
    bm.format = myBiomodData,
    models = c("GLM", "GBM", "RF"),
    bm.options = myBiomodOption,
    nb.rep = 2,
    data.split.perc = 80,
    var.import = 3,
    modeling.id = 'demo1'
  )
Aspen_models
get_variables_importance(Aspen_models,as.data.frame=TRUE)
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = Aspen_models,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.7),
                                      metric.eval = c('TSS', 'ROC'),
                                      var.import = 3,
                                      prob.mean = TRUE,
                                      prob.median = FALSE,
                                      prob.cv = FALSE,
                                      prob.ci = FALSE,
                                      prob.ci.alpha = 0.05,
                                      committee.averaging = TRUE,
                                      prob.mean.weight = FALSE,
                                      prob.mean.weight.decay = 'proportional',
                                      seed.val = 42)
myBiomodProj <- BIOMOD_Projection(bm.mod = Aspen_models,
                                  proj.name = 'Current',
                                  new.env = myExpl,
                                  models.chosen = 'all',
                                  metric.binary = 'all',
                                  metric.filter = 'all',
                                  build.clamping.mask = TRUE,
                                  output.format = '.img')
myBiomodProj

plot(myBiomodProj)

BiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                           proj.name = 'CurrentEM',
                                           new.env = myExpl,
                                           models.chosen = 'all',
                                           metric.binary = 'all',
                                           metric.filter = 'all',
                                           output.format = '.img')

BiomodEMProj
plot(BiomodEMProj)
a1<-raster("C:/Users/paude/Desktop/BioMod2Try/try/Aspen/proj_CurrentEM/individual_projections/Aspen_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img")
plot(a1)