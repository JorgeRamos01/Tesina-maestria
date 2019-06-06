rm(list=ls())
library(maptools)
library(spdep)
library(splm)
library(plm)
library(readstata13)
library(dplyr)
library(ggpubr)

setwd("~/Documentos/Tesina/tesina")
datos_tesina<-read.dta13("baseWB-13-feb-2017.dta")

years<-c("2004","2009","2014")
datos_tesina <- datos_tesina %>% filter(year %in% years)

#Promedios por a\~no de personal por industria en todos los municipios
datos_tesina %>% group_by(year) %>% summarise(avg_per_com=mean(per_com/personnel, na.rm =TRUE),
                                              avg_per_mfng=mean(per_mfng/personnel, na.rm =TRUE), 
                                              avg_per_ming=mean(per_ming/personnel, na.rm =TRUE),
                                              avg_per_serv=mean(per_serv/personnel, na.rm =TRUE))

#Generamos las columnas de promedios
#datos_tesina<- datos_tesina %>% mutate(avg_per_com=case_when(year=="1986"~0.534, year=="1989"~0.528, year=="1994"~0.498, year=="1999"~0.419, 
                                        #                     year=="2004"~0.496, year=="2009"~0.449, year=="2014"~0.420),
                                       #avg_per_mfng=case_when(year=="1986"~0.272, year=="1989"~0.246, year=="1994"~0.281, year=="1999"~0.269, 
                                        #                    year=="2004"~0.232, year=="2009"~0.212, year=="2014"~0.214),
                                       #avg_per_ming=case_when(year=="1986"~0.00818, year=="1989"~0.0457, year=="1994"~0.0267, year=="1999"~0.0213, 
                                      #                        year=="2004"~0.0181, year=="2009"~0.0163, year=="2014"~0.00711),
#                                       avg_per_serv=case_when(year=="1986"~0.186, year=="1989"~0.180, year=="1994"~0.195, year=="1999"~0.228, 
#                                                              year=="2004"~0.189, year=="2009"~0.248, year=="2014"~0.226))

datos_tesina<- datos_tesina %>% mutate(avg_per_com=case_when(year=="2004"~0.496, year=="2009"~0.449, year=="2014"~0.420),
            avg_per_mfng=case_when(year=="2004"~0.232, year=="2009"~0.212, year=="2014"~0.214),
            avg_per_ming=case_when(year=="2004"~0.0181, year=="2009"~0.0163, year=="2014"~0.00711),
           avg_per_serv=case_when(year=="2004"~0.189, year=="2009"~0.248, year=="2014"~0.226))

datos_tesina <- datos_tesina %>% mutate(ksi_empleo=abs(per_com/personnel - avg_per_com)+abs(per_mfng/personnel - avg_per_mfng)+
                         abs(per_ming/personnel - avg_per_ming)+abs(per_serv/personnel - avg_per_serv))

#datos_tesina %>% group_by(year) %>% summarize(median(pop_15))

datos_tesina<-datos_tesina %>% mutate(dummy_highedu=case_when(#year=="1986" & high_edu>0.0136~1, year=="1986" & high_edu<=0.0136~0, 
                                                              #year=="1989" & high_edu>0.0180~1, year=="1989" & high_edu<=0.0136~0,
                                                              #year=="1994" & high_edu>0.0221~1, year=="1994" & high_edu<=0.0221~0,
                                                              #year=="1999" & high_edu>0.0263~1, year=="1999" & high_edu<=0.0263~0,
                                                              year=="2004" & high_edu>0.0382~1, year=="2004" & high_edu<=0.0382~0,
                                                              year=="2009" & high_edu>0.0515~1, year=="2009" & high_edu<=0.0515~0,
                                                              year=="2014" & high_edu>0.108~1, year=="2014" & high_edu<=0.108~0))

datos_tesina<-datos_tesina %>% mutate(dummy_pop15=case_when(#year=="1986" & high_edu>0.0136~1, year=="1986" & high_edu<=0.0136~0, 
  #year=="1989" & high_edu>0.0180~1, year=="1989" & high_edu<=0.0136~0,
  #year=="1994" & high_edu>0.0221~1, year=="1994" & high_edu<=0.0221~0,
  #year=="1999" & pop_15>7151~1, year=="1999" & high_edu<=7151~0,
  year=="2004" & pop_15>7506~1, year=="2004" & pop_15<=7506~0,
  year=="2009" & pop_15>8563~1, year=="2009" & pop_15<=8563~0,
  year=="2014" & pop_15>8563~1, year=="2014" & pop_15<=0.8563~0))

model_test<-plm(I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*capital)+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu, 
                data=datos_tesina, model="pooling", index = c("year", "code"))

model_test1<-plm(I(log(ksi_empleo+1))~lndist1kk, 
                data=datos_tesina, model="pooling", index = c("year", "code"))


######Generando la matriz de pesos
#municipios_nuevo<-st_read("municipios2.shp")
municipios_nuevo<-readShapePoly("municipios3.shp", IDvar="new_code")

coords<-coordinates(municipios_nuevo)

text(coords, label=sapply(slot(municipios_nuevo, "polygons"),function(i) slot(i, "ID")))

contb<-dnearneigh(coordinates(municipios_nuevo), 0, 380000, longlat = F)

W<-nb2listw(contb, glist=NULL, style="W")

##### Generando los modelos de panel spacial de datos
#Efectos aleatorios
model1<-spml(I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu, 
             data=datos_tesina, listw=W, model="random", spatial.error="n", lag=FALSE, index = c("code", "year"))

model1_2<-spml(I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu, 
               data=datos_tesina, listw=W, model="random", spatial.error="n", lag=TRUE, index = c("code", "year"))

model1_3<-spml(I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu, 
               data=datos_tesina, listw=W, model="random", spatial.error="n", lag=TRUE, index = c("code", "year"))

#model1_5<-spml(I(log(ksi_empleo+1))~lndist1kk, 
#               data=datos_tesina, listw=W, model="random", spatial.error="b", lag=FALSE, index = c("code", "year"))
#No jala

#Efectos fijos
model0<-plm(I(log(ksi_empleo+1))~lndist1kk, 
               data=datos_tesina, index = c("code", "year"))

model0_1<-spml(I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*capital)+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu), 
               data=datos_tesina, listw=W, spatial.error="none", lag=TRUE, index = c("code", "year"))

model0_2<-spml(log(ksi_empleo)~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), 
               data=datos_tesina, listw=W, spatial.error="none", lag=TRUE, index = c("code", "year")) #

ggqqplot(model0_2$residuals)

model0_2_1<-spml(I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*dummy_highedu)+dummy_highedu, 
               data=datos_tesina, listw=W, spatial.error="b", lag=FALSE, index = c("code", "year"))

#model0_2_1b<-spml(I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu, 
#                 data=datos_tesina, listw=W, spatial.error="none", lag=TRUE, index = c("code", "year"), effect="twoways")

model0_2_2<-spml(I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), 
               data=datos_tesina, listw=W, spatial.error="none", lag=TRUE, index = c("code", "year"))

ggqqplot(model0_2_2$residuals)

#model0_2_2b<-spml(I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu, 
#                 data=datos_tesina, listw=W, spatial.error="none", lag=TRUE, index = c("code", "year"), effect="twoways")

model0_2_3<-spml(I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), 
                 data=datos_tesina, listw=W, spatial.error="b", lag=FALSE, index = c("code", "year"))

#Comprobamos si hay efectos aleatorios modelos ksi
test1<-splm::bsktest(x=I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="LM1", index = c("code", "year"))

#Autocorrelacion espacial
test2<-splm::bsktest(x=I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="CLMmu", index = c("code", "year"))

#Hausman
test3<-sphtest(x=I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="CLMmu", 
               index = c("code", "year"), spatial.model = "lag", errors="BSK", method="ML")

test4<-sphtest(x=I(log(ksi_empleo+1))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="CLMmu", 
               index = c("code", "year"), spatial.model = "lag", errors="KKP", method="ML")

#Comprobamos si hay efectos aleatorios modelos emple
test1_1<-splm::bsktest(x=I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="LM1", index = c("code", "year"))

#Autocorrelacion espacial
test2_1<-splm::bsktest(x=I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="LM2", index = c("code", "year"))

#Hausman
test3_1<-sphtest(x=I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="CLMmu", 
               index = c("code", "year"), spatial.model = "lag", errors="BSK", method="ML")

test4_1<-sphtest(x=I(log(personnel))~lndist1kk+I(lndist1kk*oil)+I(lndist1kk*dummy_highedu)+dummy_highedu+I(lndist1kk*capital), data=datos_tesina, listw=W, test="CLMmu", 
               index = c("code", "year"), spatial.model = "lag", errors="KKP", method="ML")
