#Data Preprocessing
library(raster)
library(rgdal)
library(mapview)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
# neighbourhood<-readOGR(dsn="/nobackup/users/dirksen/data/auxcillary_NED/Buurtenkaart2018/",layer="buurt2018")
# district<-readOGR(dsn="/nobackup/users/dirksen/data/auxcillary_NED/Buurtenkaart2018/",layer="wijk_2018")
############################Layer information SVF and Greenness within district Utrecht
municipality<-readOGR(dsn="/nobackup/users/dirksen/data/auxcillary_NED/Buurtenkaart2018/",layer="gem_2018")
Utrecht<-municipality[which(municipality$GM_NAAM=="Utrecht"),]

greenness<-stack("/nobackup/users/dirksen/data/auxcillary_NED/rivm_20170415_g_groenkaart_10m/rivm_20170415_g_groenkaart_10m.tif")
Utrecht<-spTransform(Utrecht,crs(greenness))

greenness.u<-crop(greenness,extent(Utrecht))
greenness.u<-mask(greenness.u,Utrecht)

mapview(Utrecht) + greenness.u
#############################Point information Utrecht
df<-fread("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/meta_data.txt")
coordinates(df)<- ~Lon + Lat
crs(df)<-CRS("+proj=longlat +datum=WGS84")
df<-spTransform(df,crs(Utrecht))

df.in.poly<-df[!is.na(over(df,Utrecht))[,1],]
df.utrecht<-data.frame(df.in.poly)
df.utrecht$exists<-unlist(lapply(df.utrecht$Station.ID,function(x) dir.exists(paste0("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax_Utrecht/",x))))
df.utrecht<-df.utrecht[df.utrecht$exists,] #only select stations for which we do have data

mapview(Utrecht) + greenness.u + df.in.poly

#############################SVF & Greenness grid
svf<-stack("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/grid_files_veg_svf/svf_utrecht_1m.grd")
grn<-stack("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/grid_files_veg_svf/greenness_utrecht_smooth_500m.grd")

df<-fread("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/meta_data.txt")
coordinates(df)<- ~Lon + Lat
crs(df)<-CRS("+proj=longlat +datum=WGS84")
df<-spTransform(df,crs(svf))

df@data$svf<-as.numeric(extract(svf,df))
df@data$grn<-as.numeric(extract(grn,df))
df<-df[complete.cases(df@data$svf),]
mapview(df,zcol="svf") +grn

saveRDS(df,"inst/Rdata/wunderground_svf_grn.rds")
#############################Information from the rural station
Cabauw<-fread("/nobackup/users/dirksen/data/AWS/Cabauw_T_S_W_2016_201809.csv")
Cabauw$IT_DATETIME<-as.POSIXct(Cabauw$IT_DATETIME,format="%Y%m%d_%H%M00_000000")
Cabauw.S<-subset(Cabauw[which(Cabauw$DS_CODE=="348_S_a"),],select = c("IT_DATETIME","DS_CODE","TOS.Q_GLOB_10"))
Cabauw.T<-subset(Cabauw[which(Cabauw$DS_CODE=="348_T_a"),],select = c("IT_DATETIME","DS_CODE","TOT.T_DRYB_10"))
Cabauw.W<-subset(Cabauw[which(Cabauw$DS_CODE=="348_W_a"),],select = c("IT_DATETIME","DS_CODE","TOW.FF_SENSOR_10"))

S<-calc_S(Cabauw.S$IT_DATETIME,Cabauw.S$TOS.Q_GLOB_10)
DTR<-calc_DTR(Cabauw.T$IT_DATETIME,Cabauw.T$TOT.T_DRYB_10)
U<-calc_U(Cabauw.W$IT_DATETIME,Cabauw.W$TOW.FF_SENSOR_10)

Meteo_params<-data.frame(DTR,"S"=S$S[1:length(DTR$start)],"U"=U$W)
saveRDS(Meteo_params,"inst/Rdata/Meteo_params_cabauw.rds")

############################Wunderground data
# library(dplyr)
wur_stations<-list.files("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax_Utrecht/",
           full.names=TRUE,
           recursive = TRUE)
wur_names<-list.files("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax_Utrecht/",
                         include.dirs=TRUE,full.names=FALSE)

wur_stations_data<-lapply(wur_stations,"fread")

TC<-lapply(wur_stations_data,function(x) x[[1]]) #select the temperatures
wur_filtered<-rapply(TC,function(x) ifelse(x < -50,NA,x),how="replace") #filter the temperature

TS<-lapply(wur_stations_data,function(x) x[[2]]) #select the timestamps
ts_filtered<-lapply(TS,function(df) as.POSIXct(df))

list.filtered<-map2(.x=wur_stations_data,.y=wur_filtered,~mutate(.,TemperatureC=.y)) #put the filtered values back in the list
list.filtered<-map2(.x=list.filtered,.y=ts_filtered,~mutate(.,Timestamp_UTC=.y))

############################Selecting Wunderground data without gaps
tstamp<-Cabauw$IT_DATETIME
Dmin<-min(Cabauw$IT_DATETIME)
Dmax<-max(Cabauw$IT_DATETIME)

for(i in 1:length(list.filtered)){
print(wur_names[i])
fname<-paste0(wur_names[i],"_filtered.rds")
df<-list.filtered[[i]]

df_complete<-df[complete.cases(df),]
df_complete<-df_complete[which(df_complete$Timestamp_UTC>Dmin &
                                 df_complete$Timestamp_UTC<Dmax),]
df_complete$gap_min<-c(as.numeric(diff(df_complete$Timestamp_UTC)/60),NA)
df_complete$over_tresh<-df_complete$gap_min>60

#Determine the length of the logical vector over_tresh
DT <- data.table(df_complete$over_tresh, runid = rleid(df_complete$over_tresh))
DT_stat <- DT[,.(length = .N, position = .I[1], type=df_complete$over_tresh[1]), by = runid]
# order by length
setorder(DT_stat, -length)
# Get top 3 (or n) by type
continuous_mm<-DT_stat[, .SD, type]
continuous_mm$days<-continuous_mm$length/(60*7)
continuous_mm$start<-df_complete$Timestamp_UTC[continuous_mm$position]
continuous_mm$stop<-df_complete$Timestamp_UTC[continuous_mm$position+(continuous_mm$length-1)]

Irel<-which(continuous_mm$days>7)
t_start<-round_date(continuous_mm$start[Irel],"10 mins")
t_stop<-round_date(continuous_mm$stop[Irel],"10 mins")

interpolate_period<-function(start,stop){
t_sq<-seq(from=start,to=stop,by="10 mins")
T_new<-approx(x=df_complete$Timestamp_UTC,y=df_complete$TemperatureC,xout=t_sq)
output<-data.frame(t_sq,T_new$y)
names(output)<-c("new_time","T_int")
return(output)
}

int_time<-mapply(interpolate_period,start=t_start,stop=t_stop,SIMPLIFY = FALSE)
df_int_time<-do.call("rbind",int_time)

output_ls<-list("filtered_obs"=df_complete,
                "continuous_measurements"=continuous_mm,
                "interpolated_time"=df_int_time)
saveRDS(output_ls,file = paste0("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/Interpolated_stations/",
                                fname))
}

#####################UHI max relations per station
#standard estimates of vars
# air_density=1.2
Cp<-1005 #specific heat capacity
R=287.058 # specific gas constant for dry air (J/(kg·K))
lambda=1.7

cabauw_parms<-readRDS("inst/Rdata/Meteo_params_cabauw.rds")
cabauw_P<-fread("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/Cabauw_meteo/Cabauw_P_10min.csv")
cabauw_P$IT_DATETIME<-as.POSIXct(cabauw_P$IT_DATETIME,format="%Y%m%d_%H%M00_000000")

cabauw_T<-fread("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/Cabauw_meteo/Cabauw_T_10min.csv")
cabauw_T$IT_DATETIME<-as.POSIXct(cabauw_T$IT_DATETIME,format="%Y%m%d_%H%M00_000000")
cabauw_PT<-merge(cabauw_P,cabauw_T,by="IT_DATETIME")

air_density<-calc_U(cabauw_PT$IT_DATETIME,(cabauw_PT$TOA.P_NAP_MSL_10*100)/(R*(cabauw_PT$TOT.T_DRYB_10+273.15)))
names(air_density)<-c("start","stop","rho")
cabauw_parms<-merge(cabauw_parms,air_density,by=c("start","stop"))
cabauw_parms$meteo<-(((cabauw_parms$S/(cabauw_parms$rho*Cp*24))*cabauw_parms$DTR^3)/cabauw_parms$U)^(1/lambda)



svf_grn<-readRDS("inst/Rdata/wunderground_svf_grn.rds")
df.svf_grn<-data.frame(svf_grn)
df.svf_grn<-df.svf_grn[df.svf_grn$Station.ID %in% c("IUTRECHT196",
                                                         "IUTRECHT242",
                                                         "IUTRECHT299",
                                                         "IUTRECHT376",
                                                         # "IUTRECHT432",
                                                         "IUTRECHT485"),]


for(i in 1:length(df.svf_grn$Station.ID)){
STN<-df.svf_grn$Station.ID[i]
stn<-readRDS(paste0("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/Interpolated_stations/",
                    STN,"_filtered.rds"))
wur_cabauw<-merge(x=stn$interpolated_time,y=cabauw_T,by.x="new_time",by.y="IT_DATETIME")
wur_cabauw<-wur_cabauw[complete.cases(wur_cabauw),]
UHI<-calc_UHImax(time = wur_cabauw$new_time,
                        Tcity = wur_cabauw$T_int,
                        Tref = wur_cabauw$TOT.T_DRYB_10)
UHI$Tcity<-calc_U(time=wur_cabauw$new_time,wur_cabauw$T_int)$W
UHI$Tref<-calc_U(time=wur_cabauw$new_time,wur_cabauw$TOT.T_DRYB_10)$W
# UHIcalc_stn<-cbind(cabauw_parms,UHIcalc)
UHI<-merge(UHI,cabauw_parms,by=c("start","stop"))
UHI$svf<-df.svf_grn$svf[i]
UHI$fveg<-df.svf_grn$grn[i]
UHI$Cp<-Cp
UHI$stn<-STN


p<-ggplot(UHI,aes(UHImeasured,meteo))+geom_point()+geom_abline()
ggsave(p,filename=paste0("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax/fig/",
                STN,".png"))
write.table(UHI,paste0("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax/",
                       STN,"_UHIparams.txt"),
            row.names = FALSE,
            col.names = TRUE,
            sep=",")
}

####################Select control days
meteo.df<-fread("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/Cabauw_meteo/rain_wind_rh_hour.csv")
meteo.df$IT_DATETIME<-as.POSIXct(meteo.df$IT_DATETIME,format="%Y%m%d_%H%M00_000000")
time<-meteo.df$IT_DATETIME
rh<-meteo.df$BGH.U
rain<-meteo.df$BGH.Q_RH
wind<-meteo.df$BGH.FH

days_subset<-uhi_sub(time=time,wind=wind,rain=rain,rh=rh)
######################
uhimax_files<-list.files("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax/",
                         full.names = TRUE,pattern=".txt")
uhimax_files<-lapply(uhimax_files,fread)
uhimax_Utrecht<-do.call("rbind",uhimax_files)

uhimax_Utrecht<-merge(days_subset,uhimax_Utrecht,by=c("start","stop"))
# uhimax_Utrecht<-uhimax_Utrecht[which(uhimax_Utrecht$Rain==TRUE),]
# uhimax_Utrecht<-uhimax_Utrecht[which(uhimax_Utrecht$Wind==TRUE),]
# uhimax_Utrecht<-uhimax_Utrecht[which(uhimax_Utrecht$rh==TRUE),]
uhimax_Utrecht<-uhimax_Utrecht[which(uhimax_Utrecht$Select==TRUE),]
uhimax_Utrecht<-uhimax_Utrecht[which(uhimax_Utrecht$Tref>17),]
uhimax_Utrecht<-uhimax_Utrecht[which(uhimax_Utrecht$stn %in% c("IUTRECHT196","IUTRECHT376","IUTRECHT299")),]

ggplot(uhimax_Utrecht,aes(UHImeasured,(2-svf-fveg)*meteo,colour=factor(stn))) +geom_point() +geom_abline() + xlim(0,10) +ylim(0,10)

ggplot(uhimax_Utrecht,aes(x=UHImeasured/(DTR*U),y=S/(rho*Cp*24))) +geom_point() +ylim(0,0.018) +xlim(0,0.80)

y<-uhimax_Utrecht$S/(uhimax_Utrecht$rho*Cp*24)
x<-uhimax_Utrecht$UHImeasured/(uhimax_Utrecht$DTR*uhimax_Utrecht$U)
df<-data.frame(y,x)
fit=nls(y ~ b*x^a,data=df,start=list(b=0.27,a=1.7))
# STN_sub<-df.svf_grn[which(df.svf_grn$System!="other"),]$Station.ID
# uhimax_Utrecht<-uhimax_Utrecht[uhimax_Utrecht$stn %in% c("IUTRECHT196",
#                                                          "IUTRECHT242",
#                                                          "IUTRECHT299",
#                                                          "IUTRECHT376",
#                                                          "IUTRECHT485"),]


p.meteo<-ggplot(uhimax_Utrecht,aes(UHImeasured,meteo,colour=factor(stn))) +geom_point() +geom_abline()
p.meteo
ggsave(p.meteo,filename = "/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax/fig/uhimax_measured.png")

#####################Fitting it ourselfs
library(lme4)
fits<-lmList(formula=UHImeasured~0+meteo | stn,data=uhimax_Utrecht) #Residual standard error: 0.04697533
coef_fits<-coef(fits)
names(coef_fits)<-"alpha"
coef_fits$stn<-rownames(coef_fits)
full_m<-merge(coef_fits,uhimax_Utrecht,by="stn")

intercept_station<-coef_fits[1]+full_m$svf[1]+full_m$fveg[1]

# fit=lm(alpha ~ svf+fveg,data=full_m)
# a1=coef(fit)[1]
# a2=coef(fit)[2]
# a3=coef(fit)[3]

ggplot(data=full_m,aes(fveg,alpha))+geom_point()+
  xlab("fveg")+
  ylab("alpha")+
  geom_abline()

ggplot(data=full_m,aes(svf,alpha))+geom_point()+
  xlab("svf")+
  ylab("alpha")+
  geom_abline()

ggplot(data=full_m,aes(2-svf-fveg,alpha,colour=factor(stn)))+geom_point()+
  xlab("2-SVF-fveg")+
  ylab("alpha")+
  geom_abline()

ggplot(uhimax_Utrecht,aes(UHImeasured,meteo,colour=factor(stn))) +geom_point() +geom_abline() +xlim(0,10)+ylim(0,10)

uhi_u<-ggplot(uhimax_Utrecht,aes(U,UHImeasured,colour=factor(stn))) +geom_point()
uhi_DTR<-ggplot(uhimax_Utrecht,aes(DTR,UHImeasured,colour=factor(stn))) +geom_point()
uhi_S<-ggplot(uhimax_Utrecht,aes(S,UHImeasured,colour=factor(stn))) +geom_point()
ggsave(uhi_u,filename = "/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax/fig/uhi_u.png")
ggsave(uhi_DTR,filename = "/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax/fig/uhi_DTR.png")
ggsave(uhi_S,filename = "/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/UHImax/fig/uhi_S.png")

# ggplot(uhimax_Utrecht,aes(UHImeasured,(a1-a2*svf-a3*fveg)*meteo,colour=factor(stn))) +geom_point() +geom_abline()
ggplot(uhimax_Utrecht,aes(UHImeasured,(2-svf-fveg)*meteo,colour=factor(stn))) +geom_point() +geom_abline() + xlim(0,10) +ylim(0,10)


uhi_u
uhi_DTR
uhi_S



uhimax_Utrecht$UHIcalc<-(a1-a2*uhimax_Utrecht$svf-a3*uhimax_Utrecht$fveg)*uhimax_Utrecht$meteo

ggplot(uhimax_Utrecht,aes(UHImeasured,UHIcalc,colour=factor(stn))) + geom_point() +geom_abline() + xlim(-2,6) +ylim(-2,6)
# df.svf_grn[df.svf_grn$Station.ID %in% c("IUTRECHT196",
#                                         "IUTRECHT242",
#                                         "IUTRECHT299",
#                                         "IUTRECHT376",
#                                         "IUTRECHT432", #on a 24m balcony
#                                         "IUTRECHT485"),][1:5]

##############gridded UHImax predictions
svf<-stack("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/grid_files_veg_svf/svf_utrecht_1m.grd")
fveg<-stack("/nobackup/users/dirksen/data/wunderground/svf_temperature_relevant_stations/grid_files_veg_svf/greenness_utrecht_smooth_500m.grd")
fveg<-projectRaster(fveg,crs=crs(svf))

svf<-resample(svf,fveg,method="bilinear")

st<-(2.6-svf-fveg)*uhimax_Utrecht$meteo[1]
