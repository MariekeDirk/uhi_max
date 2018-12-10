#'@title Clean and interpolate wunderground data
#'@description Filters out non-realistic low values downloaded using `download_time_seq`, creates UTC timestamps
#'from which a 10min sequence is created. The following filters are applied:
#'
#'\itemize{
#'\item Temperatures lower than -40 are unrealistic in the Netherlands and are excluded (set to NA)
#'\item Similarly temperatures above 45 are also excluded (set to NA)
#'\item In case there are more than 8 identical measurements in a row the value is set to NA
#'}
#'
#'If the gap between the measurements is more than 1 hour values
#'are not interpolated.
#'@param Tunit Temperature unit, since December 2018 the Wunderground data is in Farenheid.
#'@param datafile Downloaded wunderground data
#'@param fname Filename to write the filtered data
#'@importFrom data.table fread rleid setorder data.table
#'@importFrom stats approx
#'@importFrom lubridate round_date
#'@export
clean_wunderground<-function(Tunit="Farenheid",
                             datafile="C:/Users/marie/OneDrive/Documenten/uhi_max/inst/Wunderground/IUTRECHT23/daydata_rbind_2009-01-01_until_2018-11-27.txt",
                             fname="C:/Users/marie/OneDrive/Documenten/uhi_max/inst/Wunderground/Filtered/IUTRECHT23_filtered.rds"){
  wur_stations_data<-fread(datafile)

message("Preparing input data")
  TC<-wur_stations_data[[1]] #select the temperatures
  if(Tunit=="Farenheid"){TC<-(TC-32)/1.8}
  wur_filtered<-ifelse(TC < -40 | TC > 45,NA,TC) #filter the temperature


  #Also filter the duplicates in the data! If TRUE for more than 8 values in a row, remove the sequence
  dub<-sequential.duplicated(wur_filtered)
  DT <- data.table(dub, runid = rleid(dub))
  DT_stat <- DT[,.(length = .N, position = .I[1], type=dub[1]), by = runid]
  DT_to_filter<-DT_stat[which(as.numeric(DT_stat$length)>8 & DT_stat$type==TRUE),]
  from<-DT_to_filter$position
  to<-DT_to_filter$position+DT_to_filter$length-1
  I_out<-unlist(mapply(seq,from,to,by=1,SIMPLIFY = TRUE))

  wur_filtered[I_out]<-NA

  TS<-wur_stations_data[[2]] #select the timestamps
  ts_filtered<-as.POSIXct(TS,format="%Y-%m-%d %H:%M:%S")

  df<-data.frame("T"=wur_filtered,"Timestamp_UTC"=ts_filtered)

message("Find the gaps in the time series")
    df_complete<-df[complete.cases(df),]
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
    I_start<-continuous_mm$position[Irel]
    I_stop<-continuous_mm$position+(continuous_mm$length-1)

message("Approximating new values")
    #Interpolating new values using a linear method
    interpolate_period<-function(start,stop,I_start,I_stop){
      t_sq<-seq(from=start,to=stop,by="10 mins")
      T_new<-approx(x=df_complete$Timestamp_UTC[I_start:I_stop],
                    y=df_complete$T[I_start:I_stop],xout=t_sq,
                    rule=1,ties = "ordered",method="linear")
      output<-data.frame(t_sq,T_new$y)
      names(output)<-c("new_time","T_int")
      return(output)
    }

    int_time<-mapply(interpolate_period,
                     start=t_start,stop=t_stop,
                     I_start=I_start,I_stop=I_stop,
                     SIMPLIFY = FALSE)
    df_int_time<-do.call("rbind",int_time)

    output_ls<-list("filtered_obs"=df_complete,
                    "continuous_measurements"=continuous_mm,
                    "interpolated_time"=df_int_time)
    saveRDS(output_ls,file = fname)
  }

#' Sequential duplicated from FedData package
#' @title Get a logical vector of which elements in a vector are sequentially duplicated.
#' @description Get a logical vector of which elements in a vector are sequentially duplicated.
#' @param x An vector of any type, or, if rows, a matrix
#' @param rows Is x a matrix?
#' @return A logical vector of the same length as x.
sequential.duplicated<-function (x, rows = F)
{
  if (!rows) {
    duplicates <- c(FALSE, unlist(lapply(1:(length(x) - 1),
                                         function(i) {
                                           duplicated(x[i:(i + 1)])[2]
                                         })))
  }
  else {
    duplicates <- c(FALSE, unlist(lapply(1:(nrow(x) - 1),
                                         function(i) {
                                           duplicated(x[i:(i + 1), ])[2]
                                         })))
  }
  return(duplicates)
}
