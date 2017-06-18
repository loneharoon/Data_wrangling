library(xts)
library(data.table)
library(gtools)

create_minutely <- function(){
  # this function is used to down sample data
  library(gtools)
  month = "default"
  parent <- "/Volumes/MacintoshHD2/Users/"
  dfiles <- mixedsort(list.files(paste0(parent,month,"/"),pattern = "*.csv"))
  lapply(dfiles, function(x){
    df <- fread(paste0(parent,month,"/",x),header = TRUE)
    df_xts <- xts(df$power,fasttime::fastPOSIXct(df$timestamp)-19800) # subtracting5:30 according to IST
    duplicate_rno <- which(duplicated(index(df_xts)))  #remvove duplicates
    if(length(duplicate_rno) > 0 ){
      df_xts <- df_xts[-duplicate_rno,] }
    sampled_df_xts <- resample_data_minutely(df_xts,1) # convert to minutely format
    stopifnot(length(unique(lubridate::second(sampled_df_xts)))==1) # ensures series end at 0 seconds
    write.csv(data.frame(timestamp=index(sampled_df_xts),power=round(coredata(sampled_df_xts),3)),file=paste0(parent,"minutely/",x),row.names = FALSE)
  })
}

resample_data_minutely <- function(xts_datap,xminutes) {
  #This function resamples input xts data to xminutes rate
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align IST hours
  align_data <- align.time(ds_data,xminutes*60) # aligning to x seconds
  rm(ds_data)
  return(align_data)
}

interpolate_missing_readings <- function(org_xts,samp_duration) {
  # function used to fill missing values by interpolation
  # input-  org_xts <- xts data, samp_duration: sampling duration of input data
  timerange = seq(start(org_xts),end(org_xts), by = samp_duration) # assuming original object is hourly sampled
  temp = xts(rep(NA,length(timerange)),timerange)
  complete_xts = merge(org_xts,temp)[,1]
  colnames(complete_xts) <- NULL
  return(complete_xts)
}