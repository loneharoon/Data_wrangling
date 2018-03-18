library(xts)
library(data.table)
library(gtools)
library(dplyr)

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

resample_occupancy_minutely <- function(xts_datap,xminutes) {
  #This function resamples input xts data to xminutes rate but it takes max as compared to common mean function
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= max) # subtracting half hour to align IST hours
  align_data <- align.time(ds_data,xminutes*60) # aligning to x seconds
  rm(ds_data)
  return(align_data)
}

fill_missing_readings_with_NA <- function(org_xts,samp_duration) {
  # function used to fill missing values with NA
  # input-  org_xts <- xts data, samp_duration: sampling duration of input data
  timerange = seq(start(org_xts),end(org_xts), by = samp_duration) # assuming original object is hourly sampled
  temp = xts(rep(NA,length(timerange)),timerange)
  #browser()
  complete_xts = merge(org_xts,temp)[,1:NCOL(org_xts)]
  colnames(complete_xts) <- NULL
  return(complete_xts)
}

create_year_data_NA <- function(year,sampling){
  # this function creates dummy timeseries filled with NA values of given year at provided sammping rate
 # year = '2016'
  #sampling = '15 mins'
  timerange = seq(as.POSIXct(paste0(year,'-01-01')),as.POSIXct(paste0(year,'-12-31 23:59:59')), by = sampling) # assuming original object is hourly sampled
  temp = xts(rep(NA,length(timerange)),timerange)
  return(temp)
}


create_NA_timeseries_data <- function(string_start,string_end,sampling){
  # this function creates dummy timeseries filled with NA values at provided sammping rate
  #sampling = '15 mins'
  timerange = seq(as.POSIXct(string_start),as.POSIXct(string_end), by = sampling) # assuming original object is hourly sampled
  temp = xts(rep(NA,length(timerange)),timerange)
  return(temp)
}

create_NA_timeseries_tibble <- function(string_start,string_end,sampling){
  # this function creates dummy timeseries tibble filled with NA values at provided sammping rate
  #sampling = '15 mins'
  timerange = seq(as.POSIXct(string_start),as.POSIXct(string_end), by = sampling) # assuming original object is hourly sampled
  temp = xts(rep(NA,length(timerange)),timerange)
  tbl <- myxts_tbl(temp)
  return(tbl)
}


resample_tbl_data_minutely <- function(tbl_datap,xminutes) {
  #This function resamples input xts data to xminutes rate
  xts_datap <- tbl_datap %>% mytbl_xts()
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align IST hours
  align_data <- align.time(ds_data,xminutes*60) # aligning to x seconds
  rm(ds_data)
  tbl <- myxts_tbl(align_data)
  print("---Done---")
  return(tbl)
}

mytbl_xts <- function(tbldata){
  library(xts)
  # converts tibble object to xts object
  if(any(class(tbldata$timestamp) == "POSIXct")){
    xtsob <- xts(as.data.frame(tbldata[,2:NCOL(tbldata)]),tbldata$timestamp)
    return(xtsob)
  } else{
    stop("INput timebased column")
  }
}

myxts_tbl <- function(xtsdata){
  mytib <- data.frame(date = index(xtsdata), coredata(xtsdata)) %>% tbl_df()
  colnames(mytib)[1] <- 'timestamp'
  return(mytib)
}