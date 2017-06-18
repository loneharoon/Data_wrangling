create_minutely <- function(){
  library(gtools)
  month = "default"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/IIITD_Jan14_Dec15/"
  dfiles <- mixedsort(list.files(paste0(parent,month,"/"),pattern = "*.csv"))
  #dfiles <- dfiles[2:length(dfiles)] # removing first first that represents aggregate
  
  lapply(dfiles, function(x){
    df <- fread(paste0(parent,month,"/",x),header = TRUE)
    #df_xts <- xts(df$power, as.POSIXct(df$timestamp,tz="Asia/Kolkata",origin = "1970-01-01"))
    df_xts <- xts(df$power,fasttime::fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    #remvove duplicates
    duplicate_rno <- which(duplicated(index(df_xts)))
    if(length(duplicate_rno) > 0 ){
      df_xts <- df_xts[-duplicate_rno,] }
    sampled_df_xts <- resample_data_minutely(df_xts,1) # convert to hourly format
    stopifnot(length(unique(lubridate::second(sampled_df_xts)))==1) # ensures series end at 0 seconds
    #duplicate_rno <- which(duplicated(index(sampled_df_xts))) # ensure no duplicates are there
    #sampled_df_xts <- sampled_df_xts[-duplicate_rno,]
    write.csv(data.frame(timestamp=index(sampled_df_xts),power=round(coredata(sampled_df_xts),3)),file=paste0(parent,"year2016_data","/minutely/",x),row.names = FALSE)
  })
}

resample_data_minutely <- function(xts_datap,xminutes) {
  #downsampled data
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
  # align data to nearest time boundary
  # browser()
  align_data <- align.time(ds_data,xminutes*60) # aligning to x seconds
  rm(ds_data)
  return(align_data)
}