library(ggplot2)
library(scales)
visualize_dataframe_all_columns <- function(xts_data) {
  library(RColorBrewer)# to increase no. of colors
  library(plotly)
  # VISUALIZE SPECiFIC PORTION OF DATA
  #http://novyden.blogspot.in/2013/09/how-to-expand-color-palette-with-ggplot.html
  #dframe <- data_10min["2014-08-9"]
  dframe <- data.frame(timeindex=index(xts_data),coredata(xts_data))
  # dframe$dataid <- NULL ; dframe$air1 <-NULL ; dframe$use<- NULL ; dframe$drye1 <- NULL
  df_long <- reshape2::melt(dframe,id.vars = "timeindex")
  colourCount = length(unique(df_long$variable))
  getPalette = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount) # brewer.pal(8, "Dark2") or brewer.pal(9, "Set1")
  g <- ggplot(df_long,aes(timeindex,value,col=variable,group=variable))
  g <- g + geom_line() + scale_colour_manual(values=getPalette)
  ggplotly(g)
}

visualize_month_data_facet_form <- function(df,column_name){
  month_data <- df
  month_data <- month_data[,column_name]
  colnames(month_data) <- "power"
  #browser()
  month_data$day <- lubridate::day(index(month_data))
  month_data$time <- lubridate::hour(index(month_data)) * 60 + lubridate::minute(index(month_data))
  # df_long <- reshape2::melt(coredata(month_data),id.vars=c("time","day"))
  g <- ggplot(as.data.frame(coredata(month_data)),aes(time,power)) + geom_line() + facet_wrap(~day,ncol=7) 
  print(g)
}

visualize_dataframe_one_column_facet_form_month_wise <- function(xts_data,ncol) {
  # Using this function I visualise data month wise in different facets
  data <- xts_data/1000
  dframe <- data.frame(timeindex=index(data),coredata(data))
  df_long <- reshape2::melt(dframe,id.vars = "timeindex")
  df_long$month <- lubridate::month(df_long$timeindex)
  g <- ggplot(df_long,aes(timeindex,value,group=variable)) + facet_wrap(~month,ncol = ncol,scales="free_x") + geom_line()
  g <- g + labs(x= "Day of the month", y="Power (kW)") + scale_x_datetime(labels=scales::date_format("%d"))
  g
}


visualize_dataframe_one_column_facet_form_day_wise <- function(xts_data,ncol) {
  # Using this function I visualise data day wise in different facets
  data <- xts_data/1000
  dframe <- data.frame(timeindex=index(data),coredata(data))
  df_long <- reshape2::melt(dframe,id.vars = "timeindex")
  df_long$day <- lubridate::day(df_long$timeindex)
  g <- ggplot(df_long,aes(timeindex,value,group=variable)) + facet_wrap(~day,ncol = ncol,scales="free_x") + geom_line()
 # g <- g + labs(x= "Day", y="Power (kW)") + scale_x_datetime(labels=scales::date_format("%H"))
  g <- g + labs(x= "Day", y="Power (kW)") + scale_x_datetime(labels=scales::date_format("%H",tz="Asia/Kolkata"))
  g
}


visualize_dataframe_all_columns_facet_form <- function(xts_data,ncol) {
  data <- xts_data/1000
  dframe <- data.frame(timeindex=index(data),coredata(data))
  df_long <- reshape2::melt(dframe,id.vars = "timeindex")
  df_long$month <- lubridate::month(df_long$timeindex)
  g <- ggplot(df_long,aes(timeindex,value,col=variable,group=variable)) + facet_wrap(~month,ncol = ncol,scales="free_x") + geom_line()
  g <- g + labs(x= "Day of the month",y="Power (kW)") + scale_x_datetime(labels=date_format("%d"))
  g
}
