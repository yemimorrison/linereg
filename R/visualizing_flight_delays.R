#' Visualizing the mean delays of the airports
#'
#' @return plot of the mean delays of different airports by longititude and latitude
#' @export
#' @import dplyr
#' @import ggplot2
#' @import nycflights13
#' @importFrom  plotly ggplotly
#' @importFrom dplyr %>%


Visualizing_flight_delay<-function(){
  # removing rows having NA values based on columns
  df_flights<-flights
  df_airports<-airports
  df_flights<-df_flights[!is.na(df_flights$dep_delay),]
  df_flights<-df_flights[!is.na(df_flights$arr_delay),]
  
  #calculating delays of destination airport
  requireNamespace('dplyr')
  fl_dest<- df_flights %>%  group_by(dest) %>%
     summarise(avg_delay_dest=mean(arr_delay+dep_delay))
  colnames(fl_dest)<-c('airports','mean_delay')
  fl_dest
  
  #calculating delays of arrival airport
  fl_origin<-df_flights %>%  group_by(origin) %>%
    summarise(avg_delay_origin=mean(dep_delay))
  colnames(fl_origin)<-c('airports','mean_delay')
  fl_origin
  
  #rbinding all the airports mean delay to create airports_mean_delay dataset
  airports_mean_delay<-rbind(fl_dest,fl_origin)
  airports_mean_delay
  
  #merging df_airports dataset and airports_mean_delay
  merging<- left_join(df_airports,airports_mean_delay,by=c('faa'='airports'))
  
  merging<-merging[!is.na(merging$mean_delay),]
  colnames(merging)[1]<-c('airport')
  
  
  #hover over the points to see mean delay
  requireNamespace('ggplot2')
  requireNamespace('plotly')
  p<- ggplot(merging, aes(label1=mean_delay))+
     geom_point( aes(x=lat,y=lon,color=airport))+
     theme(legend.position = 'none')+ xlab('latitude')+ ylab('longitude')+
     ggtitle('Mean delay of flights for different airports by longitude and latitude.')
  return( ggplotly(p))
}

#Visualizing_flight_delay()
