
library(readr)
# function to generate time-series based on data for each year in the UAE

#setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/Interactive_plots_R")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV")


NCMS_2013 <- read_csv("database_NCMS_2013_hourly.csv")
NCMS_2014 <- read_csv("database_NCMS_2014_hourly.csv")
NCMS_2015 <- read_csv("database_NCMS_2015_hourly.csv")
NCMS_2016 <- read_csv("database_NCMS_2016_hourly.csv")

# bind data together
NCMS <- rbind(NCMS_2013 ,NCMS_2014, NCMS_2015, NCMS_2016)

NCMS_2013_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box/database_NCMS_ 2013 _hourly_filtered.csv")
NCMS_2014_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box/database_NCMS_ 2014 _hourly_filtered.csv")
NCMS_2015_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box/database_NCMS_ 2015 _hourly_filtered.csv")
NCMS_2016_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box/database_NCMS_ 2016 _hourly_filtered.csv")

#DM <- DM_2013
NCMS_filtered <-rbind(NCMS_2013_filtered, NCMS_2014_filtered,NCMS_2015_filtered, NCMS_2016_filtered)





get_sites <- function(var) {
  NCMS_PM25 <- NCMS %>%
    filter(Pollutant == var) %>%
    distinct(Site, Latitude, Longitude)
  
# Return
NCMS_PM25
}
  
 

get_measurement_time_series <- function(station, pollutant) {

  # Import hourly data from several years
  
  # NCMS[sapply(NCMS,is.na)] = NA 
  

  NCMS <- NCMS %>%
    # mutate(date = mdy_hms(DateTime, tz = "UTC")) %>%
     mutate(date = DateTime) %>%
     dplyr:: select(date,
            Site,
            Pollutant,
            Value) %>%
    filter(Site == station)
  
  
  # replace NaN (not a Number with NA that is a missing value)
  # NCMS[sapply(NCMS,is.na)] = NA 
  NCMS_filtered <- NCMS_filtered %>%
    #  mutate(date = mdy_hms(DateTime, tz = "UTC")) %>%
    mutate(date = DateTime) %>%
    dplyr:: select(date,
                   Site,
                   Pollutant,
                   Value) %>%
    filter(Site == station)
  
  
  data_time <- NCMS %>%
    spread(Pollutant, Value)
  
  data_time_filtered <- NCMS_filtered %>%
    spread(Pollutant, Value)
  
  # Build timeseries for plots
  time_series <- data_frame_to_timeseries(data_time)
  time_series_filtered <- data_frame_to_timeseries(data_time_filtered)
  
  # Return
  #time_series
  #time_series_filtered
  # bind two time series together
  All_data <- cbind(time_series,time_series_filtered)
  
  #return
  
  All_data
}


# time_series_AlHamriyah <- get_measurement_time_series("AlHamriyah", pollutant)
# time_series_Kalba <- get_measurement_time_series("Kalba")
# time_series_Rashidyah <- get_measurement_time_series("Rashidyah")
# time_series_ELdErLyHouse <- get_measurement_time_series("ELdErLyHouse")
# time_series_AlJeer <- get_measurement_time_series("AlJeer")
# time_series_Burairat <- get_measurement_time_series("Burairat")
# time_series_AlQasimiyah <- get_measurement_time_series("AlQasimiyah")
# time_series_Ghalilah <- get_measurement_time_series("Ghalilah",pollutant)

############################################################################
############################################################################

# to create grouped interactive dygraphs

interactive_plot <- function(ts, station, group, pollu) {
  check_<-row.names(ts)
  
  # NO2
  if (!is.null(ts) & is.element(pollu, check_) ) {
    
    # Get colour vector
    colour_vector <- threadr::ggplot2_colours(45)
    
    if (pollutant == "NO<sub>2</sub>") {
      
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly NO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>%
        dyRangeSelector()
      
      
    }
    
    #SO2
    
    if (pollutant == "SO<sub>2</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly SO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    
    #CO
    
    if (pollutant == "CO") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly CO (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #O3
    
    
    if (pollutant == "O<sub>3</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly O<sub>3</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #PM10
    
    if (pollutant == "PM<sub>10</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #PM2.5
    
    if (pollutant == "PM<sub>2.5</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly PM<sub>2.5</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    
    #Lower Ambient Temperature
    
    if (pollutant == "Lower Ambient Temperature") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Temp. (<sup>o</sup>C)") %>% 
        dyRangeSelector()
    }
    
    #Upper Ambient Temperature
    
    if (pollutant == "Upper Ambient Temperature") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Temp. (<sup>o</sup>C)") %>% 
        dyRangeSelector()
    }
    #Barometric Pressure
    
    if (pollutant == "Barometric Pressure") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Atmo. Pressure (Pa)") %>% 
        dyRangeSelector()
    }
    #RelativeHumidity
    
    if (pollutant == "RelativeHumidity") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Humiduty (%)") %>% 
        dyRangeSelector()
    }
    #Wind Direction
    
    if (pollutant == "Wind Direction") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly (degree)") %>% 
        dyRangeSelector()
    }
    #Wind Speed 
    
    if (pollutant == "Wind Speed") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Speed (km hr<sup>-1</sup>)") %>% 
        dyRangeSelector()
    }
    #H2S
    
    if (pollutant == "H<sub>2</sub>S") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly H<sub>2</sub>S (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #Toluene
    
    if (pollutant == "Toluene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Toluene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #O_Xylene
    
    if (pollutant == "O_Xylene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly O_Xylene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #EthylBenzene
    
    if (pollutant == "EthylBenzene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly EthylBenzene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #mp_xylene_
    
    if (pollutant == "mp_xylene_") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly mp_xylene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #Benzene
    
    if (pollutant == "Benzene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Benzene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #Noise
    
    if (pollutant == "Noise") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      
      
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Hourly Noise (Dba)") %>% 
        dyRangeSelector()
    } 
    
    
    
    # Return
    plot
    
  }
  
}

        
# pollutant <- "NO2"
# plot <- interactive_plot(time_series_Ghalilah$NO2, station = "Ghalilah", group = pollutant)
# plot



interactive_map_index <- function(df) {

  # Map
  map <- leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>%
    addMarkers(data = df, lng = ~ Longitude, lat = ~ Latitude,
               popup = ~ Site, group = "Sites") %>%
    addPolygons(stroke = TRUE, data = shp_UAE,
                weight = 1.5, color = ~ colorNumeric(c("#a56e6e", "#7a7acc", "#FFFF00", "#ff0000", "#be68be", "#7fbf7f", "#008000", "#0000ff"), shp_UAE$ID_1)(ID_1),
                fillOpacity = 0.5,
                group = "shape_UAE") %>%
    addLayersControl(baseGroups = c("OpenStreetMap", "Toner", "Images"),
                     overlayGroups = c("Sites"))
  
  # Return
  map

}

# 

interactive_map <- function(df) {
  
  # Map
  map <- leaflet() %>%
    setView(lng = 55.9971, lat = 25.3302, zoom = 9) %>%  
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>%
    addMarkers(data = df, lng = ~ Longitude, lat = ~ Latitude,
               popup = ~ Site, group = "Sites") %>%
    addLayersControl(baseGroups = c("OpenStreetMap", "Toner", "Images"),
                     overlayGroups = c("Sites"))
  
  # Return
  map
  
}


