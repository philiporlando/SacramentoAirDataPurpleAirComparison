# created by Philip Orlando @ Sustainable Atmopsheres Research Lab
# PI Dr. Linda George
# 2018-04-27
# Comparing 3rd party PurpleAir sensors and reference sites in Sacramento
# Trying to determine if existing PurpleAir data can be reliable, and if we should avoid those grid cells when siting new sensors...


# load the necessary packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(readr
       ,ggplot2
       ,plyr
       ,dplyr
       ,broom
       ,reshape2
       ,tidyr
       ,stringr
       ,magrittr
       ,rlist
       ,gridExtra
)



read_airData <- function(fpath) {
  x <- read_csv(fpath)
  return(x)
  
}


pm25_path_hourly <- "./data/AirData/pm2_5/"
wind_path_hourly <- "./data/AirData/wind/"

pm25_files_hourly <- list.files(path = pm25_path_hourly, pattern = "\\.zip$",
                                all.files=FALSE, full.names = TRUE,
                                ignore.case = FALSE)


wind_files_hourly <- list.files(path = wind_path_hourly, pattern = "\\.zip$",
                                all.files=FALSE, full.names = TRUE,
                                ignore.case = FALSE)



## see what our memory limitations are:
cat(paste("Percent memory allocated:", round(100*(memory.size()/memory.limit()), 2), "%"))
gc()

pm25_master_hourly <- ldply(pm25_files_hourly, read_airData)

gc()
cat(paste("Percent memory allocated:", round(100*(memory.size()/memory.limit()), 2), "%"))

wind_master_hourly <- ldply(wind_files_hourly, read_airData)

sacra_pm <- pm25_master_hourly %>% filter(`State Code` == "06" & `County Code` == "067" | `County Code` == "061" | `County Code` == "113")
sacra_pm$datetime <- strptime(paste(sacra_pm$`Date Local`, sacra_pm$`Time Local`)
                                ,"%Y-%m-%d %H:%M:%S"
                                ,tz="UTC"
                              ) %>% as.POSIXct()

sacra_pm$key <- paste0(sacra_pm$`State Code`
                       ,"-"
                       ,sacra_pm$`County Code`
                       ,"-"
                       ,sacra_pm$`Site Num`
                       ,"-"
                       ,sacra_pm$`Parameter Code`
                       ,"-"
                       ,sacra_pm$`POC`
                       )


pm <- sacra_pm %>% select(c(`datetime`
                        ,key
                        ,`Site Num`
                        ,`Sample Measurement`
                        ,`Longitude`
                        ,`Latitude`)
                        ) %>%
  filter(`Site Num` == "0010" | `Site Num` == "0012" | `Site Num` == "0015")


sacra_wind <- wind_master_hourly %>% filter(`State Code` == "06" & `County Code` == "067" | `County Code` == "061" | `County Code` == "113")
sacra_wind$datetime <- strptime(paste(sacra_wind$`Date Local`, sacra_wind$`Time Local`)
                                ,"%Y-%m-%d %H:%M:%S"
                                ,tz="UTC"
                                ) %>% as.POSIXct()
# sacra_wind$key <- paste0(sacra_wind$`State Code`
#                        ,"-"
#                        ,sacra_wind$`County Code`
#                        ,"-"
#                        ,sacra_wind$`Site Num`
#                        ,"-"
#                        ,sacra_wind$`Parameter Code`
#                        ,"-"
#                        ,sacra_wind$`POC`
# )



wind <- sacra_wind %>% select(c(`datetime`
                        ,`Site Num`
                        ,`Parameter Name`
                        ,`Sample Measurement`
                        ,`Longitude`
                        ,`Latitude`
                        ) 
                      ) %>% filter(`Site Num` == "0010" | `Site Num` == "0012" | `Site Num` == "0015"
                                   ) %>% spread(key = `Parameter Name`
                                                ,value = `Sample Measurement`
                                   )

unique(wind$`Site Num`)
unique(pm$`Site Num`)

head(wind)
head(pm)

# 
# df <- inner_join(wind, pm, by = c("datetime"
#                                   ,"Site Num"
#                                   ,"Latitude"
#                                   ,"Longitude"
#                                   )
#                  )


df <- inner_join(wind, pm, by = c("datetime", "Site Num", "Latitude", "Longitude"))
head(df)

colnames(df) <- c("datetime"
                  ,"key"
                  ,"site"
                  ,"longitude"
                  ,"latitude"
                  ,"wd"
                  ,"ws"
                  ,"pm25"
                  )

head(df)

# we're getting duplicate wind data for different pm data... 
# are there more pm measurements per site per hour?
# probably since we're incorporating multiple FRM and non-FRM files...
# need to figure out the correct way to merge these data before moving forward with the analysis....


