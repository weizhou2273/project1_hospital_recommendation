
library(rsconnect)
library(scatterD3)
library(leaflet)
library(ggplot2)
library(dplyr)

### import hospital lon and lat data 
hospital_address1 = readRDS("hospital_address1.RDS")
hospital_quality = read.csv("hospital quality rating.csv")
med = readRDS("med.RDS")
med_by_hospital = readRDS("med_by_hospital.RDS")
med_quality_address = readRDS("med_quality_address.RDS")


###### assign value to options in UI select box######
state <- sort(unique(med_by_hospital$Provider.State))

hospital_rating <- c(1,2,3,4,5)


year<- c("2011"=2011,
         "2012"=2012,
         "2013"=2013,
         "2014"=2014)

color <- c("Hospital overall quality rating" = "Hospital.overall.rating",
           "Hospital type" = "hospital.type")
size <- c(
           "Total inpatient discharge (2011-2014)" = "total_discharge",
           "Average hospital charge" = "avg_charge",
           "Average total payments" = "avg_total_payment",
           "Average Medicare payments" = "avg_total_Medicare_payment")

DRG <- unique(med$DRG.Definition)

quality_concern <- c("1","2","3","4","5")

x_axis_bp <- c("Hospital overall quality rating" = "Hospital.overall.rating",
           "Hospital type" = "hospital.type")
#########################################

hospitalicon <- awesomeIconList(
"Higher quality , Higher cost" = makeAwesomeIcon(markerColor = 'lightgreen',icon = 'heart',library = "glyphicon"),
"Higher quality , Lower cost" = makeAwesomeIcon(markerColor = 'green',icon = 'heart',library = "glyphicon"),
"Lower quality , Higher cost"= makeAwesomeIcon(markerColor = 'red',icon = 'heart',library = "glyphicon"),
"Lower quality , Lower cost" = makeAwesomeIcon(markerColor = 'lightred',icon = 'heart',library = "glyphicon")

)

x_axis =c(
  "Average hospital charge" = "avg_charge",
  "Average total payments" = "avg_total_payment",
  "Average Medicare payments" = "avg_total_Medicare_payment",
  "Medicare coverage rate" = "medicare_coverage_rate"
)

y_axis =c(
  "Average hospital charge" = "avg_charge",
  "Average total payments" = "avg_total_payment",
  "Average Medicare payments" = "avg_total_Medicare_payment",
  "Medicare coverage rate" = "medicare_coverage_rate"
)

gov <- c("Government - Hospital District or Authority",
         "Government - State",
         "Government - Local",
         "Government - Federal")
voluntary <- c("Voluntary non-profit - Private",
               "Voluntary non-profit - Other",
               "Voluntary non-profit - Church")


#DT column
keepcolumn = c("Provider.Name","total_discharge",
               "avg_charge","avg_total_payment", 
               "avg_total_Medicare_payment",                                          
               "Hospital.overall.rating")

  
# create med_by_hospital dataset
  med_by_hospital = med%>%
    group_by(Provider.Id)%>%
    summarise(total_discharge = sum(Total.Discharges),
              avg_charge=round(sum(Total.Discharges*Average.Covered.Charges) /sum(Total.Discharges)),
              avg_total_payment =round(sum(Total.Discharges*Average.Total.Payments) /sum(Total.Discharges)),
              avg_total_Medicare_payment = round(sum(Total.Discharges*Average.Medicare.Payments) / sum(Total.Discharges))
    )
  #merger med_by_hospital with lon and lat data
  med_by_hospital= merge(x=med_by_hospital,y=hospital_address1,by="Provider.Id",all.x = TRUE)
  med_by_hospital=merge(x=med_by_hospital,y=hospital_quality[,c(1,9:28)],by.x="Provider.Id",by.y="Provider.ID",All.x=TRUE)
  med_by_hospital = med_by_hospital[!is.na(med_by_hospital$lon),]
  

  ##Generate your location icon
  
  
  
  # # Create hospital leaflet map############################################################
  #   # Extraxt hospital Lon and Lat data from google map
  #             hospital_add=unique(med%>%
  #               select(Provider.Id,Provider.Name,Provider.City,Provider.Street.Address,Provider.State,Provider.Zip.Code))
  # 
  #             hospital_add$lon = rep(NA,nrow(hospital_add))
  #             hospital_add$lat = rep(NA,nrow(hospital_add))
  # 
  #             for (i in 1:nrow(hospital_address1)) {
  #               if(is.na(hospital_address1$lat[i])||is.na(hospital_address1$lon[i])){
  #                 address <- paste(hospital_address1$Provider.Street.Address[i],
  #                                  hospital_address1$Provider.City[i],
  #                                  hospital_address1$Provider.State[i],
  #                                  hospital_address1$Provider.Zip.Code[i],
  #                                  sep =",")
  #                 latlon = geocode(address,output = "latlon")
  #                 hospital_address1[i,"lon"] = latlon$lon
  #                 hospital_address1[i,"lat"] = latlon$lat
  #                 print(paste("added:",hospital_address1$Provider.Name[i]))
  #               } else {
  #                 print(paste("skipped:",hospital_address1$Provider.Name[i]))
  #               }
  #             }
  # 
  #             saveRDS(hospital_address1,"C:\\Users\\William Zhou\\Desktop\\shiny app\\hospital_address1.RDS")
  # 
  # 
  

  
  