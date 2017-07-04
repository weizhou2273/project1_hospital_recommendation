library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)
library(ggmap)
library(geosphere)
library(DT)
library(dplyr)
library(leaflet)

function(input,output,session) {
  
  # ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = -93.85, lat = 37.45, zoom =4)
  })
  
  # A reacctive expression that returns the set of hospital that are in bounds right not
  hospitalinbounds <- reactive({
    DRG = input$DRG1
    df = med_quality_address%>%
          filter(DRG.Definition==DRG)%>% 
          mutate(quality_index = (as.numeric(Hospital.overall.rating)*16+
                                    as.numeric(Mortality.national.comparison)/4*8+
                                    as.numeric(Safety.of.care.national.comparison)/4*6+
                                    as.numeric(Readmission.national.comparison)/4*3+
                                    as.numeric(Patient.experience.national.comparison)/4*1+
                                    as.numeric(Effectiveness.of.care.national.comparison)/4*1+
                                    as.numeric(Timeliness.of.care.national.comparison)/4*1),
                 hospital.type = ifelse(Hospital.Ownership %in% gov,"Government",
                                        ifelse(Hospital.Ownership %in% voluntary,"Voluntary Non-profit","Other"))
                 )%>%
          group_by(Provider.Name,
                   lon,lat,Provider.Street.Address,
                   DRG.Definition,
                   quality_index,
                   Hospital.overall.rating,
                   hospital.type,
                   Mortality.national.comparison,
                   Safety.of.care.national.comparison,
                   Readmission.national.comparison,
                   Patient.experience.national.comparison,
                   Effectiveness.of.care.national.comparison,
                   Timeliness.of.care.national.comparison
          )%>%
          summarise(total_discharge = sum(Total.Discharges),
                    avg_charge=round(sum(Total.Discharges*Average.Covered.Charges) /sum(Total.Discharges)),
                    avg_total_payment =round(sum(Total.Discharges*Average.Total.Payments) /sum(Total.Discharges)),
                    avg_total_Medicare_payment = round(sum(Total.Discharges*Average.Medicare.Payments) / sum(Total.Discharges)),
                    medicare_coverage_rate = round(sum(Total.Discharges*(Average.Medicare.Payments/Average.Total.Payments)) / sum(Total.Discharges),3),
                    avg_out_of_pocket = sum(Total.Discharges*(Average.Total.Payments-Average.Medicare.Payments)) / sum(Total.Discharges)
          )
    
    if (is.null(input$map_bounds))
      return(df[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(df,
           lat >= latRng[1] & lat <= latRng[2] &
           lon >= lngRng[1] & lon <= lngRng[2])
     })
  # Create scatter plot and add to sidebar  
  output$scatterplot <- renderPlot({
    x = hospitalinbounds()[,input$x_axis]
    y = hospitalinbounds()[,input$y_axis]
    if (nrow(hospitalinbounds())==0)
      return(NULL)
    
    print(ggplot(hospitalinbounds(),aes(x=x,y=y,color=hospital.type,fill = hospital.type))+
            geom_point(col = '#00b4dd')+
            geom_smooth(aes(method="lm",se=TRUE))) 
    print(head(hospitalinbounds()))
    })

  
  # Creat boxplot and add to sidebar
  output$boxplot <- renderPlot({
    
    x_axis <-as.factor(hospitalinbounds()[,input$x_axis_bp])
    print(x_axis)
    y_axis <-as.numeric(hospitalinbounds()[,input$y_axis_bp])
    if (nrow(hospitalinbounds())==0)
      return(NULL)
    
  print(ggplot(hospitalinbounds(),aes(x_axis,
                                      y_axis,
                                      fill = Hospital.overall.rating)) +
    geom_boxplot()+
    guides(fill=FALSE))
  })
  
  
  # add circle layer to map  #####
  observe ({
    
    
    if(nrow(hospitalinbounds()) > 0) {
      isolate({
        data <- data.frame(hospitalinbounds())
        colorBy <- input$color
        sizeBy <- input$size
        data$rad <- data[,sizeBy]/max(data[,sizeBy])*150000
        print(data$rad)
        pal <- colorFactor("RdYlGn", domain =NULL)

        leafletProxy("map") %>%
          clearShapes() %>%
          addCircles(data = data,
                     lng = ~lon, lat = ~lat, weight = 1,
                     radius = ~rad,
                     # popup =~Provider.Name,
                     color = pal(data[,colorBy])) %>%
          clearControls() %>%
          addLegend("bottomleft",pal = pal, values =data[,colorBy],
                    title = colorBy,
                    opacity = 1
          )
      })
    }
    
    })

  #show a popup at the given location
  showhospitalpopup <- function(lat,lng){
    selectedhospital <- med_by_hospital[med_by_hospital$lat==lat & med_by_hospital$lon==lng,]
    print(selectedhospital)
    #popup content edit#############
    content <- as.character(tagList(
      tags$h4(selectedhospital$Provider.Name),
      sprintf("%s, %s %s"   ,selectedhospital$Provider.City,
              selectedhospital$Provider.State,
              selectedhospital$Provider.Zip.Code), tags$br(),
      tags$h4("Overall Rating:", selectedhospital$Hospital.overall.rating,"/ 5"),
                       sprintf("Total inpatient discharge (2011-2014): %s patients", as.integer(selectedhospital$total_discharge)),tags$br(),
                       sprintf("Average hospital charge: $%s", as.integer(selectedhospital$avg_total_Medicare_payment)),tags$br(),
                       sprintf("Average total payment: $%s" , as.integer(selectedhospital$avg_total_payment),tags$br(),
                       sprintf("Average total Medicare payment: $%s", as.integer(selectedhospital$avg_total_Medicare_payment)))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }

  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    print(event)
    if (is.null(event))
      return()

    isolate({
      showhospitalpopup(event$lat, event$lng)
    })
  })

#Tab "PICK YOUR HOSPITAL"##########################################################################################

  
  ### select hospitals within x miles of type in location
  hospitalincycle <- reactive({
    radius_mile= input$radius_mile
    ur_location = input$ur_location
    ur_latlon = geocode(ur_location,output="latlon")
    DRG = input$DRG
    quality_concern = input$quality_concern
    
    # radius_mile=  eventReactive(input$search, {
    #   runif(input$radius_mile)
    # })
    # 
    # ur_latlon = eventReactive(input$search, { geocode(input$ur_location,output="latlon")})
    # DRG = eventReactive(input$search, {
    #   runif( input$DRG) })
    # quality_concern = eventReactive(input$search, {
    #   runif( input$quality_concern)})
    
    select_hospital_ID = c()
    for(i in 1:nrow(med_by_hospital)){
      if (distm(ur_latlon,
                c(med_by_hospital$lon[i], med_by_hospital$lat[i]), 
                fun = distHaversine)[,1] / 1609 <= radius_mile) {
        select_hospital_ID = c(select_hospital_ID,med_by_hospital$Provider.Id[i])
      } 
    }

   df = subset(med_quality_address,
           Provider.Id %in% select_hospital_ID)%>%
      filter(DRG.Definition==DRG & Hospital.overall.rating %in% quality_concern)%>%
      mutate(quality_index = (as.numeric(Hospital.overall.rating)*16+
                                as.numeric(Mortality.national.comparison)/4*8+
                                as.numeric(Safety.of.care.national.comparison)/4*6+
                                as.numeric(Readmission.national.comparison)/4*3+
                                as.numeric(Patient.experience.national.comparison)/4*1+
                                as.numeric(Effectiveness.of.care.national.comparison)/4*1+
                                as.numeric(Timeliness.of.care.national.comparison)/4*1))%>%
      group_by(Provider.Name,
               lon,lat,Provider.Street.Address,
               DRG.Definition,
               quality_index,
               Hospital.overall.rating,
               Mortality.national.comparison,
               Safety.of.care.national.comparison,
               Readmission.national.comparison,
               Patient.experience.national.comparison,
               Effectiveness.of.care.national.comparison,
               Timeliness.of.care.national.comparison
               )%>%
      summarise(total_discharge = sum(Total.Discharges),
                avg_charge=round(sum(Total.Discharges*Average.Covered.Charges) /sum(Total.Discharges)),
                avg_total_payment =round(sum(Total.Discharges*Average.Total.Payments) /sum(Total.Discharges)),
                avg_total_Medicare_payment = round(sum(Total.Discharges*Average.Medicare.Payments) / sum(Total.Discharges)),
                medicare_coverage_rate = round(sum(Total.Discharges*(Average.Medicare.Payments/Average.Total.Payments)) / sum(Total.Discharges),3),
                avg_out_of_pocket = sum(Total.Discharges*(Average.Total.Payments-Average.Medicare.Payments)) / sum(Total.Discharges)
            )

    totalqualityMedian = median(df$quality_index)
    totalcostMedian = median(df$avg_out_of_pocket)
    df = mutate(df, quality_comparison = ifelse(quality_index >= totalqualityMedian, 'Higher quality', 'Lower quality'),
                    cost_comparison = ifelse(avg_out_of_pocket >= totalcostMedian, 'Higher cost', 'Lower cost'),
                    quality_cost= paste(quality_comparison,",",cost_comparison)
                )
    
    return(df)
  })
  
  


 
  #map2 for hospital recommendation tab 
  # Create the map
  output$map2 <- renderLeaflet({
    ur_latlon = geocode(input$ur_location,output="latlon")
    leaflet() %>% 
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = ur_latlon$lon, lat = ur_latlon$lat,zoom =12)
  })
  
  observe ({

    print(nrow(hospitalincycle()))
    leafletProxy("map2", session) %>%
    clearMarkers()%>%
   # clearMarkerClusters()%>%
    addAwesomeMarkers(data = hospitalincycle(),
               lng = ~lon, lat = ~lat,
                      icon=hospitalicon[hospitalincycle()$quality_cost],
                      label = ~as.character(paste(Provider.Name,":",quality_cost))#,
                      #clusterOptions = markerClusterOptions()
                     )
    # print(hospitalincycle()$quality_cost)
    # print(icons)
  })

##################################################################################################

# DT show hospital data  
  output$hospital_in_cycle = renderDataTable({hospitalincycle()[,'Provider.Name']},
                                                  options = list(lengthMenu = c(5, 10, 15), 
                                                                 pageLength = 5),
                                             selection = 'single'   
                                            )



#click DT row, popup in map
  observe({
    event <- input$hospital_in_cycle_rows_selected
    content1 <- as.character(tagList(
      tags$h4(hospitalincycle()[event,"Provider.Name"]),
      sprintf("Quality Comparison: %s",hospitalincycle()[event,"quality_comparison"]), tags$br(),
      sprintf("Cost Comparison: %s",hospitalincycle()[event,"cost_comparison"]), tags$br(),
      tags$h4("Overall Rating:", hospitalincycle()[event,"Hospital.overall.rating"],"/ 5"),
      sprintf("Total inpatient discharge (2011-2014): %s patients", as.integer(hospitalincycle()[event,"total_discharge"])),tags$br(),
      sprintf("Average total payment: $%s" , as.integer(hospitalincycle()[event,"avg_total_payment"])),tags$br(),
      sprintf("Average total Medicare payment: $%s", as.integer(hospitalincycle()[event,"avg_total_Medicare_payment"]))
      ))

    if (is.null(event))
      return()
    isolate({
      leafletProxy("map2") %>% 
        clearPopups()%>%
        addPopups(as.numeric(hospitalincycle()[event,"lon"]),
                  as.numeric(hospitalincycle()[event,"lat"]),
                  content1)
      })
  })

  
  
  observe({
    leafletProxy("map2") %>% clearPopups()
    event2 <- input$map2_marker_click
    if (is.null(event2))
      return()
    
    isolate({
      showhospitalpopup(event2$lat, event2$lng)
    })
  })
  
  output$out_of_pocket_info<- renderInfoBox({
    s=input$hospital_in_cycle_rows_selected
    if(is.null(s))
      return()
    isolate({
      infoBox(
        "Out of Pocket",
        paste("$",round(hospitalincycle()[s,"avg_out_of_pocket"])),
        icon = icon("usd"),
        color = ifelse(hospitalincycle()[s,"cost_comparison"]=="Lower cost", "olive","orange")
      )
    })
  })
  

  output$quality_index <- renderInfoBox({
    s=input$hospital_in_cycle_rows_selected
    if(is.null(s)) 
      return()
    isolate({
      infoBox(
        "Quality index",
        paste(round(hospitalincycle()[s,"quality_index"])," / 100"),
        icon = icon("star",lib = "glyphicon"),
        color =ifelse(hospitalincycle()[s,"quality_comparison"]=="Lower quality","orange","olive")
      )
    })
  }) 
 

}
  
  
  
