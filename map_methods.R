library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(sqldf)
library(leaflet)
library(latticeExtra)
library(ggplot2)
library(leaflet.extras)

icon_function <- function(type, color){
  if(type=='co' && color=='green'){
    return(makeIcon(
      iconUrl = "./www/contract_green.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } else if(type=='t' && color=='green'){
    return(makeIcon(
      iconUrl = "./www/tires.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } else if(type=='o' && color=='green'){
    return(makeIcon(
      iconUrl = "./www/auto_green.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } else if(type=='c' && color=='green'){
    return(makeIcon(
      iconUrl = "./www/einzelteil_green.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } else if(type=='c' && color=='red'){
    return(makeIcon(
      iconUrl = "./www/einzelteil_red.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } else if(type=='co' && color=='red'){
    return(makeIcon(
      iconUrl = "./www/contract_red.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } else if(type=='t' && color=='red'){
    return(makeIcon(
      iconUrl = "./www/tires_red.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } else if(type=='o' && color=='red'){
    return( makeIcon(
      iconUrl = "./www/auto.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    ))
  } 
}

render_profile_map <- function(ort,icon_profile,producer){
  setDT(ort)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
  return(
  # leaflet----
  leaflet() %>%
    addProviderTiles("OpenStreetMap.DE",
                     options = providerTileOptions(noWrap = TRUE))%>%
    addMarkers(ort, 
               lat = as.numeric(ort$Coordinates_Sent2),
               lng = as.numeric(ort$Coordinates_Sent1),
               icon =  icon_profile,
               popup = paste(sep = "<br/>",
                             sprintf("<b>Producer: %s</b>",producer),
                             sprintf("Coordinates: %s",ort$Coordinates_Sent)),
               label = as.character(paste0("Producer: ", sep = " ", producer))
    ))
}

render_complex_map_component <- function(ort, ort_red, ort_red_line){
  # Input:
  # ort contains all data of NOT faulty part producers -> green icons  
  # ort_red contains all data of faulty part producers -> red icons
  # ort_red_line contains all data of faulty part producers -> red line
  #        draws all lines green and wrong ones again with red
  
  # Output:
  # leaflet data to be plotted on map
  
  # Assumption: 
  # 1) ort and ort_red are not empty at the same time
  # 2) ort and ort_red and ort_red_line are not empty at the same time
  # -> always at least two producer on the map and one line in between (color irrelevant)
  if(dim(ort)[1]==0 && dim(ort_red)[1]==0){
    return('Error')
  }
  
  # 4 cases to consider:
  #     1) ort empty (no producers with correct productions)
  #     1.b) ort_red empty (no producers with correct productions)
  #     2) ort_red_line empty (all transports successful)
  #     3) ort_red_line empty (all transports successful) & ort empty (no producers with correct productions)
  #     3.b) ort_red_line empty (all transports successful) & ort_red empty (no producers with correct productions)
  #     4) nothing empty
  
  

  if(dim(ort)[1]==0 && dim(ort_red_line)[1]!=0){
    #-------------CASE 1) ----------
      setDT(ort_red_line)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
      setDT(ort_red_line)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
      a=ort_red_line[,c(19,20)] # Einzelteil Werk
      b=ort_red_line[,c(21,22)] # Componenten Werk
      a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
      b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
      poly_df_ab_red_line <- sqldf("
                                   SELECT * FROM b
                                   UNION ALL
                                   SELECT * FROM a
                                   ORDER BY sequence")
      
      #color stuff green-----
      setDT(ort_red)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
      setDT(ort_red)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
      a_green=ort_red[,c(19,20)] # Einzelteil Werk
      b_green=ort_red[,c(21,22)] # Componenten Werk
      a_green$sequence <- c(sequence = seq(2, length.out = nrow(a_green), by=2))
      b_green$sequence <- c(sequence = seq(1, length.out = nrow(b_green), by=2))
      
      
      #polydf green----
      poly_df_ab_green <- sqldf("
                                SELECT * FROM b_green
                                UNION ALL
                                SELECT * FROM a_green
                                ORDER BY sequence")
      
      
      # content for markers: ----
      
      content_T_green <- paste(sep = "<br/>",
                               sprintf("<b>Faulty: %s</b>",ort_red$Parts_Faulty),
                               sprintf("<b>Producer: %s</b>",ort_red$Producer),
                               sprintf("Coordinates: %s",ort_red$Coordinates_Sent),
                               sprintf("Build on: %s",ort_red$Produced_Date),
                               sprintf("Sent Date: %s",ort_red$Date_Sent)
      )
      content_K_green <- paste(sep = "<br/>",
                               sprintf("<b>Producer: %s</b>",ort_red$Producer_Received),
                               sprintf("Coordinates: %s",ort_red$Coordinates_Received)
      )
      return(
        # leaflet----
        leaflet(ort_red) %>%
          #Add Polylines----
        addProviderTiles("OpenStreetMap.DE",
                         options = providerTileOptions(noWrap = TRUE))%>% 
          
          addPolylines(
            data = poly_df_ab_green,
            lat = as.numeric(poly_df_ab_green$Coordinates_Received2), 
            lng = as.numeric(poly_df_ab_green$Coordinates_Received1),
            color = 'green',
            weight = 3,
            opacity = 3
          )%>%
          addPolylines(
            data = poly_df_ab_red_line,
            lat = as.numeric(poly_df_ab_red_line$Coordinates_Received2), 
            lng = as.numeric(poly_df_ab_red_line$Coordinates_Received1),
            color = 'red',
            weight = 3,
            opacity = 3
          )%>%
          #AddMarkers----
        addMarkers(ort_red, 
                   lat = as.numeric(ort_red$Coordinates_Sent2),
                   lng = as.numeric(ort_red$Coordinates_Sent1),
                   icon =  icon_function('t','red'),
                   popup = content_T_green,
                   label = as.character(paste0("Part Producer: ", sep = " ", ort_red$Producer))
        )%>%
          addMarkers(ort_red, 
                     lat = as.numeric(ort_red$Coordinates_Received2),
                     lng = as.numeric(ort_red$Coordinates_Received1),
                     icon =  icon_function('c','red'),
                     popup = content_K_green,
                     label = as.character(paste0("Component Producer: ", sep = " ", ort_red$Producer_Received))
          )
      )
  }else if(dim(ort_red)[1]==0 && dim(ort_red_line)[1]!=0){
    #-------------CASE 1.b) ----------
    setDT(ort_red_line)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort_red_line)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a=ort_red_line[,c(19,20)] # Einzelteil Werk
    b=ort_red_line[,c(21,22)] # Componenten Werk
    a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
    b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
    poly_df_ab_red_line <- sqldf("
                                 SELECT * FROM b
                                 UNION ALL
                                 SELECT * FROM a
                                 ORDER BY sequence")
    
    #color stuff green-----
    setDT(ort)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a_green=ort[,c(19,20)] # Einzelteil Werk
    b_green=ort[,c(21,22)] # Componenten Werk
    a_green$sequence <- c(sequence = seq(2, length.out = nrow(a_green), by=2))
    b_green$sequence <- c(sequence = seq(1, length.out = nrow(b_green), by=2))
    
    
    #polydf green----
    poly_df_ab_green <- sqldf("
                              SELECT * FROM b_green
                              UNION ALL
                              SELECT * FROM a_green
                              ORDER BY sequence")
    
    
    # content for markers: ----
    
    content_T_green <- paste(sep = "<br/>",
                             sprintf("<b>Faulty: %s</b>",ort$Parts_Faulty),
                             sprintf("<b>Producer: %s</b>",ort$Producer),
                             sprintf("Coordinates: %s",ort$Coordinates_Sent),
                             sprintf("Build on: %s",ort$Produced_Date),
                             sprintf("Sent Date: %s",ort$Date_Sent)
    )
    content_K_green <- paste(sep = "<br/>",
                             sprintf("<b>Producer: %s</b>",ort$Producer_Received),
                             sprintf("Coordinates: %s",ort$Coordinates_Received)
    )
    return(
      # leaflet----
      leaflet(ort) %>%
        #Add Polylines----
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>% 
        
        addPolylines(
          data = poly_df_ab_green,
          lat = as.numeric(poly_df_ab_green$Coordinates_Received2), 
          lng = as.numeric(poly_df_ab_green$Coordinates_Received1),
          color = 'green',
          weight = 3,
          opacity = 3
        )%>%
        addPolylines(
          data = poly_df_ab_red_line,
          lat = as.numeric(poly_df_ab_red_line$Coordinates_Received2), 
          lng = as.numeric(poly_df_ab_red_line$Coordinates_Received1),
          color = 'red',
          weight = 3,
          opacity = 3
        )%>%
        #AddMarkers----
      addMarkers(ort, 
                 lat = as.numeric(ort$Coordinates_Sent2),
                 lng = as.numeric(ort$Coordinates_Sent1),
                 icon =  icon_function('t','red'),
                 popup = content_T_green,
                 label = as.character(paste0("Part Producer: ", sep = " ", ort$Producer))
      )%>%
        addMarkers(ort, 
                   lat = as.numeric(ort$Coordinates_Received2),
                   lng = as.numeric(ort$Coordinates_Received1),
                   icon =  icon_function('c','red'),
                   popup = content_K_green,
                   label = as.character(paste0("Component Producer: ", sep = " ", ort$Producer_Received))
        )
    )
  }else if(dim(ort_red_line)[1]==0 && dim(ort)[1]>0){
    #-------------CASE 2) ----------
    
    setDT(ort)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a=ort[,c(19,20)] # Einzelteil Werk
    b=ort[,c(21,22)] # Componenten Werk
    a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
    b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
    
    
    #polydf----
    poly_df_ab <- sqldf("
                        SELECT * FROM b
                        UNION ALL
                        SELECT * FROM a
                        ORDER BY sequence")
    
    
    #color stuff green-----
    setDT(ort_red)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort_red)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a_green=ort_red[,c(19,20)] # Einzelteil Werk
    b_green=ort_red[,c(21,22)] # Componenten Werk
    a_green$sequence <- c(sequence = seq(2, length.out = nrow(a_green), by=2))
    b_green$sequence <- c(sequence = seq(1, length.out = nrow(b_green), by=2))
    
    
    #polydf green----
    poly_df_ab_green <- sqldf("
                              SELECT * FROM b_green
                              UNION ALL
                              SELECT * FROM a_green
                              ORDER BY sequence")
    
    
    # content for markers: ----
    content_T <- paste(sep = "<br/>",
                       sprintf("<b>Faulty: %s</b>",ort$Parts_Faulty),
                       sprintf("<b>Producer: %s</b>",ort$Producer),
                       sprintf("Coordinates: %s",ort$Coordinates_Sent),
                       sprintf("Build on: %s",ort$Produced_Date),
                       sprintf("Sent Date: %s",ort$Date_Sent)
    )
    content_K <- paste(sep = "<br/>",
                       sprintf("<b>Producer: %s</b>",ort$Producer_Received),
                       sprintf("Coordinates: %s",ort$Coordinates_Received)
    )
    content_T_green <- paste(sep = "<br/>",
                             sprintf("<b>Faulty: %s</b>",ort_red$Parts_Faulty),
                             sprintf("<b>Producer: %s</b>",ort_red$Producer),
                             sprintf("Coordinates: %s",ort_red$Coordinates_Sent),
                             sprintf("Build on: %s",ort_red$Produced_Date),
                             sprintf("Sent Date: %s",ort_red$Date_Sent)
    )
    content_K_green <- paste(sep = "<br/>",
                             sprintf("<b>Producer: %s</b>",ort_red$Producer_Received),
                             sprintf("Coordinates: %s",ort_red$Coordinates_Received)
    )
    return(
      # leaflet----
      leaflet(ort) %>%
        #Add Polylines----
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>% 
        addPolylines(
          data = poly_df_ab,
          lat = as.numeric(poly_df_ab$Coordinates_Received2), 
          lng = as.numeric(poly_df_ab$Coordinates_Received1),
          color = 'green',
          weight = 3,
          opacity = 3
        )%>%
        addPolylines(
          data = poly_df_ab_green,
          lat = as.numeric(poly_df_ab_green$Coordinates_Received2), 
          lng = as.numeric(poly_df_ab_green$Coordinates_Received1),
          color = 'green',
          weight = 3,
          opacity = 3
        )%>%
        #AddMarkers----
      addMarkers(ort, 
                 lat = as.numeric(ort$Coordinates_Sent2),
                 lng = as.numeric(ort$Coordinates_Sent1),
                 icon =  icon_function('t','green'),
                 popup = content_T,
                 label = as.character(paste0("Part Producer: ", sep = " ", ort$Producer))
      )%>%
        addMarkers(ort, 
                   lat = as.numeric(ort$Coordinates_Received2),
                   lng = as.numeric(ort$Coordinates_Received1),
                   icon =  icon_function('c','green'),
                   popup = content_K,
                   label = as.character(paste0("Component Producer: ", sep = " ", ort$Producer_Received))
        )%>%
        addMarkers(ort_red, 
                   lat = as.numeric(ort_red$Coordinates_Sent2),
                   lng = as.numeric(ort_red$Coordinates_Sent1),
                   icon =  icon_function('t','red'),
                   popup = content_T_green,
                   label = as.character(paste0("Part Producer: ", sep = " ", ort_red$Producer))
        )%>%
        addMarkers(ort_red, 
                   lat = as.numeric(ort_red$Coordinates_Received2),
                   lng = as.numeric(ort_red$Coordinates_Received1),
                   icon =  icon_function('c','red'),
                   popup = content_K_green,
                   label = as.character(paste0("Component Producer: ", sep = " ", ort_red$Producer_Received))
        )
    )
  } else if(dim(ort_red_line)[1]==0 && dim(ort)[1]==0){
    
    
    #-------------CASE 3) ----------
    
    #color stuff red-----
    setDT(ort_red)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort_red)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a_green=ort_red[,c(19,20)] # Einzelteil Werk
    b_green=ort_red[,c(21,22)] # Componenten Werk
    a_green$sequence <- c(sequence = seq(2, length.out = nrow(a_green), by=2))
    b_green$sequence <- c(sequence = seq(1, length.out = nrow(b_green), by=2))
    
    
    #polydf green----
    poly_df_ab_green <- sqldf("
                              SELECT * FROM b_green
                              UNION ALL
                              SELECT * FROM a_green
                              ORDER BY sequence")
    
    
    # content for markers: ----
    
    content_T_green <- paste(sep = "<br/>",
                             sprintf("<b>Faulty: %s</b>",ort_red$Parts_Faulty),
                             sprintf("<b>Producer: %s</b>",ort_red$Producer),
                             sprintf("Coordinates: %s",ort_red$Coordinates_Sent),
                             sprintf("Build on: %s",ort_red$Produced_Date),
                             sprintf("Sent Date: %s",ort_red$Date_Sent)
    )
    content_K_green <- paste(sep = "<br/>",
                             sprintf("<b>Producer: %s</b>",ort_red$Producer_Received),
                             sprintf("Coordinates: %s",ort_red$Coordinates_Received)
    )
    return(
      # leaflet----
      leaflet(ort_red) %>%
        #Add Polylines----
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>% 
        addPolylines(
          data = poly_df_ab_green,
          lat = as.numeric(poly_df_ab_green$Coordinates_Received2), 
          lng = as.numeric(poly_df_ab_green$Coordinates_Received1),
          color = 'green',
          weight = 3,
          opacity = 3
        )%>%
        #AddMarkers----
      
      addMarkers(ort_red, 
                 lat = as.numeric(ort_red$Coordinates_Sent2),
                 lng = as.numeric(ort_red$Coordinates_Sent1),
                 icon =  icon_function('t','red'),
                 popup = content_T_green,
                 label = as.character(paste0("Part Producer: ", sep = " ", ort_red$Producer))
      )%>%
        addMarkers(ort_red, 
                   lat = as.numeric(ort_red$Coordinates_Received2),
                   lng = as.numeric(ort_red$Coordinates_Received1),
                   icon =  icon_function('c','red'),
                   popup = content_K_green,
                   label = as.character(paste0("Component Producer: ", sep = " ", ort_red$Producer_Received))
        )
    )
  } else if(dim(ort_red_line)[1]==0 && dim(ort_red)[1]==0){
    #-------------CASE 3.b) ----------
    
    #color stuff red-----
    setDT(ort)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a_green=ort[,c(19,20)] # Einzelteil Werk
    b_green=ort[,c(21,22)] # Componenten Werk
    a_green$sequence <- c(sequence = seq(2, length.out = nrow(a_green), by=2))
    b_green$sequence <- c(sequence = seq(1, length.out = nrow(b_green), by=2))
    
    
    #polydf green----
    poly_df_ab_green <- sqldf("
                              SELECT * FROM b_green
                              UNION ALL
                              SELECT * FROM a_green
                              ORDER BY sequence")
    
    
    # content for markers: ----
    
    content_T_green <- paste(sep = "<br/>",
                             sprintf("<b>Faulty: %s</b>",ort$Parts_Faulty),
                             sprintf("<b>Producer: %s</b>",ort$Producer),
                             sprintf("Coordinates: %s",ort$Coordinates_Sent),
                             sprintf("Build on: %s",ort$Produced_Date),
                             sprintf("Sent Date: %s",ort$Date_Sent)
    )
    content_K_green <- paste(sep = "<br/>",
                             sprintf("<b>Producer: %s</b>",ort$Producer_Received),
                             sprintf("Coordinates: %s",ort$Coordinates_Received)
    )
    return(
      # leaflet----
      leaflet(ort) %>%
        #Add Polylines----
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>% 
        addPolylines(
          data = poly_df_ab_green,
          lat = as.numeric(poly_df_ab_green$Coordinates_Received2), 
          lng = as.numeric(poly_df_ab_green$Coordinates_Received1),
          color = 'green',
          weight = 3,
          opacity = 3
        )%>%
        #AddMarkers----
      
      addMarkers(ort, 
                 lat = as.numeric(ort$Coordinates_Sent2),
                 lng = as.numeric(ort$Coordinates_Sent1),
                 icon =  icon_function('t','red'),
                 popup = content_T_green,
                 label = as.character(paste0("Part Producer: ", sep = " ", ort$Producer))
      )%>%
        addMarkers(ort, 
                   lat = as.numeric(ort$Coordinates_Received2),
                   lng = as.numeric(ort$Coordinates_Received1),
                   icon =  icon_function('c','red'),
                   popup = content_K_green,
                   label = as.character(paste0("Component Producer: ", sep = " ", ort$Producer_Received))
        )
    )
  } else if(dim(ort_red_line)[1]==0 && dim(ort)[1]==0){
      
    
    #-------------CASE 3) ----------
      
      #color stuff red-----
      setDT(ort_red)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
      setDT(ort_red)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
      a_green=ort_red[,c(19,20)] # Einzelteil Werk
      b_green=ort_red[,c(21,22)] # Componenten Werk
      a_green$sequence <- c(sequence = seq(2, length.out = nrow(a_green), by=2))
      b_green$sequence <- c(sequence = seq(1, length.out = nrow(b_green), by=2))
      
      
      #polydf green----
      poly_df_ab_green <- sqldf("
                                SELECT * FROM b_green
                                UNION ALL
                                SELECT * FROM a_green
                                ORDER BY sequence")
      
      
      # content for markers: ----
      
      content_T_green <- paste(sep = "<br/>",
                               sprintf("<b>Faulty: %s</b>",ort_red$Parts_Faulty),
                               sprintf("<b>Producer: %s</b>",ort_red$Producer),
                               sprintf("Coordinates: %s",ort_red$Coordinates_Sent),
                               sprintf("Build on: %s",ort_red$Produced_Date),
                               sprintf("Sent Date: %s",ort_red$Date_Sent)
      )
      content_K_green <- paste(sep = "<br/>",
                               sprintf("<b>Producer: %s</b>",ort_red$Producer_Received),
                               sprintf("Coordinates: %s",ort_red$Coordinates_Received)
      )
      return(
        # leaflet----
        leaflet(ort_red) %>%
          #Add Polylines----
        addProviderTiles("OpenStreetMap.DE",
                         options = providerTileOptions(noWrap = TRUE))%>% 
          addPolylines(
            data = poly_df_ab_green,
            lat = as.numeric(poly_df_ab_green$Coordinates_Received2), 
            lng = as.numeric(poly_df_ab_green$Coordinates_Received1),
            color = 'green',
            weight = 3,
            opacity = 3
          )%>%
          #AddMarkers----
        
          addMarkers(ort_red, 
                     lat = as.numeric(ort_red$Coordinates_Sent2),
                     lng = as.numeric(ort_red$Coordinates_Sent1),
                     icon =  icon_function('t','red'),
                     popup = content_T_green,
                     label = as.character(paste0("Part Producer: ", sep = " ", ort_red$Producer))
          )%>%
          addMarkers(ort_red, 
                     lat = as.numeric(ort_red$Coordinates_Received2),
                     lng = as.numeric(ort_red$Coordinates_Received1),
                     icon =  icon_function('c','red'),
                     popup = content_K_green,
                     label = as.character(paste0("Component Producer: ", sep = " ", ort_red$Producer_Received))
          )
      )
    } else {
      
      
      #-------------CASE 4) ----------
    # EVERYTHING NOT EMPTY
      
    setDT(ort_red_line)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort_red_line)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a=ort_red_line[,c(19,20)] # Einzelteil Werk
    b=ort_red_line[,c(21,22)] # Componenten Werk
    a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
    b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
    poly_df_ab_red_line <- sqldf("
                                 SELECT * FROM b
                                 UNION ALL
                                 SELECT * FROM a
                                 ORDER BY sequence")
    
    setDT(ort)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a=ort[,c(19,20)] # Einzelteil Werk
    b=ort[,c(21,22)] # Componenten Werk
    a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
    b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
    
    
    #polydf----
    poly_df_ab <- sqldf("
                        SELECT * FROM b
                        UNION ALL
                        SELECT * FROM a
                        ORDER BY sequence")
    
    
    #color stuff green-----
    setDT(ort_red)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    setDT(ort_red)[, paste0("Coordinates_Received", 1:2) := tstrsplit(Coordinates_Received, " ")]
    a_green=ort_red[,c(19,20)] # Einzelteil Werk
    b_green=ort_red[,c(21,22)] # Componenten Werk
    a_green$sequence <- c(sequence = seq(2, length.out = nrow(a_green), by=2))
    b_green$sequence <- c(sequence = seq(1, length.out = nrow(b_green), by=2))
    
    
    #polydf green----
    poly_df_ab_green <- sqldf("
                              SELECT * FROM b_green
                              UNION ALL
                              SELECT * FROM a_green
                              ORDER BY sequence")
    
    
    # content for markers: ----
    content_T <- paste(sep = "<br/>",
                       sprintf("<b>Faulty: %s</b>",ort$Parts_Faulty),
                       sprintf("<b>Producer: %s</b>",ort$Producer),
                       sprintf("Coordinates: %s",ort$Coordinates_Sent),
                       sprintf("Build on: %s",ort$Produced_Date),
                       sprintf("Sent Date: %s",ort$Date_Sent)
    )
    content_K <- paste(sep = "<br/>",
                       sprintf("<b>Producer: %s</b>",ort$Producer_Received),
                       sprintf("Coordinates: %s",ort$Coordinates_Received)
    )
    content_T_green <- paste(sep = "<br/>",
                             sprintf("<b>Faulty: %s</b>",ort_red$Parts_Faulty),
                             sprintf("<b>Producer: %s</b>",ort_red$Producer),
                             sprintf("Coordinates: %s",ort_red$Coordinates_Sent),
                             sprintf("Build on: %s",ort_red$Produced_Date),
                             sprintf("Sent Date: %s",ort_red$Date_Sent)
    )
    content_K_green <- paste(sep = "<br/>",
                             sprintf("<b>Producer: %s</b>",ort_red$Producer_Received),
                             sprintf("Coordinates: %s",ort_red$Coordinates_Received)
    )
    return(
    # leaflet----
    leaflet(ort) %>%
      #Add Polylines----
    addProviderTiles("OpenStreetMap.DE",
                     options = providerTileOptions(noWrap = TRUE))%>% 
      addPolylines(
        data = poly_df_ab,
        lat = as.numeric(poly_df_ab$Coordinates_Received2), 
        lng = as.numeric(poly_df_ab$Coordinates_Received1),
        color = 'green',
        weight = 3,
        opacity = 3
      )%>%
      addPolylines(
        data = poly_df_ab_green,
        lat = as.numeric(poly_df_ab_green$Coordinates_Received2), 
        lng = as.numeric(poly_df_ab_green$Coordinates_Received1),
        color = 'green',
        weight = 3,
        opacity = 3
      )%>%
      addPolylines(
        data = poly_df_ab_red_line,
        lat = as.numeric(poly_df_ab_red_line$Coordinates_Received2), 
        lng = as.numeric(poly_df_ab_red_line$Coordinates_Received1),
        color = 'red',
        weight = 3,
        opacity = 3
      )%>%
      #AddMarkers----
    addMarkers(ort, 
               lat = as.numeric(ort$Coordinates_Sent2),
               lng = as.numeric(ort$Coordinates_Sent1),
               icon =  icon_function('t','green'),
               popup = content_T,
               label = as.character(paste0("Part Producer: ", sep = " ", ort$Producer))
    )%>%
      addMarkers(ort, 
                 lat = as.numeric(ort$Coordinates_Received2),
                 lng = as.numeric(ort$Coordinates_Received1),
                 icon =  icon_function('c','green'),
                 popup = content_K,
                 label = as.character(paste0("Component Producer: ", sep = " ", ort$Producer_Received))
      )%>%
      addMarkers(ort_red, 
                 lat = as.numeric(ort_red$Coordinates_Sent2),
                 lng = as.numeric(ort_red$Coordinates_Sent1),
                 icon =  icon_function('t','red'),
                 popup = content_T_green,
                 label = as.character(paste0("Part Producer: ", sep = " ", ort_red$Producer))
      )%>%
      addMarkers(ort_red, 
                 lat = as.numeric(ort_red$Coordinates_Received2),
                 lng = as.numeric(ort_red$Coordinates_Received1),
                 icon =  icon_function('c','red'),
                 popup = content_K_green,
                 label = as.character(paste0("Component Producer: ", sep = " ", ort_red$Producer_Received))
      )
    )
    }
  
  
}

render_complex_map_oem <- function(ort, ort_red_part, ort_red_component, ort_red_oem){
  # Input:
  # ort contains all data of producers transports etc. -> green icons  
  # ort_red contains only all data of faulty producers -> red icons
  
  # Output:
  # leaflet data to be plotted on map
  
  # Algo:
  # data from df "ort" are plotted on map, polylines in correct color, markers are green
  #     red markers are plotted on top, data of those is in "ort_red"
  
  # Assumption: 
  # 1) ort not empty
  # -> always at least two producer on the map and one line in between (color irrelevant)
  if(dim(ort)[1]==0){
    return('Error')
  }
  
  # split coordinates----
  setDT(ort)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
  setDT(ort)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
  setDT(ort)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
  setDT(ort)[, paste0("OEM_Coordinates_Received", 1:2) := tstrsplit(OEM_Coordinates_Received, " ")]
  a=ort[,c(53,54)] # Einzelteil Werk
  b=ort[,c(55,56)] # Componenten Werk
  c=ort[,c(57,58)] # OEM Werk
  d=ort[,c(59,60)] # Zulassungsort
  a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
  b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
  c$sequence <- c(sequence = seq(1, length.out = nrow(c), by=2))
  d$sequence <- c(sequence = seq(2, length.out = nrow(d), by=2))
  
  # polydf----
  poly_df_ab <- sqldf("
                      SELECT * FROM b
                      UNION ALL
                      SELECT * FROM a
                      ORDER BY sequence")
  poly_df_bc <- sqldf("
                      SELECT * FROM c
                      UNION ALL
                      SELECT * FROM b
                      ORDER BY sequence")
  poly_df_cd <- sqldf("
                      SELECT * FROM d
                      UNION ALL
                      SELECT * FROM c
                      ORDER BY sequence")
  
  
  # color stuff-----
  col_K = lapply(ort$Part_Faulty_Received, function(x) if(x>1){'red'}else{'green'})
  col_OEM = lapply(ort$Component_Faulty_Received, function(x) if(x>1){'red'}else{'green'})
  col_Z = lapply(ort$OEM_Faulty_Received, function(x) if(x>1){'red'}else{'green'})
  
  
  # content for markers: ----
  content_T <- paste(sep = "<br/>",
                     sprintf("<b>Producer: %s</b>",ort$Part_Producer),
                     sprintf("Coordinates: %s",ort$Part_Coordinates_Sent),
                     sprintf("Build on: %s",ort$Part_Build_Date),
                     sprintf("Sent Date: %s",ort$Part_Date_Sent),
                     sprintf("Faulty: %s, %s, %s",ort$Part_Faulty_sum,ort$Part_Faulty_Date,ort$Part_Faulty_Performance)
  )
  content_K <- paste(sep = "<br/>",
                     sprintf("<b>Producer: %s</b>",ort$Component_Producer),
                     sprintf("Coordinates: %s",ort$Component_Build_Coordinates),
                     sprintf("Build on: %s",ort$Component_Build_Date),
                     sprintf("Sent Date: %s",ort$Component_Date_Sent)
                     
  )
  content_Zulassung <- paste(sep = "<br/>",
                             sprintf("<b>Registration: %s</b>",ort$OEM_Producer_Received),
                             sprintf("Coordinates: %s",ort$OEM_Coordinates_Received),
                             sprintf("Registration Date: %s",ort$OEM_Coordinates_Sent)
  )
  content_OEM <- paste(sep = "<br/>",
                       sprintf("<b>Producer OEM: %s</b>",ort$OEM_Producer),
                       sprintf("Coordinates: %s",ort$OEM_Build_Coordinates),
                       sprintf("Build on: %s",ort$OEM_Build_Date),
                       sprintf("Sent Date: %s",ort$OEM_Date_Sent)
  )
  
  
  
  
  
  
  # Cases to consider: ----
  # 7 cases to consider:
  #     1) ort_red_part empty (no part producers with correct productions)
  #     2) ort_red_component empty (no component producers with correct productions)
  #     3) ort_red_oem empty (no oem producers with correct productions)
  #     4) ort_red_part && ort_red_component empty 
  #     5) ort_red_part && ort_red_oem empty 
  #     6) ort_red_component && ort_red_oem empty 
  #     7) all ort_red..  empty
  
  # ------------------------------------
  # CASE 1) -PART EMPTY---
  if(dim(ort_red_part)[1]==0 && dim(ort_red_oem)[1]!=0 && dim(ort_red_component)[1]!=0){
    # setDT(ort)----
    setDT(ort_red_component)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
    setDT(ort_red_oem)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
    
    # leaflet----
    return(leaflet() %>%
             addProviderTiles("OpenStreetMap.DE",
                              options = providerTileOptions(noWrap = TRUE))%>%
             
             # addPolylines----
           addPolylines(
             data = poly_df_bc,
             lat = as.numeric(poly_df_bc$OEM_Coordinates_Sent2), 
             lng = as.numeric(poly_df_bc$OEM_Coordinates_Sent1),
             color = col_OEM,
             weight = 3,
             opacity = 3
           )%>%
             addPolylines(
               data = poly_df_cd,
               lat = as.numeric(poly_df_cd$OEM_Coordinates_Received2), 
               lng = as.numeric(poly_df_cd$OEM_Coordinates_Received1),
               color = col_Z,
               weight = 3,
               opacity = 3
             )%>%
             addPolylines(
               data = poly_df_ab,
               lat = as.numeric(poly_df_ab$Component_Coordinates_Sent2), 
               lng = as.numeric(poly_df_ab$Component_Coordinates_Sent1),
               color = col_K,
               weight = 3,
               opacity = 3
             )
           %>%
             #addMarkers-GREEN---
             addMarkers(ort, 
                        lat = as.numeric(ort$Part_Coordinates_Sent2),
                        lng = as.numeric(ort$Part_Coordinates_Sent1),
                        icon =  icon_function('t','green'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
             )%>%
             addMarkers(ort, 
                        lat = as.numeric(ort$Component_Coordinates_Sent2),
                        lng = as.numeric(ort$Component_Coordinates_Sent1),
                        icon =  icon_function('c','green'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Received2), 
                        lng = as.numeric(ort$OEM_Coordinates_Received1),
                        icon=icon_function('co','green'),
                        popup = content_Zulassung,
                        label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort$OEM_Coordinates_Sent1),
                        icon=icon_function('o','green'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))%>%
             #addMarkers-RED---
             addMarkers(ort_red_component, 
                        lat = as.numeric(ort_red_component$Component_Coordinates_Sent2),
                        lng = as.numeric(ort_red_component$Component_Coordinates_Sent1),
                        icon =  icon_function('c','red'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort_component$Component_Producer))
             )%>%
             addMarkers(ort_red_oem, 
                        lat =as.numeric(ort_red_oem$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort_red_oem$OEM_Coordinates_Sent1),
                        icon=icon_function('o','red'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort_red_oem$OEM_Producer)))
    )
  
    }else if(dim(ort_red_component)[1]==0 && dim(ort_red_part)[1]!=0 && dim(ort_red_oem)[1]!=0){
    # CASE 2) ---COMPONENT EMPTY-----
    # setDT(ort)----
    setDT(ort_red_part)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
    setDT(ort_red_oem)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
    
    # leaflet----
    return(leaflet() %>%
             addProviderTiles("OpenStreetMap.DE",
                              options = providerTileOptions(noWrap = TRUE))%>%
             
             # addPolylines----
           addPolylines(
             data = poly_df_bc,
             lat = as.numeric(poly_df_bc$OEM_Coordinates_Sent2), 
             lng = as.numeric(poly_df_bc$OEM_Coordinates_Sent1),
             color = col_OEM,
             weight = 3,
             opacity = 3
           )%>%
             addPolylines(
               data = poly_df_cd,
               lat = as.numeric(poly_df_cd$OEM_Coordinates_Received2), 
               lng = as.numeric(poly_df_cd$OEM_Coordinates_Received1),
               color = col_Z,
               weight = 3,
               opacity = 3
             )%>%
             addPolylines(
               data = poly_df_ab,
               lat = as.numeric(poly_df_ab$Component_Coordinates_Sent2), 
               lng = as.numeric(poly_df_ab$Component_Coordinates_Sent1),
               color = col_K,
               weight = 3,
               opacity = 3
             )%>%
             #addMarkers-GREEN---
             addMarkers(ort, 
                        lat = as.numeric(ort$Part_Coordinates_Sent2),
                        lng = as.numeric(ort$Part_Coordinates_Sent1),
                        icon =  icon_function('t','green'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
             )%>%
             addMarkers(ort, 
                        lat = as.numeric(ort$Component_Coordinates_Sent2),
                        lng = as.numeric(ort$Component_Coordinates_Sent1),
                        icon =  icon_function('c','green'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Received2), 
                        lng = as.numeric(ort$OEM_Coordinates_Received1),
                        icon=icon_function('co','green'),
                        popup = content_Zulassung,
                        label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort$OEM_Coordinates_Sent1),
                        icon=icon_function('o','green'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))%>%
           #addMarkers-RED----
           addMarkers(ort_red_part, 
                      lat = as.numeric(ort_red_part$Part_Coordinates_Sent2),
                      lng = as.numeric(ort_red_part$Part_Coordinates_Sent1),
                      icon =  icon_function('t','red'),
                      popup = content_T,
                      label = as.character(paste0("Part Producer: ", sep = " ", ort_red_part$Part_Producer))
           )%>%
             addMarkers(ort_red_oem, 
                        lat =as.numeric(ort_red_oem$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort_red_oem$OEM_Coordinates_Sent1),
                        icon=icon_function('o','red'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort_red_oem$OEM_Producer)))
           
    )
  
    }else if(dim(ort_red_part)[1]!=0 && dim(ort_red_component)[1]!=0 && dim(ort_red_oem)[1]==0){
    # CASE 3) ---OEM EMPTY-
    # setDT(ort)----
    setDT(ort_red_part)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
    setDT(ort_red_component)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
    
    # leaflet----
    return(leaflet() %>%
             addProviderTiles("OpenStreetMap.DE",
                              options = providerTileOptions(noWrap = TRUE))%>%
             
             # addPolylines----
           addPolylines(
             data = poly_df_bc,
             lat = as.numeric(poly_df_bc$OEM_Coordinates_Sent2), 
             lng = as.numeric(poly_df_bc$OEM_Coordinates_Sent1),
             color = col_OEM,
             weight = 3,
             opacity = 3
           )%>%
             addPolylines(
               data = poly_df_cd,
               lat = as.numeric(poly_df_cd$OEM_Coordinates_Received2), 
               lng = as.numeric(poly_df_cd$OEM_Coordinates_Received1),
               color = col_Z,
               weight = 3,
               opacity = 3
             )%>%
             addPolylines(
               data = poly_df_ab,
               lat = as.numeric(poly_df_ab$Component_Coordinates_Sent2), 
               lng = as.numeric(poly_df_ab$Component_Coordinates_Sent1),
               color = col_K,
               weight = 3,
               opacity = 3
             )%>%
             #addMarkers-GREEN---
             addMarkers(ort, 
                        lat = as.numeric(ort$Part_Coordinates_Sent2),
                        lng = as.numeric(ort$Part_Coordinates_Sent1),
                        icon =  icon_function('t','green'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
             )%>%
             addMarkers(ort, 
                        lat = as.numeric(ort$Component_Coordinates_Sent2),
                        lng = as.numeric(ort$Component_Coordinates_Sent1),
                        icon =  icon_function('c','green'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Received2), 
                        lng = as.numeric(ort$OEM_Coordinates_Received1),
                        icon=icon_function('co','green'),
                        popup = content_Zulassung,
                        label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort$OEM_Coordinates_Sent1),
                        icon=icon_function('o','green'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))%>%
             #addMarkers-RED---
             addMarkers(ort_red_part, 
                        lat = as.numeric(ort_red_part$Part_Coordinates_Sent2),
                        lng = as.numeric(ort_red_part$Part_Coordinates_Sent1),
                        icon =  icon_function('t','red'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort_red_part$Part_Producer))
             )%>%
             addMarkers(ort_red_component, 
                        lat = as.numeric(ort_red_component$Component_Coordinates_Sent2),
                        lng = as.numeric(ort_red_component$Component_Coordinates_Sent1),
                        icon =  icon_function('c','red'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort_component$Component_Producer))
             )
    )
  
    
    
    }else if(dim(ort_red_part)[1]==0 && dim(ort_red_component)[1]==0 && dim(ort_red_oem)[1]!=0){
    # CASE 4) ---Part && COMPONENT EMPTY-
    # setDT(ort)----
    setDT(ort_red_oem)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
    
    # leaflet----
    return(leaflet() %>%
             addProviderTiles("OpenStreetMap.DE",
                              options = providerTileOptions(noWrap = TRUE))%>%
             
             # addPolylines----
           addPolylines(
             data = poly_df_bc,
             lat = as.numeric(poly_df_bc$OEM_Coordinates_Sent2), 
             lng = as.numeric(poly_df_bc$OEM_Coordinates_Sent1),
             color = col_OEM,
             weight = 3,
             opacity = 3
           )%>%
             addPolylines(
               data = poly_df_cd,
               lat = as.numeric(poly_df_cd$OEM_Coordinates_Received2), 
               lng = as.numeric(poly_df_cd$OEM_Coordinates_Received1),
               color = col_Z,
               weight = 3,
               opacity = 3
             )%>%
             addPolylines(
               data = poly_df_ab,
               lat = as.numeric(poly_df_ab$Component_Coordinates_Sent2), 
               lng = as.numeric(poly_df_ab$Component_Coordinates_Sent1),
               color = col_K,
               weight = 3,
               opacity = 3
             )
           %>%
             #addMarkers-GREEN---
             addMarkers(ort, 
                        lat = as.numeric(ort$Part_Coordinates_Sent2),
                        lng = as.numeric(ort$Part_Coordinates_Sent1),
                        icon =  icon_function('t','green'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
             )%>%
             addMarkers(ort, 
                        lat = as.numeric(ort$Component_Coordinates_Sent2),
                        lng = as.numeric(ort$Component_Coordinates_Sent1),
                        icon =  icon_function('c','green'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Received2), 
                        lng = as.numeric(ort$OEM_Coordinates_Received1),
                        icon=icon_function('co','green'),
                        popup = content_Zulassung,
                        label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort$OEM_Coordinates_Sent1),
                        icon=icon_function('o','green'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))%>%
             #addMarkers-RED---
             addMarkers(ort_red_oem, 
                        lat =as.numeric(ort_red_oem$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort_red_oem$OEM_Coordinates_Sent1),
                        icon=icon_function('o','red'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort_red_oem$OEM_Producer)))
    )
  
    }else if(dim(ort_red_component)[1]!=0 && dim(ort_red_oem)[1]==0 && dim(ort_red_part)[1]==0){
    # CASE 5) ---PART && OEM EMPTY-
    # setDT(ort)----
    setDT(ort_red_component)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
    
    # leaflet----
    return(leaflet() %>%
             addProviderTiles("OpenStreetMap.DE",
                              options = providerTileOptions(noWrap = TRUE))%>%
             
             # addPolylines----
           addPolylines(
             data = poly_df_bc,
             lat = as.numeric(poly_df_bc$OEM_Coordinates_Sent2), 
             lng = as.numeric(poly_df_bc$OEM_Coordinates_Sent1),
             color = col_OEM,
             weight = 3,
             opacity = 3
           )%>%
             addPolylines(
               data = poly_df_cd,
               lat = as.numeric(poly_df_cd$OEM_Coordinates_Received2), 
               lng = as.numeric(poly_df_cd$OEM_Coordinates_Received1),
               color = col_Z,
               weight = 3,
               opacity = 3
             )%>%
             addPolylines(
               data = poly_df_ab,
               lat = as.numeric(poly_df_ab$Component_Coordinates_Sent2), 
               lng = as.numeric(poly_df_ab$Component_Coordinates_Sent1),
               color = col_K,
               weight = 3,
               opacity = 3
             )%>%
             #addMarkers-GREEN---
             addMarkers(ort, 
                        lat = as.numeric(ort$Part_Coordinates_Sent2),
                        lng = as.numeric(ort$Part_Coordinates_Sent1),
                        icon =  icon_function('t','green'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
             )%>%
             addMarkers(ort, 
                        lat = as.numeric(ort$Component_Coordinates_Sent2),
                        lng = as.numeric(ort$Component_Coordinates_Sent1),
                        icon =  icon_function('c','green'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Received2), 
                        lng = as.numeric(ort$OEM_Coordinates_Received1),
                        icon=icon_function('co','green'),
                        popup = content_Zulassung,
                        label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort$OEM_Coordinates_Sent1),
                        icon=icon_function('o','green'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))%>%
             #addMarkers-RED---
             addMarkers(ort_red_component, 
                        lat = as.numeric(ort_red_component$Component_Coordinates_Sent2),
                        lng = as.numeric(ort_red_component$Component_Coordinates_Sent1),
                        icon =  icon_function('c','red'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort_component$Component_Producer))
             )
    )
  
    }else if(dim(ort_red_component)[1]==0 && dim(ort_red_oem)[1]==0){
    # CASE 6) ---Component && OEM EMPTY-
    # setDT(ort)----
    setDT(ort_red_part)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
    
    # leaflet----
    return(leaflet() %>%
             addProviderTiles("OpenStreetMap.DE",
                              options = providerTileOptions(noWrap = TRUE))%>%
             
             # addPolylines----
           addPolylines(
             data = poly_df_bc,
             lat = as.numeric(poly_df_bc$OEM_Coordinates_Sent2), 
             lng = as.numeric(poly_df_bc$OEM_Coordinates_Sent1),
             color = col_OEM,
             weight = 3,
             opacity = 3
           )%>%
             addPolylines(
               data = poly_df_cd,
               lat = as.numeric(poly_df_cd$OEM_Coordinates_Received2), 
               lng = as.numeric(poly_df_cd$OEM_Coordinates_Received1),
               color = col_Z,
               weight = 3,
               opacity = 3
             )%>%
             addPolylines(
               data = poly_df_ab,
               lat = as.numeric(poly_df_ab$Component_Coordinates_Sent2), 
               lng = as.numeric(poly_df_ab$Component_Coordinates_Sent1),
               color = col_K,
               weight = 3,
               opacity = 3
             )%>%
             #addMarkers-GREEN---
             addMarkers(ort, 
                        lat = as.numeric(ort$Part_Coordinates_Sent2),
                        lng = as.numeric(ort$Part_Coordinates_Sent1),
                        icon =  icon_function('t','green'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
             )%>%
             addMarkers(ort, 
                        lat = as.numeric(ort$Component_Coordinates_Sent2),
                        lng = as.numeric(ort$Component_Coordinates_Sent1),
                        icon =  icon_function('c','green'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Received2), 
                        lng = as.numeric(ort$OEM_Coordinates_Received1),
                        icon=icon_function('co','green'),
                        popup = content_Zulassung,
                        label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort$OEM_Coordinates_Sent1),
                        icon=icon_function('o','green'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))%>%
             #addMarkers-RED---
             addMarkers(ort_red_part, 
                        lat = as.numeric(ort_red_part$Part_Coordinates_Sent2),
                        lng = as.numeric(ort_red_part$Part_Coordinates_Sent1),
                        icon =  icon_function('t','red'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort_red_part$Part_Producer))
             )
    )
  
    }else if(dim(ort_red_part)[1]!=0 && dim(ort_red_component)[1]!=0 && dim(ort_red_oem)[1]!=0){
  # CASE 7) ---NOTHING EMPTY-
    # setDT(ort)----
    setDT(ort_red_part)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
    setDT(ort_red_component)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
    setDT(ort_red_oem)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
    
    # leaflet----
    return(leaflet() %>%
             addProviderTiles("OpenStreetMap.DE",
                              options = providerTileOptions(noWrap = TRUE))%>%
             
             # addPolylines----
           addPolylines(
             data = poly_df_bc,
             lat = as.numeric(poly_df_bc$OEM_Coordinates_Sent2), 
             lng = as.numeric(poly_df_bc$OEM_Coordinates_Sent1),
             color = col_OEM,
             weight = 3,
             opacity = 3
           )%>%
             addPolylines(
               data = poly_df_cd,
               lat = as.numeric(poly_df_cd$OEM_Coordinates_Received2), 
               lng = as.numeric(poly_df_cd$OEM_Coordinates_Received1),
               color = col_Z,
               weight = 3,
               opacity = 3
             )%>%
             addPolylines(
               data = poly_df_ab,
               lat = as.numeric(poly_df_ab$Component_Coordinates_Sent2), 
               lng = as.numeric(poly_df_ab$Component_Coordinates_Sent1),
               color = col_K,
               weight = 3,
               opacity = 3
             )
           %>%
             #addMarkers-GREEN---
           addMarkers(ort, 
                      lat = as.numeric(ort$Part_Coordinates_Sent2),
                      lng = as.numeric(ort$Part_Coordinates_Sent1),
                      icon =  icon_function('t','green'),
                      popup = content_T,
                      label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
           )%>%
             addMarkers(ort, 
                        lat = as.numeric(ort$Component_Coordinates_Sent2),
                        lng = as.numeric(ort$Component_Coordinates_Sent1),
                        icon =  icon_function('c','green'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Received2), 
                        lng = as.numeric(ort$OEM_Coordinates_Received1),
                        icon=icon_function('co','green'),
                        popup = content_Zulassung,
                        label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
             )%>%
             addMarkers(ort, 
                        lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort$OEM_Coordinates_Sent1),
                        icon=icon_function('o','green'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))%>%
             #addMarkers-RED---
             addMarkers(ort_red_part, 
                        lat = as.numeric(ort_red_part$Part_Coordinates_Sent2),
                        lng = as.numeric(ort_red_part$Part_Coordinates_Sent1),
                        icon =  icon_function('t','red'),
                        popup = content_T,
                        label = as.character(paste0("Part Producer: ", sep = " ", ort_red_part$Part_Producer))
             )%>%
             addMarkers(ort_red_component, 
                        lat = as.numeric(ort_red_component$Component_Coordinates_Sent2),
                        lng = as.numeric(ort_red_component$Component_Coordinates_Sent1),
                        icon =  icon_function('c','red'),
                        popup = content_K,
                        label = as.character(paste0("Component Producer: ", sep = " ", ort_component$Component_Producer))
             )%>%
             addMarkers(ort_red_oem, 
                        lat =as.numeric(ort_red_oem$OEM_Coordinates_Sent2), 
                        lng = as.numeric(ort_red_oem$OEM_Coordinates_Sent1),
                        icon=icon_function('o','red'),
                        popup = content_OEM,
                        label = as.character(paste0("OEM Producer: ", sep = " ", ort_red_oem$OEM_Producer)))
    )
  
    }else{return(c(dim(ort_red_part)[1],dim(ort_red_component)[1],dim(ort_red_oem)[1]==0))}
}