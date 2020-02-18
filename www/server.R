#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(data.table)
library(sqldf)
library(latticeExtra)
source('Blockchain_methods.R')
sql_methods=list()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #---Manu Buttons -----
  
  #----add_block-----
  
  observeEvent(input$block_save_add,{
    tryCatch({
      Type = sprintf("%s: %s",input$t,strsplit(input$id_t,"-")[[1]][1])
      block <- create_json_add(input$id_t,input$producer,input$faulty,
                               input$faulty_date,input$faulty_performance,input$produced_date,Type)
      
      dbSendQuery(con,block["sql"])
      sql_methods<<-append(sql_methods,block["sql"])
      blockchain <<- create_block(get_previous_block(blockchain),block,blockchain)
      session$sendCustomMessage(type = 'testmessage',
                                message = paste('Success'))},
      error = function(x){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Error, please try again')
      })
    
    output$db_table_manu_part <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Part"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_transport <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Transport"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_assembly <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Assembly"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
  })
  
 
  
  
  #----edit_block-----
  
  observeEvent(input$block_save_edit,{
    tryCatch({
      block <- create_json_edit(input$id_t,input$producer,input$edit_name,input$edit_value)
      dbSendQuery(con,block["sql"])    
      sql_methods<<-append(sql_methods,block["sql"])
      blockchain <<- create_block(get_previous_block(blockchain),block,blockchain)
      session$sendCustomMessage(type = 'testmessage',
                                message = paste('Success'))},
      error = function(x){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Error, please try again')
      })
    output$db_table_manu_part <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Part"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_transport <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Transport"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_assembly <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Assembly"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
  })
  

  #----sent_block-----
  
  observeEvent(input$block_save_sent,{
    tryCatch({
      block <- create_json_sent(input$id_t,input$producer_send,input$sent_date,input$sent_coordinates)
      dbSendQuery(con,block["sql"])       
      sql_methods<<-append(sql_methods,block["sql"])
      blockchain <<- create_block(get_previous_block(blockchain),block,blockchain)
      session$sendCustomMessage(type = 'testmessage',
                                message = paste('Success'))},
      error = function(x){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Error, please try again')
      })
    
    output$db_table_manu_part <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Part"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_transport <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Transport"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_assembly <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Assembly"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
  })
  
 
  #----received_block-----
  
  observeEvent(input$block_save_received,{
    tryCatch({
      block <- create_json_received(input$id_t,input$producer_received,input$received_date,input$received_coordinates,input$faulty_received)
      dbSendQuery(con,block["sql"])       
      sql_methods<<-append(sql_methods,block["sql"])
      blockchain <<- create_block(get_previous_block(blockchain),block,blockchain)
      session$sendCustomMessage(type = 'testmessage',
                                message = paste('Success'))},
      error = function(x){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Error, please try again')
        })
    
    output$db_table_manu_part <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Part"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_transport <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Transport"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_assembly <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Assembly"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
  })
 
  
  #----build_block-----
 
  observeEvent(input$block_save_build,{
    tryCatch({
      Type = sprintf("%s: %s",input$t_build,strsplit(input$id_component,"-")[[1]][1])
      block <- create_json_build(input$id_component,input$id_part,input$producer_build,input$build_date,input$build_coordinates,0,Type)
      
      dbSendQuery(con,block["sql"])       
      sql_methods<<-append(sql_methods,block["sql"])
      blockchain <<- create_block(get_previous_block(blockchain),block,blockchain)
      session$sendCustomMessage(type = 'testmessage',
                                message = paste('Success'))},
      error = function(x){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Error, please try again')
      })
    
    output$db_table_manu_part <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Part"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_transport <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Transport"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
    
    output$db_table_manu_assembly <- renderDataTable({     # daten an WUnsch anpassen
      dbFetch(dbSendQuery(con,"Select * FROM Assembly"))
    }, 
    options = list(pageLength = 10, width="100%", scrollX = TRUE))
  })

  
  observeEvent(input$BC_save,{
    tryCatch({save(blockchain,file=input$BC_save_name)
    session$sendCustomMessage(type = 'testmessage',
                              message = paste('Success'))},
    error = function(x){
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Error, please try again')
    })
  })
  
  observeEvent(input$BC_add_file, {
    
    tryCatch({block.env <- LoadToEnvironment(input$block$name)
    blockchain <<- create_block(get_previous_block(blockchain),block.env$block,blockchain)
    session$sendCustomMessage(type = 'testmessage',
                              message = paste('Success'))},
    error = function(x){
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Error, please try again')
    })
  })
  
  
 
  #--- Setup Buttons----  
  observeEvent(input$BC_exist, {
    #browser()
    tryCatch({blockchain.env <- LoadToEnvironment(input$file_blockchain$name)
      blockchain <<- blockchain.env$blockchain
      session$sendCustomMessage(type = 'testmessage',
                                message = paste('Success'))},
      error = function(x){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Error, please try again')
      })
  })
  
  #----DB-Manu-Tables----
  output$db_table_manu_part <- renderDataTable({     # daten an WUnsch anpassen
    dbFetch(dbSendQuery(con,"Select * FROM Part"))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  output$db_table_manu_transport <- renderDataTable({     # daten an WUnsch anpassen
    dbFetch(dbSendQuery(con,"Select * FROM Transport"))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  output$db_table_manu_assembly <- renderDataTable({     # daten an WUnsch anpassen
    dbFetch(dbSendQuery(con,"Select * FROM Assembly"))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  #----DB-Overview----
  output$db_table_part <- renderDataTable({     # daten an WUnsch anpassen
    dbFetch(dbSendQuery(con,"Select * FROM Part"))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  output$db_table_transport <- renderDataTable({     # daten an WUnsch anpassen
    dbFetch(dbSendQuery(con,"Select * FROM Transport"))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  output$db_table_assembly <- renderDataTable({     # daten an WUnsch anpassen
    dbFetch(dbSendQuery(con,"Select * FROM Assembly"))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  
  output$blockchain <-  renderUI({ 
    if(exists("blockchain")!=FALSE){       
      print_blockchain_text(blockchain,c(blockchain[1],blockchain[2],blockchain[length(blockchain)]))     
      }else{
      blockchain <<- start_blockchain()
    print_blockchain_text(blockchain,blockchain[1])}
  })
  output$blockchain2 <-  renderUI({ 
    if(exists("blockchain")!=FALSE){       print_blockchain_text(blockchain,blockchain[length(blockchain)])     }
  })
  output$blockchain3 <-  renderUI({ 
    if(exists("blockchain")!=FALSE){       print_blockchain_text(blockchain,c(blockchain[1],blockchain[2],blockchain[length(blockchain)]))     }
  })
  
  output$db_table_client_3 <- renderDataTable({     
    dbGetQuery(con,sprintf("select 
      Assembly.ID_Assembly as OEM_ID, A.ID_Assembly as Component_ID, A.ID_Part as Part_ID,
                           A.Producer as Component_Producer,
                           A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
                           Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
                           Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
                           Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, 
                           Part.Faulty_Performance as Part_Faulty_Performance, Part.Produced_Date as Part_Build_Date,
                           Part.Edited as Part_Edited, Part.Editor as Part_Editor,T_Part.Producer_Sent as Part_Producer_Sent,
                           T_Part.Producer_Received as Part_Producer_Received, T_Part.Date_Sent as Part_Date_Sent,
                           T_Part.Date_Received as Part_Date_Received, T_Part.Coordinates_Sent as Part_Coordinates_Sent,
                           T_Part.Coordinates_Received as Part_Coordinates_Received, T_Part.Faulty_Received as Part_Faulty_Received,
                           T_component.Producer_Sent as Component_Producer_Sent,
                           T_component.Producer_Received as Component_Producer_Received, T_component.Date_Sent as Component_Date_Sent,
                           T_component.Date_Received as Component_Date_Received, T_component.Coordinates_Sent as Component_Coordinates_Sent,
                           T_component.Coordinates_Received as Component_Coordinates_Received, 
                           T_component.Faulty_Received as Component_Faulty_Received,
                           T_oem.Producer_Sent as OEM_Producer_Sent,
                           T_oem.Producer_Received as OEM_Producer_Received, T_oem.Date_Sent as OEM_Date_Sent,
                           T_oem.Date_Received as OEM_Date_Received, T_oem.Coordinates_Sent as OEM_Coordinates_Sent,
                           T_oem.Coordinates_Received as OEM_Coordinates_Received, 
                           T_oem.Faulty_Received as OEM_Faulty_Received
                           from Assembly as A 
                           inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                           inner join Part on A.ID_Part=Part.ID
                           inner join Transport as T_Part on T_Part.ID=Part.ID
                           inner join Transport as T_component on T_component.ID=A.ID_Assembly
                           inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                           where Assembly.ID_Assembly = '%s'",input$fahrzeug_id))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  output$db_table_client_2 <- renderDataTable({    
    dbFetch(dbSendQuery(con,sprintf("Select * FROM assembly where ID_Assembly='%s'",input$fahrzeug_id)))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  output$db_table_client_1 <- renderDataTable({     
    dbFetch(dbSendQuery(con,sprintf("Select * FROM transport where ID='%s'",input$fahrzeug_id)))
  }, 
  options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  #-----icons-------
  komponente_icon <- makeIcon(
    iconUrl = "./www/einzelteil.png",
    iconWidth = 38, iconHeight = 40,
    iconAnchorX = 19, iconAnchorY = 40
  )
  contract_icon <- makeIcon(
    iconUrl = "./www/contract_green.png",
    iconWidth = 38, iconHeight = 40,
    iconAnchorX = 19, iconAnchorY = 40
  )
  icon_T <- makeIcon(
    iconUrl = "./www/tires.png",
    iconWidth = 38, iconHeight = 40,
    iconAnchorX = 19, iconAnchorY = 40
  )
  oem_icon <- makeIcon(
    iconUrl = "./www/auto_green.png",
    iconWidth = 38, iconHeight = 40,
    iconAnchorX = 19, iconAnchorY = 40
  )
  col_icon_K_g =
    makeIcon(
      iconUrl = "./www/einzelteil_green.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    )
  
 
  #--- Karte-Client-----
  
  output$mymap <- renderLeaflet({
    
    #sql----
  ort = dbGetQuery(con,sprintf("select 
      Assembly.ID_Assembly as OEM_ID, A.ID_Assembly as Component_ID, A.ID_Part as Part_ID,
           A.Producer as Component_Producer,
           A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
           Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
           Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
           Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, 
           Part.Faulty_Performance as Part_Faulty_Performance, Part.Produced_Date as Part_Build_Date,
           Part.Edited as Part_Edited, Part.Editor as Part_Editor,T_Part.Producer_Sent as Part_Producer_Sent,
           T_Part.Producer_Received as Part_Producer_Received, T_Part.Date_Sent as Part_Date_Sent,
           T_Part.Date_Received as Part_Date_Received, T_Part.Coordinates_Sent as Part_Coordinates_Sent,
           T_Part.Coordinates_Received as Part_Coordinates_Received, T_Part.Faulty_Received as Part_Faulty_Received,
           T_component.Producer_Sent as Component_Producer_Sent,
           T_component.Producer_Received as Component_Producer_Received, T_component.Date_Sent as Component_Date_Sent,
           T_component.Date_Received as Component_Date_Received, T_component.Coordinates_Sent as Component_Coordinates_Sent,
           T_component.Coordinates_Received as Component_Coordinates_Received, 
           T_component.Faulty_Received as Component_Faulty_Received,
           T_oem.Producer_Sent as OEM_Producer_Sent,
           T_oem.Producer_Received as OEM_Producer_Received, T_oem.Date_Sent as OEM_Date_Sent,
           T_oem.Date_Received as OEM_Date_Received, T_oem.Coordinates_Sent as OEM_Coordinates_Sent,
           T_oem.Coordinates_Received as OEM_Coordinates_Received, 
           T_oem.Faulty_Received as OEM_Faulty_Received
           from Assembly as A 
           inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
           inner join Part on A.ID_Part=Part.ID
           inner join Transport as T_Part on T_Part.ID=Part.ID
           inner join Transport as T_component on T_component.ID=A.ID_Assembly
           inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
           where Assembly.ID_Assembly = '%s'",input$fahrzeug_id))
  if(dim(ort)[1]>0){
  # crope----
  
  setDT(ort)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
  setDT(ort)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
  setDT(ort)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
  setDT(ort)[, paste0("OEM_Coordinates_Received", 1:2) := tstrsplit(OEM_Coordinates_Received, " ")]
  a=ort[,c(38,39,23)] # Einzelteil Werk
  b=ort[,c(40,41,23)] # Componenten Werk
  c=ort[,c(42,43,30)] # OEM Werk
  d=ort[,c(44,45,37)] # Zulassungsort
  a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
  b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
  c$sequence <- c(sequence = seq(1, length.out = nrow(c), by=2))
  d$sequence <- c(sequence = seq(2, length.out = nrow(d), by=2))
  
   
  #polydf----
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
  
  
  #color stuff-----
  col_K = lapply(ort$Part_Faulty, function(x) if(x==1){'red'}else{'green'})
  col_OEM = lapply(ort$Part_Faulty, function(x) if(x==1){'orange'}else{'green'})
  col_Z = lapply(ort$Part_Faulty, function(x) if(x==1){'orange'}else{'green'})
  
  if(ort$OEM_Faulty_Received==1){
    oem_icon <- makeIcon(
      iconUrl = "./www/auto.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    )
    contract_icon <- makeIcon(
      iconUrl = "./www/contract_red.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    )}
  if(ort$Component_Faulty_Received==1){
    icon_T <- makeIcon(
      iconUrl = "./www/tires_red.png",
      iconWidth = 38, iconHeight = 40,
      iconAnchorX = 19, iconAnchorY = 40
    )}
  
  # content for markers: ----
  content_T <- paste(sep = "<br/>",
                     sprintf("<b>Producer: %s</b>",ort$Part_Producer),
                     sprintf("Coordinates: %s",ort$Part_Coordinates_Sent),
                     sprintf("Build on: %s",ort$Part_Build_Date),
                     sprintf("Sent Date: %s",ort$Part_Date_Sent),
                     sprintf("Faulty: %s, %s, %s",ort$Part_Faulty,ort$Part_Faulty_Date,ort$Part_Faulty_Performance)
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
  # leaflet----
  leaflet() %>%
    addProviderTiles("OpenStreetMap.DE",
                     options = providerTileOptions(noWrap = TRUE))%>%
     
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
    addMarkers(ort, 
               lat = as.numeric(ort$Part_Coordinates_Sent2),
               lng = as.numeric(ort$Part_Coordinates_Sent1),
               icon =  icon_T,
               popup = content_T,
               label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
    )%>%
    addMarkers(ort, 
               lat = as.numeric(ort$Component_Coordinates_Sent2),
               lng = as.numeric(ort$Component_Coordinates_Sent1),
               icon =  col_icon_K_g,
               popup = content_K,
               label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
    )%>%
    addMarkers(ort, 
               lat =as.numeric(ort$OEM_Coordinates_Received2), 
               lng = as.numeric(ort$OEM_Coordinates_Received1),
               icon=contract_icon,
               popup = content_Zulassung,
               label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
    )%>%
    addMarkers(ort, 
               lat =as.numeric(ort$OEM_Coordinates_Sent2), 
               lng = as.numeric(ort$OEM_Coordinates_Sent1),
               icon=oem_icon,
               popup = content_OEM,
               label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))
  }})
  
  # Karte ----mymap_component_producer-----
  output$mymap_component_producer <- renderLeaflet({
    
    #sql----
    ort = dbGetQuery(con,sprintf("Select *,sum(Part.Faulty) as Parts_Faulty FROM Part 
                       inner join Transport on Transport.ID=Part.ID 
                      where Producer_Received = '%s'
                       %s group by Part.Producer",
                                 input$component_producer,input$radio_button_faulty_component_on_map))
    # crope----
    
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
    
    
    #color stuff-----
    if(input$radio_button_faulty_component_on_map=="and Part.Faulty = 1"){
      col_K = lapply(ort$Faulty, function(x) if(x==1){'red'}else{'green'})
      col_icon_K_g =
        makeIcon(
          iconUrl = "./www/einzelteil_red.png",
          iconWidth = 38, iconHeight = 40,
          iconAnchorX = 19, iconAnchorY = 40
        )
        contract_icon <- makeIcon(
          iconUrl = "./www/contract_red.png",
          iconWidth = 38, iconHeight = 40,
          iconAnchorX = 19, iconAnchorY = 40
        )
        icon_T <- makeIcon(
          iconUrl = "./www/tires_red.png",
          iconWidth = 38, iconHeight = 40,
          iconAnchorX = 19, iconAnchorY = 40
        )
        oem_icon <- makeIcon(
          iconUrl = "./www/auto.png",
          iconWidth = 38, iconHeight = 40,
          iconAnchorX = 19, iconAnchorY = 40
        )
    }else{
      col_K = lapply(ort$Faulty_Received, function(x) if(x==1){'red'}else{"green"})
    }
    
    
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
    
    # leaflet----
    leaflet(ort) %>%
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>% 
      addPolylines(
        data = poly_df_ab,
        lat = as.numeric(poly_df_ab$Coordinates_Received2), 
        lng = as.numeric(poly_df_ab$Coordinates_Received1),
        color = col_K,
        weight = 3,
        opacity = 3
      )%>%
      addMarkers(ort, 
                 lat = as.numeric(ort$Coordinates_Sent2),
                 lng = as.numeric(ort$Coordinates_Sent1),
                 icon =  icon_T,
                 popup = content_T,
                 label = as.character(paste0("Part Producer: ", sep = " ", ort$Producer))
      )%>%
      addMarkers(ort, 
                 lat = as.numeric(ort$Coordinates_Received2),
                 lng = as.numeric(ort$Coordinates_Received1),
                 icon =  col_icon_K_g,
                 popup = content_K,
                 label = as.character(paste0("Component Producer: ", sep = " ", ort$Producer_Received))
      )
  })
  output$Barplot_faulty_component_analysis_part_producer_general <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
       Producer as Part_Producer,count(*) as Number_of_Parts_Produced,Type
      from Part
       inner join Transport on Transport.Producer_Sent=Part.Producer
                       where Transport.Producer_Received='%s'
       group by Producer,Type",input$component_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Number_of_Parts_Produced, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Number of Parts Produced")+
      ggtitle("Parts Produced by Part-Producer")
  })
  output$Barplot_faulty_component_analysis_part_producer <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
       Producer as Part_Producer,sum(Faulty_Received) as Transport_Faulty,Type
      from Part
       inner join Transport on Transport.Producer_Sent=Part.Producer
                       where Transport.Producer_Received='%s'
       group by Producer,Type",input$component_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Transport_Faulty, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
    ylab("Number of faulty Parts through transportation")+
      ggtitle("Faulty Parts by Part-Producer")
  })
  
  output$Barplot_faulty_component_analysis_part_type <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
       Producer as Part_Producer,sum(Faulty) as Parts_Faulty,Type
      from Part
       inner join Transport on Transport.Producer_Sent=Part.Producer
                       where Transport.Producer_Received='%s'
       group by Producer,Type",input$component_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Parts_Faulty, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Number of faulty Parts")+
      ggtitle("Faulty Parts by Part-Type")
  })
  output$Barplot_faulty_component_analysis_part_delivery1 <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Producer as Part_Producer,sum(Faulty) as Parts_Faulty,Type,
                                  julianday(Date_Received)-julianday(Date_sent) as Deliverytime_in_Days
                                  from Part
                                  inner join Transport on Transport.Producer_Sent=Part.Producer
                                  where Transport.Producer_Received=%s %s
                                  group by Producer,Type",input$component_producer,input$radio_button_faulty_component_on_map))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Deliverytime_in_Days)) + geom_boxplot() + facet_wrap(~Type,ncol = 4)+
      xlab("Part Producer Name")+
      ylab("Deliverytime in days")+
      ggtitle("Delivery Analysis")+
      geom_point(aes(y=data$Deliverytime_in_Days, group=data$Type), position = position_dodge(width=0.75))+
      guides(fill=guide_legend(title="Type"))
  })
  output$Barplot_faulty_component_analysis_part_delivery <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Producer as Part_Producer,sum(Faulty) as Parts_Faulty,Type,
                                  julianday(Date_Received)-julianday(Date_sent) as Deliverytime_in_Days
                                  from Part
                                  inner join Transport on Transport.Producer_Sent=Part.Producer
                                  where Transport.Producer_Received='%s'
                                  group by Producer,Type",input$component_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Deliverytime_in_Days, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Deliverytime in days")+
      ggtitle("Delivery Analysis")
  })
  
  output$db_table_manu_map_component_producer <- renderDataTable({     
    dbGetQuery(con,sprintf("select 
       Part.Producer as Part_Producer,
        count(*) as No_Of_Parts, sum(Part.Faulty) as Parts_Faulty,sum(Transport.Faulty_Received) as Transport_Faulty
       from Assembly as A 
       inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
       inner join Part on A.ID_Part=Part.ID
       inner join Transport on Transport.ID=Part.ID
       where A.Producer='%s' and Part.Faulty = '1'
       group by Part.Producer
       order by Assembly.ID_Part",input$component_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  #Heatmap----
  output$myheatmap <- renderLeaflet({
    df=dbGetQuery(con,sprintf("Select *,sum(Part.Faulty) as Parts_Faulty FROM part 
                       inner join Transport on Transport.ID=Part.ID 
                           where Producer_Received = '%s' and Part.Faulty = '1'
                            group by Part.Producer",
                           input$component_producer))
    setDT(df)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
  leaflet(heatmap) %>% 
    clearMarkers() %>%
    clearShapes() %>%
      addProviderTiles("OpenStreetMap.DE",
                                        options = providerTileOptions(noWrap = TRUE))%>%
  addHeatmap(lng=as.numeric(df$Coordinates_Sent1), lat=as.numeric(df$Coordinates_Sent2),intensity = as.numeric(df$Parts_Faulty))
  })
  output$db_table_manu_map_part_faulty <- renderDataTable({     
    dbGetQuery(con,sprintf("Select Producer as Part_Prducer,
                    Part.Produced_Date as Part_Produced_Date,Type,Date_Sent,Date_Received,
                    count(*) as No_Of_Parts,
              sum(Part.Faulty) as Parts_Faulty, sum(Faulty_Received) as Transport_Faulty
                      FROM part 
                       inner join Transport on Transport.ID=Part.ID 
                           where Producer_Received = '%s' and Part.Faulty = '1'
                           group by Part.Producer",
                           input$component_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  output$myheatmap_assembly <- renderLeaflet({
    df = dbGetQuery(con,sprintf("Select *, sum(Part.Faulty) as Parts_Faulty FROM assembly 
                       inner join Transport on Transport.ID=ID_Part 
                       inner join Part on Transport.ID=Part.ID 
                                where Transport.Producer_Received = '%s' and Part.Faulty = '1'
                            group by assembly.Producer",
                                input$component_producer))
    setDT(df)[, paste0("Build_Coordinates", 1:2) := tstrsplit(Build_Coordinates, " ")]
    leaflet(heatmap) %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>%
      addHeatmap(lng=as.numeric(df$Build_Coordinates1), lat=as.numeric(df$Build_Coordinates2),intensity = df$Parts_Faulty)
  })
  output$db_table_manu_map_assembly_faulty <- renderDataTable({     
    dbGetQuery(con,sprintf("Select assembly.Producer as Part_Producer, Build_Date as Build_Date_Assembly, 
                        Date_Sent, Date_Received,count(*) as No_Of_Parts,sum(Part.Faulty) as Parts_Faulty,
                        sum(Transport.Faulty_Received) as Transport_Faulty FROM assembly 
                       inner join Transport on Transport.ID=ID_Part 
                       inner join Part on Transport.ID=Part.ID 
                           where Transport.Producer_Received = '%s' and Part.Faulty = '1'
                            group by assembly.Producer",
       input$component_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  output$myheatmap_transport <- renderLeaflet({
    df = dbGetQuery(con,sprintf("Select *,sum(Part.Faulty) as Parts_Faulty FROM transport 
                       inner join Part on Transport.ID=Part.ID 
           where Producer_Received = '%s'
                           and Part.Faulty = '1' group by Part.Producer",
                                input$component_producer))
    setDT(df)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    leaflet(heatmap) %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>%
      addHeatmap(lng=as.numeric(df$Coordinates_Sent1), lat=as.numeric(df$Coordinates_Sent2),intensity = df$Parts_Faulty)
  })
  output$db_table_manu_map_transport_faulty <- renderDataTable({     
    dbGetQuery(con,sprintf("Select Producer_Sent,Producer_Received,Produced_Date,
        Date_Sent, Date_Received,count(*) as No_Of_Parts,sum(Part.Faulty) as Parts_Faulty, 
                    sum(Transport.Faulty_Received) as Transport_Faulty FROM transport 
                       inner join Part on Transport.ID=Part.ID 
                           where Producer_Received = '%s' and Part.Faulty = '1'
                            group by Part.Producer",
                           input$component_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))

  
  
  
  
  
  # analysis OEM-Manufacturer:
  #--- Karte overview-----
  output$mymap_component_producer_oem <- renderLeaflet({
    
    #sql----
    
    ort=dbGetQuery(con,sprintf("select 
                                 Assembly.ID_Assembly as OEM_ID, A.ID_Assembly as Component_ID, A.ID_Part as Part_ID,
                               A.Producer as Component_Producer,
                               A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
                               Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
                               Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
                               Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, 
                               Part.Faulty_Performance as Part_Faulty_Performance, Part.Produced_Date as Part_Build_Date,
                               Part.Edited as Part_Edited, Part.Editor as Part_Editor,T_Part.Producer_Sent as Part_Producer_Sent,
                               T_Part.Producer_Received as Part_Producer_Received, T_Part.Date_Sent as Part_Date_Sent,
                               T_Part.Date_Received as Part_Date_Received, T_Part.Coordinates_Sent as Part_Coordinates_Sent,
                               T_Part.Coordinates_Received as Part_Coordinates_Received, T_Part.Faulty_Received as Part_Faulty_Received,
                               T_component.Producer_Sent as Component_Producer_Sent,
                               T_component.Producer_Received as Component_Producer_Received, T_component.Date_Sent as Component_Date_Sent,
                               T_component.Date_Received as Component_Date_Received, T_component.Coordinates_Sent as Component_Coordinates_Sent,
                               T_component.Coordinates_Received as Component_Coordinates_Received, 
                               T_component.Faulty_Received as Component_Faulty_Received,
                               T_oem.Producer_Sent as OEM_Producer_Sent,
                               T_oem.Producer_Received as OEM_Producer_Received, T_oem.Date_Sent as OEM_Date_Sent,
                               T_oem.Date_Received as OEM_Date_Received, T_oem.Coordinates_Sent as OEM_Coordinates_Sent,
                               T_oem.Coordinates_Received as OEM_Coordinates_Received, 
                               T_oem.Faulty_Received as OEM_Faulty_Received, sum(T_oem.Faulty_Received),
                                sum(T_component.Faulty_Received) as OEM_Faulty_Transport,sum(T_Part.Faulty_Received) as Component_Faulty_Transport,
                                sum(T_oem.Faulty_Received) as Registration_Faulty_Transport, sum(Part.Faulty) as Part_Faulty_sum
                               from Assembly as A 
                               inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                               inner join Part on A.ID_Part=Part.ID
                               inner join Transport as T_Part on T_Part.ID=Part.ID
                               inner join Transport as T_component on T_component.ID=A.ID_Assembly
                               inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                where Assembly.Producer='%s'
                               group by Part_Producer,Type,Component_Producer %s
                               ",input$oem_producer,sprintf("limit %s",input$limit)))
    if(dim(ort)[1]>0){
      # crope----
      
      setDT(ort)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
      setDT(ort)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
      setDT(ort)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
      setDT(ort)[, paste0("OEM_Coordinates_Received", 1:2) := tstrsplit(OEM_Coordinates_Received, " ")]
      a=ort[,c(43,44)] # Einzelteil Werk
      b=ort[,c(45,46)] # Componenten Werk
      c=ort[,c(47,48)] # OEM Werk
      d=ort[,c(49,50)] # Zulassungsort
      a$sequence <- c(sequence = seq(2, length.out = nrow(a), by=2))
      b$sequence <- c(sequence = seq(1, length.out = nrow(b), by=2))
      c$sequence <- c(sequence = seq(1, length.out = nrow(c), by=2))
      d$sequence <- c(sequence = seq(2, length.out = nrow(d), by=2))
      
      
      #polydf----
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
      
      
      #color stuff-----
      col_K = lapply(ort$Part_Faulty_sum, function(x) if(x>1){'red'}else{'green'})
      col_OEM = lapply(ort$Part_Faulty_sum, function(x) if(x>1){'orange'}else{'green'})
      col_Z = lapply(ort$Part_Faulty_sum, function(x) if(x>1){'orange'}else{'green'})
      
      # content for markers: ----
      content_T <- paste(sep = "<br/>",
                         sprintf("<b>Producer: %s</b>",ort$Part_Producer),
                         sprintf("Coordinates: %s",ort$Part_Coordinates_Sent),
                         sprintf("Build on: %s",ort$Part_Build_Date),
                         sprintf("Sent Date: %s",ort$Part_Date_Sent),
                         sprintf("Faulty: %s, %s, %s",ort$Part_Faulty,ort$Part_Faulty_Date,ort$Part_Faulty_Performance)
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
      # leaflet----
      leaflet() %>%
        addProviderTiles("OpenStreetMap.DE",
                         options = providerTileOptions(noWrap = TRUE))%>%
        
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
        addMarkers(ort, 
                   lat = as.numeric(ort$Part_Coordinates_Sent2),
                   lng = as.numeric(ort$Part_Coordinates_Sent1),
                   icon =  icon_T,
                   popup = content_T,
                   label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
        )%>%
        addMarkers(ort, 
                   lat = as.numeric(ort$Component_Coordinates_Sent2),
                   lng = as.numeric(ort$Component_Coordinates_Sent1),
                   icon =  col_icon_K_g,
                   popup = content_K,
                   label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
        )%>%
        addMarkers(ort, 
                   lat =as.numeric(ort$OEM_Coordinates_Received2), 
                   lng = as.numeric(ort$OEM_Coordinates_Received1),
                   icon=contract_icon,
                   popup = content_Zulassung,
                   label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
        )%>%
        addMarkers(ort, 
                   lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                   lng = as.numeric(ort$OEM_Coordinates_Sent1),
                   icon=oem_icon,
                   popup = content_OEM,
                   label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))
    }})
  
  
  output$Barplot_faulty_component_analysis_part_producer_general_oem <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Part.Producer as Part_Producer,count(*) as Number_of_Parts_Produced,Type
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s'
                                  group by Part_Producer,Type",input$oem_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Number_of_Parts_Produced, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Number of Parts Produced")+
      ggtitle("Parts Produced by Part-Producer")
  })
  output$Barplot_faulty_component_analysis_part_producer_oem <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Part.Producer as Part_Producer,sum(T_Part.Faulty_Received) as Transport_Faulty,Type
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s'
                                  group by Part_Producer,Type",input$oem_producer))

    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Transport_Faulty, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Number of faulty Parts through transportation")+
      ggtitle("Faulty Parts by Part-Producer")
  })
  
  output$Barplot_faulty_component_analysis_part_type_oem <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Part.Producer as Part_Producer,sum(Part.Faulty) as Parts_Faulty,Type
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s'
                                  group by Part_Producer,Type",input$oem_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Parts_Faulty, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Number of faulty Parts")+
      ggtitle("Faulty Parts by Part-Type")
  })
  output$Barplot_faulty_component_analysis_part_delivery1_oem1 <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Part.Producer as Part_Producer,sum(Part.Faulty) as Parts_Faulty,Type,
                                  julianday(T_Part.Date_Received)-julianday(T_Part.Date_sent) as Deliverytime_in_Days
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s' %s
                                  group by Part_Producer,Type %s",input$oem_producer,input$radio_button_faulty_oem_on_map,sprintf("limit %s",input$limit)))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Deliverytime_in_Days)) + geom_boxplot() + facet_wrap(~Type,ncol = 4)+
      xlab("Part Producer Name")+
      ylab("Deliverytime in days")+
      ggtitle("Delivery Analysis from Part to Component Manufacturer")+
      geom_point(aes(y=data$Deliverytime_in_Days, group=data$Type), position = position_dodge(width=0.75))+
      guides(fill=guide_legend(title="Type"))
  })
  output$Barplot_faulty_component_analysis_part_delivery1_oem2 <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Part.Producer as Part_Producer,sum(Part.Faulty) as Parts_Faulty,Type,
                                  julianday(T_component.Date_Received)-julianday(T_component.Date_sent) as Deliverytime_in_Days
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s' %s
                                  group by Part_Producer,Type %s",input$oem_producer,input$radio_button_faulty_oem_on_map,sprintf("limit %s",input$limit)))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Deliverytime_in_Days)) + geom_boxplot() + facet_wrap(~Type,ncol = 4)+
      xlab("Part Producer Name")+
      ylab("Deliverytime in days")+
      ggtitle("Delivery Analysis from Component to OEM Manufacturer")+
      geom_point(aes(y=data$Deliverytime_in_Days, group=data$Type), position = position_dodge(width=0.75))+
      guides(fill=guide_legend(title="Type"))
  })
  output$Barplot_faulty_component_analysis_part_delivery_oem <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Part.Producer as Part_Producer,sum(Part.Faulty) as Parts_Faulty,Type,
                                  julianday(T_component.Date_Received)-julianday(T_Part.Date_sent) as Deliverytime_in_Days
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s'
                                  group by Part_Producer,Type",input$oem_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Deliverytime_in_Days, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Deliverytime in days from Part to OEM factory")+
      ggtitle("Delivery Analysis")
  })
  
  output$db_table_manu_map_producer_oem <- renderDataTable({     
    ort=dbGetQuery(con,sprintf("select 
                                 A.Producer as Component_Producer,Assembly.Producer as OEM_Producer, Part.Producer as Part_Producer,
                               Part.Type as Type , sum(T_oem.Faulty_Received),
                               sum(T_component.Faulty_Received) as OEM_Faulty_Transport,sum(T_Part.Faulty_Received) as Component_Faulty_Transport,
                               sum(T_oem.Faulty_Received) as Registration_Faulty_Transport, sum(Part.Faulty) as Part_Faulty_sum
                               from Assembly as A 
                               inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                               inner join Part on A.ID_Part=Part.ID
                               inner join Transport as T_Part on T_Part.ID=Part.ID
                               inner join Transport as T_component on T_component.ID=A.ID_Assembly
                               inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                               where Assembly.Producer='%s' %s
                               group by Part_Producer,Type,Component_Producer %s
                               ",input$oem_producer,input$radio_button_faulty_oem_on_map,sprintf("limit %s",input$limit)))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  #Heatmap----
  output$myheatmap_oem <- renderLeaflet({
    df=dbGetQuery(con,sprintf("select sum(Part.Faulty) as Parts_Faulty,
                                  Part.Producer as Part_Producer,sum(Part.Faulty) as Parts_Faulty,Type,
                                T_Part.Coordinates_Sent as Coordinates_Sent,Part.Producer as Producer
                              from Assembly as A 
                              inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                              inner join Part on A.ID_Part=Part.ID
                              inner join Transport as T_Part on T_Part.ID=Part.ID
                              inner join Transport as T_component on T_component.ID=A.ID_Assembly
                              inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                              where Assembly.Producer='%s'  and Part.Faulty = '1'
                              group by Part.Producer,Type",input$oem_producer))

    setDT(df)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    leaflet(heatmap) %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>%
      addHeatmap(lng=as.numeric(df$Coordinates_Sent1), lat=as.numeric(df$Coordinates_Sent2),intensity = as.numeric(df$Parts_Faulty))
  })
  output$db_table_manu_map_part_faulty_oem <- renderDataTable({     
    dbGetQuery(con,sprintf("Select Part.Producer as Part_Producer,
                           Part.Produced_Date as Part_Produced_Date,Type,T_Part.Date_Sent as Date_Sent,T_Part.Date_Received as Date_Received,
                           count(*) as No_Of_Parts,
                           sum(Part.Faulty) as Parts_Faulty, sum(T_Part.Faulty_Received) as Transport_Faulty
                           from Assembly as A 
                              inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                           inner join Part on A.ID_Part=Part.ID
                           inner join Transport as T_Part on T_Part.ID=Part.ID
                           inner join Transport as T_component on T_component.ID=A.ID_Assembly
                           inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                           where Assembly.Producer='%s'  and Part.Faulty = '1'
                           group by Part.Producer,Type",input$oem_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  output$myheatmap_assembly_oem <- renderLeaflet({
    df = dbGetQuery(con,sprintf("Select *, sum(Part.Faulty) as Parts_Faulty , A.Build_Coordinates as Build_Coordinates
                              from Assembly as A 
                              inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                inner join Part on A.ID_Part=Part.ID
                                inner join Transport as T_Part on T_Part.ID=Part.ID
                                inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                where Assembly.Producer='%s'  and Assembly.Faulty = '1'
                                group by Part.Producer,Type",input$oem_producer))
    setDT(df)[, paste0("Build_Coordinates", 1:2) := tstrsplit(Build_Coordinates, " ")]
    leaflet(heatmap) %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>%
      addHeatmap(lng=as.numeric(df$Build_Coordinates1), lat=as.numeric(df$Build_Coordinates2),intensity = df$Parts_Faulty)
  })
  output$db_table_manu_map_assembly_faulty_oem <- renderDataTable({     
    dbGetQuery(con,sprintf("Select assembly.Producer as Part_Producer, assembly.Build_Date as Build_Date_Assembly, 
                           T_component.Date_Sent as Date_Sent , T_component.Date_Received as Date_Received,count(*) as No_Of_Parts,sum(Part.Faulty) as Parts_Faulty,
                           sum(T_component.Faulty_Received) as Transport_Faulty 
                          from Assembly as A 
                              inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                           inner join Part on A.ID_Part=Part.ID
                           inner join Transport as T_Part on T_Part.ID=Part.ID
                           inner join Transport as T_component on T_component.ID=A.ID_Assembly
                           inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                           where Assembly.Producer='%s'  and Assembly.Faulty = '1'
                           group by Part.Producer,Type",input$oem_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  output$myheatmap_transport_oem <- renderLeaflet({
    df = dbGetQuery(con,sprintf("Select T_component.Coordinates_Sent as Coordinates_Sent,sum(Part.Faulty) as Parts_Faulty 
                                    from Assembly as A 
                              inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                inner join Part on A.ID_Part=Part.ID
                                inner join Transport as T_Part on T_Part.ID=Part.ID
                                inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                where Assembly.Producer='%s'  and T_component.Faulty_Received = '1'
                                group by Part.Producer,Type",input$oem_producer))
    setDT(df)[, paste0("Coordinates_Sent", 1:2) := tstrsplit(Coordinates_Sent, " ")]
    leaflet(heatmap) %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addProviderTiles("OpenStreetMap.DE",
                       options = providerTileOptions(noWrap = TRUE))%>%
      addHeatmap(lng=as.numeric(df$Coordinates_Sent1), lat=as.numeric(df$Coordinates_Sent2),intensity = df$Parts_Faulty)
  })
  output$db_table_manu_map_transport_faulty_oem <- renderDataTable({     
    dbGetQuery(con,sprintf("Select T_component.Producer_Sent as Producer_Sent,T_component.Producer_Received as Producer_Received,Part.Produced_Date as Produced_Date,
                           T_component.Date_Sent as Date_Sent, T_component.Date_Received as Date_Received,count(*) as No_Of_Parts,sum(Part.Faulty) as Parts_Faulty, 
                           sum(T_component.Faulty_Received) as Transport_Faulty 
                             from Assembly as A 
                              inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                           inner join Part on A.ID_Part=Part.ID
                           inner join Transport as T_Part on T_Part.ID=Part.ID
                           inner join Transport as T_component on T_component.ID=A.ID_Assembly
                           inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                           where Assembly.Producer='%s'  and T_component.Faulty_Received = '1'
                           group by Part.Producer,Type",input$oem_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  output$db_table_faulty_oem <- renderDataTable({     
    dbGetQuery(con,sprintf("select 
                                 Assembly.ID_Assembly as OEM_ID, A.ID_Assembly as Component_ID, A.ID_Part as Part_ID,
                               A.Producer as Component_Producer,
                               A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
                               Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
                               Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
                               Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, 
                               Part.Faulty_Performance as Part_Faulty_Performance, Part.Produced_Date as Part_Build_Date,
                               Part.Edited as Part_Edited, Part.Editor as Part_Editor,T_Part.Producer_Sent as Part_Producer_Sent,
                               T_Part.Producer_Received as Part_Producer_Received, T_Part.Date_Sent as Part_Date_Sent,
                               T_Part.Date_Received as Part_Date_Received, T_Part.Coordinates_Sent as Part_Coordinates_Sent,
                               T_Part.Coordinates_Received as Part_Coordinates_Received, T_Part.Faulty_Received as Part_Faulty_Received,
                               T_component.Producer_Sent as Component_Producer_Sent,
                               T_component.Producer_Received as Component_Producer_Received, T_component.Date_Sent as Component_Date_Sent,
                               T_component.Date_Received as Component_Date_Received, T_component.Coordinates_Sent as Component_Coordinates_Sent,
                               T_component.Coordinates_Received as Component_Coordinates_Received, 
                               T_component.Faulty_Received as Component_Faulty_Received,
                               T_oem.Producer_Sent as OEM_Producer_Sent,
                               T_oem.Producer_Received as OEM_Producer_Received, T_oem.Date_Sent as OEM_Date_Sent,
                               T_oem.Date_Received as OEM_Date_Received, T_oem.Coordinates_Sent as OEM_Coordinates_Sent,
                               T_oem.Coordinates_Received as OEM_Coordinates_Received, 
                               T_oem.Faulty_Received as OEM_Faulty_Received, sum(T_oem.Faulty_Received),
                               sum(T_component.Faulty_Received) as OEM_Faulty_Transport,sum(T_Part.Faulty_Received) as Component_Faulty_Transport,
                               sum(T_oem.Faulty_Received) as Registration_Faulty_Transport, sum(Part.Faulty) as Part_Faulty_sum
                               from Assembly as A 
                               inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                               inner join Part on A.ID_Part=Part.ID
                               inner join Transport as T_Part on T_Part.ID=Part.ID
                               inner join Transport as T_component on T_component.ID=A.ID_Assembly
                               inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                               where Assembly.Producer='%s' and T_Part.Faulty_Received=='0' and Part.Faulty=='0' and T_component.Faulty_Received=='0' and A.Faulty=='0'
                           group by T_oem.Producer_Received
                               ",input$oem_producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  
  output$Barplot_analysis_oem1 <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                 Assembly.ID_Assembly as OEM_ID, A.ID_Assembly as Component_ID, A.ID_Part as Part_ID,
                                  A.Producer as Component_Producer,
                                  A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
                                  Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
                                  Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
                                  Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, 
                                  Part.Faulty_Performance as Part_Faulty_Performance, Part.Produced_Date as Part_Build_Date,
                                  Part.Edited as Part_Edited, Part.Editor as Part_Editor,T_Part.Producer_Sent as Part_Producer_Sent,
                                  T_Part.Producer_Received as Part_Producer_Received, T_Part.Date_Sent as Part_Date_Sent,
                                  T_Part.Date_Received as Part_Date_Received, T_Part.Coordinates_Sent as Part_Coordinates_Sent,
                                  T_Part.Coordinates_Received as Part_Coordinates_Received, T_Part.Faulty_Received as Part_Faulty_Received,
                                  T_component.Producer_Sent as Component_Producer_Sent,
                                  T_component.Producer_Received as Component_Producer_Received, T_component.Date_Sent as Component_Date_Sent,
                                  T_component.Date_Received as Component_Date_Received, T_component.Coordinates_Sent as Component_Coordinates_Sent,
                                  T_component.Coordinates_Received as Component_Coordinates_Received, 
                                  T_component.Faulty_Received as Component_Faulty_Received,
                                  T_oem.Producer_Sent as OEM_Producer_Sent,
                                  T_oem.Producer_Received as OEM_Producer_Received, T_oem.Date_Sent as OEM_Date_Sent,
                                  T_oem.Date_Received as OEM_Date_Received, T_oem.Coordinates_Sent as OEM_Coordinates_Sent,
                                  T_oem.Coordinates_Received as OEM_Coordinates_Received, 
                                  T_oem.Faulty_Received as OEM_Faulty_Received, sum(T_oem.Faulty_Received),
                                  sum(T_component.Faulty_Received) as OEM_Faulty_Transport,sum(T_Part.Faulty_Received) as Component_Faulty_Transport,
                                  sum(T_oem.Faulty_Received) as Registration_Faulty_Transport, sum(Part.Faulty) as Part_Faulty_sum,count(*) as Number,Type
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s' and T_Part.Faulty_Received=='0' and Part.Faulty=='0' and T_component.Faulty_Received=='0' and A.Faulty=='0'
                                  group by Type,Part.Producer
                                  ",input$oem_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Part_Producer, y=data$Number, fill=data$Type)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Part Producer Name")+
      ylab("Number of working vehicals")+
      ggtitle("Perfect Vehicals")
  })
  output$Barplot_analysis_oem2 <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Assembly.ID_Assembly as OEM_ID, A.ID_Assembly as Component_ID, A.ID_Part as Part_ID,
                                  A.Producer as Component_Producer,
                                  A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
                                  Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
                                  Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
                                  Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, 
                                  Part.Faulty_Performance as Part_Faulty_Performance, Part.Produced_Date as Part_Build_Date,
                                  Part.Edited as Part_Edited, Part.Editor as Part_Editor,T_Part.Producer_Sent as Part_Producer_Sent,
                                  T_Part.Producer_Received as Part_Producer_Received, T_Part.Date_Sent as Part_Date_Sent,
                                  T_Part.Date_Received as Part_Date_Received, T_Part.Coordinates_Sent as Part_Coordinates_Sent,
                                  T_Part.Coordinates_Received as Part_Coordinates_Received, T_Part.Faulty_Received as Part_Faulty_Received,
                                  T_component.Producer_Sent as Component_Producer_Sent,
                                  T_component.Producer_Received as Component_Producer_Received, T_component.Date_Sent as Component_Date_Sent,
                                  T_component.Date_Received as Component_Date_Received, T_component.Coordinates_Sent as Component_Coordinates_Sent,
                                  T_component.Coordinates_Received as Component_Coordinates_Received, 
                                  T_component.Faulty_Received as Component_Faulty_Received,
                                  T_oem.Producer_Sent as OEM_Producer_Sent,
                                  T_oem.Producer_Received as OEM_Producer_Received, T_oem.Date_Sent as OEM_Date_Sent,
                                  T_oem.Date_Received as OEM_Date_Received, T_oem.Coordinates_Sent as OEM_Coordinates_Sent,
                                  T_oem.Coordinates_Received as OEM_Coordinates_Received, 
                                  T_oem.Faulty_Received as OEM_Faulty_Received, sum(T_oem.Faulty_Received),
                                  sum(T_component.Faulty_Received) as OEM_Faulty_Transport,sum(T_Part.Faulty_Received) as Component_Faulty_Transport,
                                  sum(T_oem.Faulty_Received) as Registration_Faulty_Transport, sum(Part.Faulty) as Part_Faulty_sum,count(*) as Number,Type,
                                  Assembly.Type_Assembly as Type_Assembly
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s' and T_Part.Faulty_Received=='0' and Part.Faulty=='0' and T_component.Faulty_Received=='0' and A.Faulty=='0'
                                  group by Assembly.Type_Assembly,Component_Producer
                                  ",input$oem_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Component_Producer, y=data$Number, fill=data$Type_Assembly)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Component Producer Name")+
      ylab("Number of working vehicals")+
      ggtitle("Perfect Vehicals")
  })
  
  output$Barplot_analysis_oem3 <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Assembly.ID_Assembly as OEM_ID, A.ID_Assembly as Component_ID, A.ID_Part as Part_ID,
                                  A.Producer as Component_Producer,
                                  A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
                                  Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
                                  Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
                                  Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, 
                                  Part.Faulty_Performance as Part_Faulty_Performance, Part.Produced_Date as Part_Build_Date,
                                  Part.Edited as Part_Edited, Part.Editor as Part_Editor,T_Part.Producer_Sent as Part_Producer_Sent,
                                  T_Part.Producer_Received as Part_Producer_Received, T_Part.Date_Sent as Part_Date_Sent,
                                  T_Part.Date_Received as Part_Date_Received, T_Part.Coordinates_Sent as Part_Coordinates_Sent,
                                  T_Part.Coordinates_Received as Part_Coordinates_Received, T_Part.Faulty_Received as Part_Faulty_Received,
                                  T_component.Producer_Sent as Component_Producer_Sent,
                                  T_component.Producer_Received as Component_Producer_Received, T_component.Date_Sent as Component_Date_Sent,
                                  T_component.Date_Received as Component_Date_Received, T_component.Coordinates_Sent as Component_Coordinates_Sent,
                                  T_component.Coordinates_Received as Component_Coordinates_Received, 
                                  T_component.Faulty_Received as Component_Faulty_Received,
                                  T_oem.Producer_Sent as OEM_Producer_Sent,
                                  T_oem.Producer_Received as OEM_Producer_Received, T_oem.Date_Sent as OEM_Date_Sent,
                                  T_oem.Date_Received as OEM_Date_Received, T_oem.Coordinates_Sent as OEM_Coordinates_Sent,
                                  T_oem.Coordinates_Received as OEM_Coordinates_Received, 
                                  T_oem.Faulty_Received as OEM_Faulty_Received, sum(T_oem.Faulty_Received),
                                  sum(T_component.Faulty_Received) as OEM_Faulty_Transport,sum(T_Part.Faulty_Received) as Component_Faulty_Transport,
                                  sum(T_oem.Faulty_Received) as Registration_Faulty_Transport, sum(Part.Faulty) as Part_Faulty_sum,count(*) as Number,Type,
                                  A.Type_Assembly as Type_Assembly
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s' and T_Part.Faulty_Received=='0' and Part.Faulty=='0' and T_component.Faulty_Received=='0' and A.Faulty=='0'
                                  group by A.Type_Assembly,Component_Producer
                                  ",input$oem_producer))
    
    # Render a barplot
    ggplot(data, aes(x=data$Component_Producer, y=data$Number, fill=data$Type_Assembly)) + 
      geom_bar(stat = "identity", position = "dodge")+
      xlab("Component Producer Name")+
      ylab("Number of working vehicals")+
      ggtitle("Perfect Vehicals")
  })
  
  })


