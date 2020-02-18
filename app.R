#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinydashboard)
require(DT)
require(data.table)
require(sqldf)
require(dplyr)
require(leaflet)
require(latticeExtra)
require(ggplot2)
require(plotly)
require(leaflet.extras)
source('Blockchain_methods.R')
source('map_methods.R')


top_left <- "https://cache.desktopnexus.com/thumbseg/684/684196-bigthumbnail.jpg"
top_right <- "https://cdn.hipwallpaper.com/i/24/78/dW7pmC.jpg"
bottom_left <- "https://previews.123rf.com/images/weicheltfilm/weicheltfilm1302/weicheltfilm130200020/17970001-a-virtual-car-computer-graphic.jpg"
bottom_right <- "https://longwallpapers.com/Desktop-Wallpaper/car-wallpaper-background-For-Desktop-Wallpaper.jpg"

# Blockchain & Database Setup
con <<- DBI::dbConnect(RSQLite::SQLite(),"./mydb_new.sqlite")
blockchain_file <-  "./DB/blockchain_start.rds"  #"blockchain_new.rds"#
blockchain <<- readRDS(file=as.character(blockchain_file))

type <<- ''

ui <- fluidPage( 
  dashboardPage(skin ='red',
                dashboardHeader(title = "Supply Chain-BC Dashboard",
                                #--- Notification messages----
                                dropdownMenu(type = "messages",
                                             messageItem(
                                               from = "New User",
                                               message = "Manual accessable here",
                                               icon = icon("question"),
                                               href = "https://github.com/NeumannCarolin/Blockchain-Technology-based-Supply-Chain-Traceability-Concept-in-R-for-the-Automotive-Industry" 
                                             )
                                ),
                                dropdownMenu(type = "messages",
                                             messageItem(
                                               from = "Image source",
                                               message = "Impressum: top_left ",
                                               icon = icon("image"),
                                               href = top_left
                                             ),
                                             messageItem(
                                               from = "Image source",
                                               message = "Impressum: top_right",
                                               icon = icon("image"),
                                               href = top_right
                                             ),
                                             messageItem(
                                               from = "Image source",
                                               message = "Impressum: bottom_left",
                                               icon = icon("image"),
                                               href = bottom_left
                                             ),
                                             messageItem(
                                               from = "Image source",
                                               message = "Impressum: bottom_right",
                                               icon = icon("image"),
                                               href = bottom_right 
                                             )
                                )
                ),
                #---Slidebar Menu----
                dashboardSidebar(
                  sidebarMenuOutput("menu")
                ),
                dashboardBody(
                  tags$head(
                    tags$link(href="style.css", rel="stylesheet", type="text/css")
                  ),
                  tabItems(
                    #--- DONE Setup tab content ----
                    tabItem(tabName = "setup",
                            fluidPage(
                              titlePanel("Welcome to the supply chain application using blockchain technology"),
                              tags$h3("This shiny app demonstrates
                                      how to use a blockchain based
                                      supply chain and
                                      analyse its transparent data."),
                              tags$hr(),
                              tags$h4("Please choose your type of user below: "),
                              selectInput("user_type_setup","",
                                          choices = c("Select User ","Client","Producer")),
                              tags$hr(),
                              htmlOutput("number_input"),
                              tags$hr(),
                              p('Within the "Blockchain"- tab an already existing blockchain can be loaded or a new created'),
                              p('All Tabs below will enable tools for transparency and data analysation based on the loaded data'),
                              htmlOutput("whoareyou")
                              )
                            ),
                    
                    #--- DONE Blockchain tab content ----
                    tabItem(tabName = "bc",
                            fluidRow(valueBoxOutput('lenBlock')),
                            fluidRow(
                              box(width = 12, title = "Block examples: ", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                  htmlOutput("blockchain_first_block"),
                                  htmlOutput("blockchain"))
                            )
                    ),
                    
                    #---DONE Data tab content----
                    tabItem(tabName = "data",
                            fluidRow(
                              box(width = 12,
                                  title = "Database Overview", status = "warning", solidHeader = TRUE,  collapsible = TRUE,
                                  htmlOutput("Database_overview")
                              )
                            )
                    ),
                    
                    #---DONE Analytics content----
                    tabItem(tabName = "ana",
                            fluidPage(htmlOutput("ana")),
                            box(width = 12,
                                title = "Map", status = "primary", solidHeader = TRUE,
                                leafletOutput("mymap")
                            )
                    ),
                    tabItem(tabName = "ana_producer",
                            fluidPage(htmlOutput('ana_producer_tabs')
                            )
                    ),
                    
                    #---DONE Profil content----
                    tabItem(tabName = "profil",
                            column(width = 12,
                                   box(
                                     title = 'Profile', width = NULL, status = "warning",
                                     infoBox("Client",fill = TRUE,
                                             icon = icon("user"),
                                             color = "yellow"
                                     )
                                   )
                            )
                    ),
                    tabItem(tabName = "profil_producer",
                            column(width = 12,
                                   box(
                                     title = 'Profile', width = NULL, status = "warning",
                                     fluidRow(
                                       valueBoxOutput('profileBox'),
                                       box(
                                         title = "Location", status = "primary",
                                         leafletOutput("mymap_profile")
                                       )
                                     )
                                   ),
                                   valueBoxOutput('profileBox_boxes1'),
                                   valueBoxOutput('profileBox_boxes2'),
                                   valueBoxOutput('profileBox_boxes3')
                            )
                    ),
                    
                    #---DONE Impressum content----
                    tabItem(tabName = "impressum",
                            tags$div(class="landing-wrapper",
                                     
                                     # child: images
                                     tags$div(class="landing-block background-content",
                                              img(src=top_left),
                                              
                                              # top right
                                              img(src=top_right),
                                              
                                              # bottom left
                                              img(src=bottom_left), 
                                              
                                              # bottom right
                                              img(src=bottom_right)
                                     ),
                                     
                                     # child: content (that will be placed on top of the images)
                                     tags$div(class="landing-block foreground-content",
                                              tags$div(class="foreground-text",
                                                       tags$h3("Development of a Blockchain Technology based Supply Chain Traceability Concept in R"),
                                                       br(),
                                                       tags$h5("Institut für Werkzeugmaschinen und Fabrikbetrieb"),
                                                       tags$h5("Fachgebiet: Qualitätswissenschaft"),
                                                       br(),
                                                       tags$h4("Carolin Neumann, 371802"),
                                                       br(),
                                                       br(),
                                                       tags$h6("Technische Universität Berlin"),
                                                       tags$h6("Straße des 17. Juni 135"),
                                                       tags$h6("10623 Berlin"),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       tags$h6('Image Sources:'),
                                                       br(),
                                                       tags$h6('top left: https://cache.desktopnexus.com/thumbseg/684/684196-bigthumbnail.jpg'),
                                                       tags$h6('top right: https://cdn.hipwallpaper.com/i/24/78/dW7pmC.jpg'),
                                                       tags$h6('bottom left: https://previews.123rf.com/images/weicheltfilm/weicheltfilm1302/weicheltfilm130200020/17970001-a-virtual-car-computer-graphic.jpg'),
                                                       tags$h6('bottom right: https://longwallpapers.com/Desktop-Wallpaper/car-wallpaper-background-For-Desktop-Wallpaper.jpg')
                                              )   
                                     )
                            )
                    )
                    )
  )
  )
)
















# Define server logic ----
server <- function(input, output, session) {
  number = ""
  # type = ''
  #con <<- DBI::dbConnect(RSQLite::SQLite(),"./mydb_new.sqlite")
  #blockchain <<- start_blockchain()
  #new_database("./mydb_new.sqlite","new") 
  #from_BC_into_DB(con, blockchain)
  
  
  output$lenBlock <- renderValueBox({
    valueBox(length(blockchain), "Blocks on the chain", icon = icon("cube"))
  })
  #---Profile Boxes----
  output$profileBox <- renderValueBox({ #TODO get all infos of that producer out of DB and print it here
    ort = dbFetch(dbSendQuery(con,
                              sprintf("select coordinates_sent from transport where producer_sent =='%s' limit 1;",input$producer)))
    
    valueBox(
      paste0(type,' ',input$user_type_setup," ",input$producer),
      paste0(" ",ort), icon = icon("user"),
      color = "yellow"
    )
   })
  output$profileBox_boxes1 <- renderValueBox({
    if(type!='Part'){
      supp = dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_received='%s' group by producer_sent;",input$producer)))
      
        valueBox("Supplier", supp, icon = icon("industry"))
    }else{
      shipp = dbFetch(dbSendQuery(con,sprintf("select count(*) from part where producer='%s';",input$producer)))
      
      valueBox("Parts created", shipp, icon = icon("cogs"))
    }
    
  })
  output$profileBox_boxes2 <- renderValueBox({
    if(type=='OEM'){
      car = dbFetch(dbSendQuery(con,sprintf("select count(distinct id_assembly) from assembly where producer='%s';",input$producer)))
      
      valueBox("Cars build", car, icon = icon("car"))
    }else{
      shipp = dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_sent='%s' group by producer_received;",input$producer)))
      valueBox("Shipping destinations", shipp, icon = icon("truck"))
    }
  })
  output$profileBox_boxes3 <- renderValueBox({
    boxes = dbFetch(dbSendQuery(con,
                                sprintf("select (select count(*) from transport where producer_sent='%s' or producer_received='%s') + (select count(*) from assembly where producer='%s') + (select count(*) from part where producer='%s');",
                                        input$producer,input$producer,input$producer,input$producer)))
    
      valueBox("Blocks created", boxes, icon = icon("cube"))
  })
  
  
  #---AnaTab----
  output$ana_producer_tabs <- renderUI({
    if(type == 'OEM'){
      part_producer_amount = dbFetch(dbSendQuery(con,sprintf("select count(distinct producer_sent) from transport where producer_received='%s';",input$producer)))
      part_types = dbFetch(dbSendQuery(con,sprintf("select count(distinct Type) from Part
                                                   inner join Transport on Transport.Producer_Sent=Part.Producer
                                                   where Transport.Producer_Received='%s';",input$producer)))
      part_name = dbFetch(dbSendQuery(con,sprintf("select type_assembly from assembly where producer='%s' group by type_assembly;",input$producer)))[['Type_Assembly']]
      part_amount = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly where producer='%s';",input$producer)))
      part_amount_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly where producer='%s' and faulty!='0';",input$producer)))
      part_amount_success = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly where producer='%s' and faulty=='0';",input$producer)))
      part_fail_origin = dbFetch(dbSendQuery(con,sprintf("select producer_sent from transport inner join assembly on transport.id=assembly.id_part where assembly.producer = '%s' and faulty!='0';",input$producer)))[['Producer_Sent']]
      part_fail_destination = dbFetch(dbSendQuery(con,sprintf("select distinct producer_received from transport inner join assembly on transport.id=assembly.id_assembly where transport.producer_sent = '%s' and assembly.faulty!='0';",input$producer)))[['Producer_Received']]
      part_received_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_received='%s' and faulty_received!='0'",input$producer)))
      part_received_fail_origin = dbFetch(dbSendQuery(con,sprintf("select distinct producer_sent from transport where producer_received='%s' and faulty_received!='0'",input$producer)))[['Producer_Sent']]
      part_shipped = dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_sent='%s'",input$producer)))
      part_shipped_success =  dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_sent='%s' and faulty_received=='0'",input$producer)))
      part_shipped_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_sent='%s' and faulty_received!='0'",input$producer)))
      part_shipped_fail_origin = dbFetch(dbSendQuery(con,sprintf("select distinct producer_received from transport where producer_sent='%s' and faulty_received!='0'",input$producer)))[['Producer_Received']]
      component_amount_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly A,assembly c inner join part on part.id=c.id_part where a.Producer = '%s' and a.id_part=c.id_assembly and c.faulty!='0';",input$producer)))
      component_fail_origin = dbFetch(dbSendQuery(con,sprintf("select distinct c.producer from assembly A,assembly c inner join part on part.id=c.id_part where a.Producer = '%s'  and a.id_part=c.id_assembly and c.faulty!='0';",input$producer)))
      fail_assembly = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly A,assembly c inner join part on part.id=c.id_part where a.producer = '%s'  and a.id_part=c.id_assembly and (c.faulty!='0' or a.faulty!='0' or part.faulty!='0');",input$producer)))
      single_part_amount_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly A,assembly c inner join part on part.id=c.id_part where a.producer = '%s' and a.id_part=c.id_assembly and (part.faulty!='0');",input$producer)))
      fail_transport = dbFetch(dbSendQuery(con,sprintf("select count(*) from (select c.id_part as id from assembly A, assembly c where a.Producer = '%s' and a.id_part=c.id_assembly union select c.id_assembly as id from assembly a, assembly c where a.Producer = '%s' and a.id_part=c.id_assembly) as tab inner join transport on tab.id=transport.id where transport.faulty_received!='0';",input$producer,input$producer)))
      single_part_fail_origin = dbFetch(dbSendQuery(con,sprintf("select distinct part.producer from assembly A,assembly c inner join part on part.id=c.id_part where a.producer = '%s' and a.id_part=c.id_assembly and (part.faulty!='0');",input$producer)))[['Producer']]
      using_fauty =  fail_transport + dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly A,assembly c inner join part on part.id=c.id_part where a.producer = '%s' and a.id_part=c.id_assembly and (c.faulty!='0' or part.faulty!='0'); ",input$producer)))
      component_received_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_received='%s' and faulty_received!='0';",input$producer)))
      component_received_fail_origin = dbFetch(dbSendQuery(con,sprintf("select producer_sent from transport where producer_received='%s' and faulty_received!='0';",input$producer)))[['Producer_Sent']]
      
      box(width = 12,
          title = "Data Analytics", status = "warning", collapsible = TRUE, solidHeader = TRUE,
          tabsetPanel(
            #---Map Tab----
            tabPanel("Map",
                     #TODO karte zeigt nur rot=faulty wenn alle faulty! sollte ab einer faulty anzeigen
                     fluidPage(box(width = 8,
                                   title = "", status = "primary", collapsible = TRUE,
                                   leafletOutput("mymap_component_producer_oem")
                     ),
                     box(width = 4,
                         title = "Supply Chains displayed", status = "primary", collapsible = TRUE,
                         textInput("limit", "",
                                   value='5')
                     )),
                     box(width = 12,
                         title = "Data displayed", status = "primary", collapsible = TRUE,
                         DT::dataTableOutput(
                           outputId = "db_table_manu_map_producer_oem"
                         ))
            ),
            #---Production Tab----
            tabPanel("Production",
                     #"display what is produced, faulty rate"
                     box(width = 12,
                         title = "Own Production Details", status = "primary", collapsible = TRUE,
                         fluidPage(
                           infoBox("Producing: ", as.list(part_name), icon = icon("cogs")),
                           infoBox("Build: ", part_amount, icon = icon("wrench"),color = "yellow"),
                           infoBox("Success: ", part_amount_success, icon = icon("thumbs-up"),color = "green"),
                           infoBox("Failures: ", part_amount_fail, icon = icon("thumbs-down"),color = "red"),
                           infoBox("Faulty assemblies built-in: ", using_fauty, icon = icon("exclamation-triangle"), color = "red", fill = TRUE)
                         )),
                     box(width = 12,
                         title = "Transportation failures", status = "danger", collapsible = TRUE,
                         #infoBox("Failures in assemblies: ", fail_assembly, icon = icon("thumbs-down"),color = "red"),
                         infoBox("Failures through transportation: ", fail_transport, icon = icon("truck-loading"),color = "red")
                     ),
                     box(width = 12,
                         title = "Failures due to Part-Product failures", status = "danger", collapsible = TRUE,
                         infoBox("Parts failed: ", single_part_amount_fail, icon = icon("thumbs-down"),color = "red"),
                         # where they're from
                         infoBox("Produced at factory: ", as.list(single_part_fail_origin), icon = icon("industry"),color = "red")
                     ),
                     box(width = 12,
                         title = "Failures due to Product-Component failures", status = "danger", collapsible = TRUE,
                         infoBox("Components failed: ", component_amount_fail, icon = icon("thumbs-down"),color = "red"),
                         # where they're from
                         infoBox("Produced at factory: ", as.list(component_fail_origin), icon = icon("industry"),color = "red")
                     )
            ),
            #---Supplier Tab----
            tabPanel("Supplier",
                     #"display all suppliers, where they are, faulty deliveries faulty parts"
                     box(width = 12, title = "Infobox", status = "warning", collapsible = TRUE,
                         infoBox(width = 6,"Part Producers", part_producer_amount, icon = icon("cog")),
                         infoBox(width = 6,"Different Part Types", part_types, icon = icon("wrench")),
                         infoBox(width = 6,"Shippment failed from part factory: ", part_received_fail, icon = icon("thumbs-down"),color = "red"),
                         infoBox(width = 6,"Failed shippment received from part factory: ", as.list(part_received_fail_origin), icon = icon("shipping-fast"),color = "red"),
                         infoBox(width = 6,"Shippment failed from component factory: ", component_received_fail, icon = icon("thumbs-down"),color = "red"),
                         infoBox(width = 6,"Failed shippment received from component factory: ", as.list(component_received_fail_origin), icon = icon("shipping-fast"),color = "red")
                     ),
                     box(width = 12, title = "Charts", status = "warning", collapsible = TRUE,
                         verticalLayout(plotOutput("Barplot_faulty_component_analysis_part_delivery_oem"))
                     )
            ),
            #---Shipping Tab----
            tabPanel("Shipment to clients",
                     #"display what is shipped where to, faulty rate"
                     box(width = 12,
                         title = "Shipping Details", status = "primary", collapsible = TRUE,
                         fluidPage(
                           infoBox("Shippments: ", part_shipped, icon = icon("shipping-fast")),
                           infoBox("Success: ", part_shipped_success, icon = icon("thumbs-up"),color = "green")
                         )),
                     box(width = 12,
                         title = "Failures", status = "danger", collapsible = TRUE,
                         infoBox("Shippment failed: ", part_shipped_fail, icon = icon("thumbs-down"),color = "red")
                     )
            )
          )
      )
    }
    else if(type == 'Component'){
      # For a component Producer
      part_producer_amount = dbFetch(dbSendQuery(con,sprintf("select count(distinct producer_sent) from transport where producer_received='%s';",input$producer)))
      part_types = dbFetch(dbSendQuery(con,sprintf("select count(distinct Type) from Part
                                                   inner join Transport on Transport.Producer_Sent=Part.Producer
                                                   where Transport.Producer_Received='%s';",input$producer)))
      part_name = dbFetch(dbSendQuery(con,sprintf("select type_assembly from assembly where producer='%s' group by type_assembly;",input$producer)))
      part_amount = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly where producer='%s';",input$producer)))
      part_amount_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from assembly where producer='%s' and faulty!='0';",input$producer)))
      part_amount_success = part_amount - part_amount_fail
      part_fail_origin = dbFetch(dbSendQuery(con,sprintf("select producer_sent from transport inner join assembly on transport.id=assembly.id_part where assembly.producer = '%s' and faulty!='0';",input$producer)))[['Producer_Sent']]
      part_fail_destination = dbFetch(dbSendQuery(con,sprintf("select distinct producer_received from transport inner join assembly on transport.id=assembly.id_assembly where transport.producer_sent = '%s' and assembly.faulty!='0';",input$producer)))[['Producer_Received']]
      part_shipped = dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_sent='%s'",input$producer)))
      part_shipped_success =  dbFetch(dbSendQuery(con,sprintf("select count(*) from transport where producer_sent='%s' and faulty_received=='0'",input$producer)))
      part_shipped_fail = part_shipped - part_shipped_success
      part_shipped_fail_origin = dbFetch(dbSendQuery(con,sprintf("select distinct producer_received from transport where producer_sent='%s' and faulty_received!='0'",input$producer)))[['Producer_Received']]
      box(width = 12,
          title = "Data Analytics", status = "warning", collapsible = TRUE, solidHeader = TRUE,
          tabsetPanel(
            #---Map Tab----
            tabPanel("Map",
                     fluidPage(box(width = 12,
                                   title = "", status = "primary", collapsible = TRUE,
                                   leafletOutput("mymap_component_producer")
                     )),
                     box(width = 12,
                         title = "Data displayed", status = "primary", collapsible = TRUE,
                         DT::dataTableOutput(
                           outputId = "db_table_manu_map_component_producer"
                         ))
            ),
            #---Production Tab----
            tabPanel("Production",
                     box(width = 12,
                         title = "Own Production Details", status = "primary", collapsible = TRUE,
                         fluidPage(
                           infoBox("Producing: ", part_name, icon = icon("cogs")),
                           infoBox("Build: ", part_amount, icon = icon("wrench"),color = "yellow"),
                           infoBox("Success: ", part_amount_success, icon = icon("thumbs-up"),color = "green")
                         )),
                     box(width = 12,
                         title = "Failures", status = "danger", collapsible = TRUE,
                         infoBox("Parts failed: ", part_amount_fail, icon = icon("thumbs-down"),color = "red"),
                         # where they're from
                         infoBox("Produced at factory: ", as.list(part_fail_origin), icon = icon("industry"),color = "red"),
                         # where they#re going
                         infoBox("Please inform these producers: ", as.list(part_fail_destination), icon = icon("shipping-fast"),color = "red")
                     )
            ),
            #---Supplier Tab----
            tabPanel("Part Supplier",
                     box(width = 8, title = "Charts", status = "warning", collapsible = TRUE,
                         splitLayout(verticalLayout(
                           plotOutput("Barplot_faulty_component_analysis_part_producer_general"),
                           plotOutput("Barplot_faulty_component_analysis_part_producer")),
                           verticalLayout(plotOutput("Barplot_faulty_component_analysis_part_type"),
                                          plotOutput("Barplot_faulty_component_analysis_part_delivery"))
                         )),
                     infoBox("Part Producers", part_producer_amount, icon = icon("cog")),
                     infoBox("Different Part Types", part_types, icon = icon("wrench"))
            ),
            #---Shipping Tab----
            tabPanel("Shipment to OEM",
                     #"display what is shipped where to, faulty rate"
                     box(width = 12,
                         title = "Shipping Details", status = "primary", collapsible = TRUE,
                         fluidPage(
                           infoBox("Shippments: ", part_shipped, icon = icon("shipping-fast")),
                           infoBox("Success: ", part_shipped_success, icon = icon("thumbs-up"),color = "green")
                         )),
                     box(width = 12,
                         title = "Failures", status = "danger", collapsible = TRUE,
                         infoBox("Shippment failed: ", part_shipped_fail, icon = icon("thumbs-down"),color = "red"),
                         # where they're from
                         infoBox("Shippment going to factory: ", as.list(part_shipped_fail_origin), icon = icon("industry"),color = "red")
                     )
            )
          )
      )
    }
    else if(type == 'Part'){ #Test with 2173
      # For a part Producer
      part_name = as.list(dbFetch(dbSendQuery(con,sprintf("select type from part where producer='%s' group by type;",input$producer)))[['Type']])
      part_amount = dbFetch(dbSendQuery(con,sprintf("select count(*) from part where producer='%s' group by type;",input$producer)))
      part_amount_success = dbFetch(dbSendQuery(con,sprintf("select count(*) from part where producer='%s' and faulty=='0' group by type;",input$producer)))
      part_amount_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from part where producer='%s' and faulty!='0' group by type;",input$producer))) # 2173
      part_fail_destination = dbFetch(dbSendQuery(con,sprintf("select distinct producer_received from transport inner join assembly on transport.id=assembly.id_assembly where transport.producer_sent = '%s' and assembly.faulty!='0';",input$producer)))[['Producer_Received']]
      part_shipped = dbFetch(dbSendQuery(con,sprintf("select count(*) from Transport inner join Part on transport.ID=Part.ID where Part.producer='%s';",input$producer)))
      part_shipped_success = dbFetch(dbSendQuery(con,sprintf("select count(*) from Transport inner join Part on transport.ID=Part.ID where Part.producer='%s' and faulty_received='0';",input$producer)))
      part_shipped_fail = dbFetch(dbSendQuery(con,sprintf("select count(*) from Transport inner join Part on transport.ID=Part.ID where Part.producer='%s' and faulty_received!='0';",input$producer)))
      part_shipped_fail_origin = dbFetch(dbSendQuery(con,sprintf("select distinct Producer_Received from Transport inner join Part on transport.ID=Part.ID where Part.producer='%s' and faulty_received!='0';",input$producer)))[['Producer_Received']]
      box(width = 12,
          title = "Data Analytics", status = "warning", collapsible = TRUE, solidHeader = TRUE,
          tabsetPanel(
            #---Production Tab----
            tabPanel("Production",
                     #"display what is produced, faulty rate"
                     box(width = 12,
                         title = "Own Production Details", status = "primary", collapsible = TRUE,
                         fluidPage(
                           infoBox("Producing: ", part_name, icon = icon("cogs")),
                           infoBox("Build: ", part_amount, icon = icon("wrench"),color = "yellow"),
                           infoBox("Success: ", part_amount_success, icon = icon("thumbs-up"),color = "green")
                         )),
                     box(width = 12,
                         title = "Failures", status = "danger", collapsible = TRUE,
                         infoBox("Parts failed: ", part_amount_fail, icon = icon("thumbs-down"),color = "red"),
                         # where they#re going
                         infoBox("Please inform these producers: ", as.list(part_fail_destination), icon = icon("shipping-fast"),color = "red")
                     )
            ),
            #---Shipping Tab----
            tabPanel("Shipment to component producers",
                     #"display what is shipped where to, faulty rate"
                     box(width = 12,
                         title = "Shipping Details", status = "primary", collapsible = TRUE,
                         fluidPage(
                           infoBox("Shippments: ", part_shipped, icon = icon("shipping-fast")),
                           infoBox("Success: ", part_shipped_success, icon = icon("thumbs-up"),color = "green")
                         )),
                     box(width = 12,
                         title = "Failures", status = "danger", collapsible = TRUE,
                         infoBox("Shippment failed: ", part_shipped_fail, icon = icon("thumbs-down"),color = "red"),
                         # where they're from
                         infoBox("Shippment going to factory: ", as.list(part_shipped_fail_origin), icon = icon("industry"),color = "red")
                     )
            )
          )
      )
    }
  })
  
  #--- Blockchain Tab block output----
  output$blockchain_first_block <-  renderUI({ 
    if(length(blockchain)>0){      
      box(solidHeader = TRUE, collapsible = TRUE, print_blockchain_text_first(blockchain,blockchain[1]))
    }
  })
  output$blockchain <-  renderUI({ 
    if(length(blockchain)>2){       
      box(solidHeader = TRUE, collapsible = TRUE, print_blockchain_text(blockchain,c(blockchain[length(blockchain)])))     
    }
  })
  
  
  #---sidebar MENU ----
  output$menu <- renderMenu({
    if(input$user_type_setup=="Client"){
      sidebarMenu(
        menuItem("Setup", tabName = "setup", icon = icon("cog", lib = "glyphicon")),
        menuItem("Blockchain", tabName = "bc", icon = icon("link")),
        menuItem("Data", tabName = "data", icon = icon("barcode")),
        menuItem("Analytics", tabName = "ana", icon = icon("chart-bar")),
        menuItem("Profil", tabName = "profil", icon =  icon("address-card")),
        menuItem("Impressum", tabName = "impressum", icon =  icon("user-tie"))
      )
    }#TODO Try troughs error if input$producer dosnt exist yet
    else if(input$user_type_setup=="Producer" && try(input$producer!="Select Producer ID")){
      sidebarMenu(
        menuItem("Setup", tabName = "setup", icon = icon("cog", lib = "glyphicon")),
        menuItem("Blockchain", tabName = "bc", icon = icon("link")),
        menuItem("Data", tabName = "data", icon = icon("barcode")),
        menuItem("Analytics", tabName = "ana_producer", icon = icon("chart-bar")),
        menuItem("Profil", tabName = "profil_producer", icon =  icon("address-card")),
        menuItem("Impressum", tabName = "impressum", icon =  icon("user-tie"))
      )
    }else{
      sidebarMenu(
        menuItem("Setup", tabName = "setup", icon = icon("cog", lib = "glyphicon"))
      )
    }
  })
  
  #--- Observe User-Type Input ----
  observeEvent(input$user_type_setup, {
    if(input$user_type_setup=="Producer"){
      output$number_input<- renderUI({
        num_producer = dbFetch(dbSendQuery(con,
                                           "select producer from part union select producer from assembly group by producer order by producer;"))
        p('Please insert your Producer-ID: ')
        selectInput("producer","", 
                    choices = c("Select Producer ID", num_producer))
      })
    }
  })
  
  #--- Observe User-Producer Number Input ----
  observeEvent(input$producer, {
    if(input$producer=="Select Producer ID"){
      output$whoareyou <- renderUI({
        paste0('')
      })}
    else{
      number <<- input$producer
      output$whoareyou <- renderUI({
        
        num_producer_part = dbFetch(dbSendQuery(con,
                                                "select producer from part group by producer order by producer;"))
        num_producer_oem = dbFetch(dbSendQuery(con,
                                               "select producer from assembly where type_assembly LIKE 'OEM%' group by type_assembly order by type_assembly;"))
        if(toString(input$producer) %in% num_producer_part[['Producer']]){
          type <<- 'Part'
          part_amount = 100
        }else if(toString(input$producer) %in% num_producer_oem[['Producer']]){ 
          type <<- 'OEM'
        }
        else{
          type <<- 'Component'
        }
        paste0("You are: ",type," ",input$user_type_setup," ",input$producer)
        
      })
      removeUI(selector = "div:has(> #user_type_setup)")
      removeUI(selector = "div:has(> #producer)")
    }
    
  })
  
  #--- input$user_type_setup == "Client"----
  observe({ 
    if(input$user_type_setup=="Client"){
      #---Client 1 ----
      output$whoareyou <- renderUI({
        paste0("Welcome to the supply chain! \n",
               "You are: ",input$user_type_setup," ",input$producer)
        
      })
      output$ana <- renderInfoBox({
        box(
          title = "Vehical number", status = "warning", solidHeader = TRUE,
          "Find all producers of your vehical",
          br(), textInput("fahrzeug_id", "Vehical ID:",value='21-2-21-322776')
        )
      })
      
      output$Database_overview <- renderUI({
        tabsetPanel(
          tabPanel("Assembly", DT::dataTableOutput(
            outputId = "db_table_client_1"
          )),
          tabPanel("Transport", DT::dataTableOutput(
            outputId = "db_table_client_2"
          )),
          tabPanel("Everything else", DT::dataTableOutput(
            outputId = "db_table_client_3"
          ))
        )
      })
    }
  })
  
  #--- input$user_type_setup == "Producer" -> Render DATA----
  observe({ 
    if(input$user_type_setup=="Producer" ){
      output$whoareyou <- renderUI({
        paste0("Welcome to the supply chain! \n",
               "You are: ",type, ' ',input$user_type_setup," ",input$producer)
        
      })
      
      output$Database_overview <- renderUI({
        tabsetPanel(
          tabPanel("Table Part", 
                   DT::dataTableOutput(
                     outputId = "db_table_manu_part"
                   )),
          tabPanel("Table Assembly", 
                   DT::dataTableOutput(
                     outputId = "db_table_manu_assembly"
                   )),
          tabPanel("Table Transport", 
                   DT::dataTableOutput(
                     outputId = "db_table_manu_transport"
                   ))
        )
      })
    }
  })
  
  
  #-----Other functionality----
  
  # DATABASE TOOLS ----
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
  
  
  #---Datatables----
  output$db_table_manu_map_producer_oem <- renderDataTable({     
    ort=dbGetQuery(con,sprintf("select 
                               A.Producer as Component_Producer,Assembly.Producer as OEM_Producer, Part.Producer as Part_Producer,
                               Part.Type as Type , 
                               sum(T_component.Faulty_Received) as Faulty_Transport_To_OEM,sum(T_Part.Faulty_Received) as Faulty_Transport_To_Component,
                               sum(T_oem.Faulty_Received) as Faulty_Transport_To_Registration, sum(Part.Faulty) as Faulty_Parts
                               from Assembly as A 
                               inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                               inner join Part on A.ID_Part=Part.ID
                               inner join Transport as T_Part on T_Part.ID=Part.ID
                               inner join Transport as T_component on T_component.ID=A.ID_Assembly
                               inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                               where Assembly.Producer='%s'
                               group by Part_Producer,Type,Component_Producer %s
                               ",input$producer,sprintf("limit %s",input$limit)))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  output$db_table_manu_map_component_producer <- renderDataTable({
    dbGetQuery(con,sprintf("select 
                           Part.Producer as Part_Producer,
                           count(*) as No_Of_Parts, sum(Part.Faulty) as Parts_Faulty,sum(Transport.Faulty_Received) as Transport_Faulty
                           FROM Part 
                           inner join Transport on Transport.ID=Part.ID 
                           where Producer_Received = '%s'
                           group by Part.Producer",
                           input$producer))
  }, 
  options = list(pageLength = 5, width="100%", scrollX = TRUE))
  
  
  # BARPLOTS ----
  #---Barplots Component----
  output$Barplot_faulty_component_analysis_part_producer_general <- renderPlot({ 
    data = dbGetQuery(con,sprintf("select 
                                  Producer as Part_Producer,count(*) as Number_of_Parts_Produced,Type
                                  from Part
                                  inner join Transport on Transport.Producer_Sent=Part.Producer
                                  where Transport.Producer_Received='%s'
                                  group by Part_Producer",input$producer))
    
    # Render a barplot
    barplot(data$Number_of_Parts_Produced,
            main = "Products Produced by Part Producer",
            ylab = "Part Producer Name",
            xlab = "Number of Products Produced",
            names.arg =data$Part_Producer,
            col = "darkred")
    
    #matplot(data, aes(x=data$Part_Producer, y=data$Number_of_Parts_Produced, fill=data$Type)) + 
    #  geom_bar(stat = "identity", position = "dodge")+
    #  xlab("Part Producer Name")+
    #  ylab("Number of Parts Produced")+
    #  ggtitle("Parts Produced by Part Producer")
  })
  output$Barplot_faulty_component_analysis_part_producer <- renderPlot({
    data = dbGetQuery(con,sprintf("select Producer as Part_Producer,Faulty_Received as Transport_Faulty,Type
                                  from Part inner join Transport on Transport.Producer_Sent=Part.Producer 
                                  where Transport.Producer_Received='%s'",input$producer))
    
    # Render a barplot
    barplot(table(data$Transport_Faulty,data$Part_Producer), 
            main="Faulty Products by Part Producer",
            xlab="Part Producer Name", ylab = "Number of Products shipped", col=c("darkgreen","red")) 
    legend("right",
           c("Faults detected by arrival","No Faults detected by delivery"),
           fill = c("red","darkgreen")
    )
    
    #ggplot(data, aes(x=data$Part_Producer, y=data$Transport_Faulty, fill=data$Type)) + 
    #  geom_bar(stat = "identity", position = "dodge")+
    #  xlab("Part Producer Name")+
    #  ylab("Number of faulty Products through transportation")+
    #  ggtitle("Faulty Products by Part Producer")
  })
  
  output$Barplot_faulty_component_analysis_part_type <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Producer as Part_Producer,Faulty as Parts_Faulty, Type
                                  from Part
                                  inner join Transport on Transport.Producer_Sent=Part.Producer
                                  where Transport.Producer_Received='%s' and transport.faulty_received!='0'
                                  ",input$producer))
    
    # Render a barplot
    
    
    barplot(table(data$Parts_Faulty,data$Part_Producer), 
            main="Faulty Products by Producer",
            xlab="Part Producer Name", ylab = "Number of Products", col=c("darkgreen","red")) 
    legend("topright",
           c("Faulty Products","Products fully functional"),
           fill = c("red","darkgreen")
    )
    
    #ggplot(data, aes(x=data$Part_Producer, y=data$Parts_Faulty, fill=data$Type)) + 
    #  geom_bar(stat = "identity", position = "dodge")+
    #  xlab("Part Producer Name")+
    #  ylab("Number of faulty Products")+
    #  ggtitle("Faulty Products by Producer")
  })
  output$Barplot_faulty_component_analysis_part_delivery <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  Distinct Producer as Part_Producer,
                                  julianday(Date_Received)-julianday(Date_sent) as Deliverytime_in_Days
                                  from Part
                                  inner join Transport on Transport.Producer_Sent=Part.Producer
                                  where Transport.Producer_Received='%s'
                                  group by Part_Producer",input$producer))
    
    # Render a barplot
    #plot_ly(data, x = data$Part_Producer, y = data$Deliverytime_in_Days, type = 'bar', 
    #        text = data$Deliverytime_in_Days, textposition = 'auto',
    #        marker = list(color = 'rgb(158,202,225)',
    #                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
    #  layout(title = "Delivery Analysis",
    #         xaxis = list(title = "Part Producer Name"),
    #         yaxis = list(title = "Deliverytime in days"))
    
    barplot(data$Deliverytime_in_Days,
            main = "Delivery Analysis",
            ylab = "Part Producer Name",
            xlab = "Deliverytime in days",
            names.arg =data$Part_Producer,
            col = "darkred")
    
    
    #ggplot(data, aes(x=data$Part_Producer, y=data$Deliverytime_in_Days, fill=data$Type)) + 
    #  geom_bar(stat = "identity", position = "dodge")+
    #  xlab("Part Producer Name")+
    #  ylab("Deliverytime in days")+
    #  ggtitle("Delivery Analysis")
  })
  
  #Barplots OEM----
  output$Barplot_faulty_component_analysis_part_delivery_oem <- renderPlot({
    data = dbGetQuery(con,sprintf("select 
                                  distinct Part.Producer as Part_Producer,
                                  julianday(T_component.Date_Received)-julianday(T_Part.Date_sent) as Deliverytime_in_Days
                                  from Assembly as A 
                                  inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                  inner join Part on A.ID_Part=Part.ID
                                  inner join Transport as T_Part on T_Part.ID=Part.ID
                                  inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                  inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                  where Assembly.Producer='%s'
                                  group by Part_Producer",input$producer))
    
    # Render a barplot
    #plot_ly(x=data$Part_Producer,y=data$Deliverytime_in_Days,type='bar') %>%
    #  layout(title = "Delivery Analysis",
    #         xaxis = list(title = "Part Producer Name"),
    #         yaxis = list(title = "Deliverytime in days from Part producer to OEM factory"))
    
    barplot(data$Deliverytime_in_Days,
            main = "Delivery Analysis",
            xlab = "Part Producer Name",
            ylab = "Deliverytime in days from Part producer to OEM factory",
            names.arg =data$Part_Producer,
            col = "darkred")
    
    #ggplot(data, aes(x=data$Part_Producer, y=data$Deliverytime_in_Days)) + 
    #  geom_bar(stat = "identity", position = "dodge")+
    #  xlab("Part Producer Name")+
    #  ylab("Deliverytime in days from Part producer to OEM factory")+
    #  ggtitle("Delivery Analysis")
  })
  
  
  
  # MAPS ----
  # Karte ----Profile-----
  output$mymap_profile <- renderLeaflet({
    if(type == 'OEM'){
      icon_profile = icon_function('o','green')
    }else if(type == 'Component'){
      icon_profile = icon_function('c','green')
    }else{
      icon_profile = icon_function('t','green')
    }
    #sql----
    ort = dbGetQuery(con,sprintf("select coordinates_sent from transport where producer_sent='%s' limit 1",input$producer))
    render_profile_map(ort, icon_profile, input$producer)
  })
  
  # Karte ----mymap_component_producer-----
  output$mymap_component_producer <- renderLeaflet({
    
    #sql----
    ort_red = dbGetQuery(con,sprintf("select *  from (Select *,sum(Part.Faulty) as Parts_Faulty FROM Part 
                                     inner join Transport on Transport.ID=Part.ID 
                                     where Producer_Received = '%s' group by Part.Producer) as tab where tab.Parts_Faulty>0
                                     ",input$producer))
    ort = dbGetQuery(con,sprintf("select *  from (Select *,sum(Part.Faulty) as Parts_Faulty FROM Part 
                                 inner join Transport on Transport.ID=Part.ID 
                                 where Producer_Received = '%s' group by Part.Producer) as tab where tab.Parts_Faulty==0
                                 ",input$producer))
    ort_red_line = dbGetQuery(con,sprintf("select *  from (Select *,sum(Part.Faulty) as Parts_Faulty FROM Part 
                                          inner join Transport on Transport.ID=Part.ID 
                                          where Producer_Received = '%s' group by Part.Producer) as tab where tab.Faulty_Received>0
                                          ",input$producer))
    
    
    render_complex_map_component(ort, ort_red, ort_red_line)
  })
  
  #--- Karte-Client-----
  output$mymap <- renderLeaflet({
    
    #---sql----
    
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
                                 T_oem.Faulty_Received as OEM_Faulty_Received, A.Faulty as Component_Part_Faulty, 
                                 Assembly.Faulty as OEM_Part_Faulty 
                                 from Assembly as A 
                                 inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                                 inner join Part on A.ID_Part=Part.ID
                                 inner join Transport as T_Part on T_Part.ID=Part.ID
                                 inner join Transport as T_component on T_component.ID=A.ID_Assembly
                                 inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                                 where Assembly.ID_Assembly = '%s';",input$fahrzeug_id))
    if(dim(ort)[1]>0){
      # crope----
      
      setDT(ort)[, paste0("Part_Coordinates_Sent", 1:2) := tstrsplit(Part_Coordinates_Sent, " ")]
      setDT(ort)[, paste0("Component_Coordinates_Sent", 1:2) := tstrsplit(Component_Coordinates_Sent, " ")]
      setDT(ort)[, paste0("OEM_Coordinates_Sent", 1:2) := tstrsplit(OEM_Coordinates_Sent, " ")]
      setDT(ort)[, paste0("OEM_Coordinates_Received", 1:2) := tstrsplit(OEM_Coordinates_Received, " ")]
      a=ort[,c(40,41,23)] # Einzelteil Werk
      b=ort[,c(42,43,30)] # Componenten Werk
      c=ort[,c(44,45,37)] # OEM Werk
      d=ort[,c(46,47,38)] # Zulassungsort
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
      col_K = lapply(ort$Part_Faulty_Received, function(x) if(x==1){'red'}else{'green'})
      col_OEM = lapply(ort$Component_Faulty_Received, function(x) if(x==1){'red'}else{'green'})
      col_Z = lapply(ort$OEM_Faulty_Received, function(x) if(x==1){'red'}else{'green'})
      
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
                   icon =  icon_function('t',if(ort$Part_Faulty=='1'){'red'}else{'green'}),
                   popup = content_T,
                   label = as.character(paste0("Part Producer: ", sep = " ", ort$Part_Producer))
        )%>%
        addMarkers(ort, 
                   lat = as.numeric(ort$Component_Coordinates_Sent2),
                   lng = as.numeric(ort$Component_Coordinates_Sent1),
                   icon =  icon_function('c',if(ort$Component_Part_Faulty=='1'){'red'}else{'green'}),
                   popup = content_K,
                   label = as.character(paste0("Component Producer: ", sep = " ", ort$Component_Producer))
        )%>%
        addMarkers(ort, 
                   lat =as.numeric(ort$OEM_Coordinates_Received2), 
                   lng = as.numeric(ort$OEM_Coordinates_Received1),
                   icon = icon_function('co','green'),
                   popup = content_Zulassung,
                   label = as.character(paste0("Registration: ", sep = " ", ort$OEM_Producer_Received))
        )%>%
        addMarkers(ort, 
                   lat =as.numeric(ort$OEM_Coordinates_Sent2), 
                   lng = as.numeric(ort$OEM_Coordinates_Sent1),
                   icon = icon_function('o',if(ort$OEM_Part_Faulty =='1'){'red'}else{'green'}),
                   popup = content_OEM,
                   label = as.character(paste0("OEM Producer: ", sep = " ", ort$OEM_Producer)))
      
    }
  })
  
  output$mymap_component_producer_oem <- renderLeaflet({
    #sql----
    ort=dbGetQuery(con,sprintf("select *, Faulty as Component_Faulty from (select Assembly.ID_Assembly as OEM_ID, 
                               A.ID_Assembly as Component_ID, A.ID_Part as Part_ID, sum(A.Faulty) as OEM_Faulty_sum, A.Faulty as OEM_Faulty,
                               A.Producer as Component_Producer,
                               A.Build_Date as Component_Build_Date, A.Build_Coordinates as Component_Build_Coordinates,
                               Assembly.Producer as OEM_Producer,Assembly.Build_Date as OEM_Build_Date,
                               Assembly.Build_Coordinates as OEM_Build_Coordinates, Part.Producer as Part_Producer,
                               Part.Faulty as Part_Faulty, Part.Faulty_Date as Part_Faulty_Date, sum(Part.Faulty) as Part_Faulty_sum,
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
                               sum(T_oem.Faulty_Received) as Registration_Faulty_Transport
                               from Assembly as A 
                               inner join Assembly on A.ID_Assembly=Assembly.ID_Part 
                               inner join Part on A.ID_Part=Part.ID
                               inner join Transport as T_Part on T_Part.ID=Part.ID
                               inner join Transport as T_component on T_component.ID=A.ID_Assembly
                               inner join Transport as T_oem on T_oem.ID=Assembly.ID_Assembly
                               where Assembly.Producer='%s'
                               group by Part_Producer,Type,Component_Producer %s) as temp 
                               inner join Assembly as C on temp.Component_ID = C.ID_Assembly",
                               input$producer,sprintf("limit %s",input$limit)))
    
    ort_red_part = ort %>% filter('Part_Faulty_sum' >0)%>% select(c(Part_Coordinates_Sent,Part_Producer,Part_Faulty_sum)) %>% distinct()
    ort_red_component = ort %>% filter(Component_Faulty >0) %>% select(c(Component_Coordinates_Sent,Component_Producer,Component_Faulty)) %>% distinct()
    ort_red_oem = ort %>% filter(OEM_Faulty_sum >0) %>% select(c(OEM_Coordinates_Sent,OEM_Producer))%>% distinct()
    render_complex_map_oem(ort, ort_red_part, ort_red_component, ort_red_oem)
  })
  
  } 

# Run the application ----
shinyApp(ui = ui, server = server)
