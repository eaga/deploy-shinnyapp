library(shiny)
library(shinydashboard)
library(RMySQL)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(maps)
library(leaflet)


#  database Connect
drv <- dbDriver("MySQL")
con <- dbConnect(drv, dbname = "freedbtech_dbeaga",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "password"
)

#querys for select input
query_continente <- 'SELECT DISTINCT(Continent) as continente FROM world_tbl_country'

#query for infobox
query_infobox <- 'SELECT sum(Population) AS poblacion, SUM(SurfaceArea) AS superficie 
FROM world_tbl_country'

varcontinente <- dbGetQuery(con,query_continente)
varinfobox <- dbGetQuery(con,query_infobox)

dbDisconnect(con)


ui <- dashboardPage(skin = "black",
      dashboardHeader(title = "Poblacion Mundial",
                      dropdownMenu(type = "messages",
                                   messageItem(
                                     from = "Inteligencia de Negocios",
                                     message = "Informacion Actualizada."
                                   ),
                                   messageItem(
                                     from = "Seguridad Informatica",
                                     message = "Bienvenido",
                                     icon = icon("question"),
                                     time = format(Sys.time(), "%Y %m %d")
                                   ),
                                   messageItem(
                                     from = "Soporte Data Science",
                                     message = "Servidor R 100% Operacional.",
                                     icon = icon("life-ring"),
                                     time = format(Sys.time(), "%H %M %S")
                                   )
      )
      ),
      dashboardSidebar(sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                         label = "Buscar..."),
                       sidebarMenu(
                                   menuItem("Menu", tabName = "dashboard", icon = icon("dashboard")),
                                   selectInput( inputId = 'continente_id',
                                                label = 'Continente:',
                                                choices = varcontinente$continente,
                                                selected = 'North America'
                                   ),
                                   uiOutput("secondSelection")

                       )
         
      ),
      dashboardBody(
                     fluidRow(
                              align = 'center',
                              column(width = 6,
                                     fluidRow(
                                              box(
                                              title = "Poblacion Mundial" , 
                                              solidHeader = TRUE, 
                                              status = "info",
                                              width =  NULL,
                                              infoBoxOutput("poblacionmundialbox")
                                              )
                                     ),
                                     fluidRow(
                                              box(
                                              title = "Superficie Mundial" , 
                                              solidHeader = TRUE, 
                                              status = "info",
                                              width = NULL,
                                              infoBoxOutput("superficiemundialbox")
                                              )
                                     )
                               ),
                              column(width = 6,
                                     box(
                                        title = "Poblacion Continente" , 
                                        solidHeader = TRUE, 
                                        status = "success",
                                        width = NULL,
                                        plotOutput(
                                           outputId = 'plotchartpoblacion'
                                        )
                                     )
                              ),
                              hr(),
                        ),
                        fluidRow(
                                 align = 'center',
                                 column(width = 6,
                                        box(
                                           title = "Paises Continente" , 
                                           solidHeader = TRUE, 
                                           status = "warning",
                                           width = NULL,
                                           dataTableOutput(
                                              outputId = 'paises_continente',
                                              height = 150
                                           )
                                           
                                        )
                                        
                                 ), 
                                 column(width = 6,
                                        fluidRow(
                                                 box(
                                                    title = "Detalles:" , 
                                                    solidHeader = TRUE, 
                                                    status = "danger",
                                                    width = NULL,
                                                    dataTableOutput(
                                                       outputId = 'distrito_pais',
                                                       height = 150
                                                    )
                                                 )
                                        ),
                                        fluidRow(
                                                 box(
                                                 title = "Detalles:" , 
                                                 solidHeader = TRUE, 
                                                 status = "danger",
                                                 width = NULL,
                                                 plotlyOutput(
                                                    outputId = 'paismap', width='100%', height='550px' )
                                                )
                                        )
                                        
                                        
                                 )
                           
                        )
                        
      )
)

server <- function(input, output) { 
   
                                    #valbox poblacion mundial
                                    output$poblacionmundialbox <- renderInfoBox({
                                       infoBox(
                                                "Poblacion:", 
                                                format((as.numeric(varinfobox$poblacion)), nsmall=2, big.mark=","),
                                                icon = icon("users"),
                                                color = "blue"
                                       )
                                    })
                                    
                                    #valbox superficie mundial
                                    output$superficiemundialbox <- renderValueBox({
                                       infoBox(
                                                "Superficie:",
                                                format((as.numeric(varinfobox$superficie)), nsmall=2, big.mark=","), 
                                                icon = icon("globe"),
                                                color = "yellow",
                                       )
                                    })
                                    
                                    #plot poblacion 
                                    output$plotchartpoblacion <- renderPlot(
                                       {
                                          #  database Connect
                                          drv <- dbDriver("MySQL")
                                          con4 <- dbConnect(drv, dbname = "freedbtech_dbeaga",
                                                            host = "localhost",
                                                            port = 3306,
                                                            user = "root",
                                                            password = "password"
                                          )
                                          
                                          #querys for plot data
                                          query_poblacion_plot <- 'SELECT sum(Population) AS poblacion ,Continent
                                                                   FROM world_tbl_country
                                                                   GROUP BY Continent'
                                          
                                          poblacionplot_data<-dbGetQuery(con4,query_poblacion_plot)
                                          
                                          dbDisconnect(con4)
                                          
                                          ggplot(data = poblacionplot_data, aes(x = Continent, y = format((as.numeric(poblacion)), nsmall=2, big.mark=","))) +
                                             geom_col(size = 14) +
                                             labs(x = 'Continente', y = 'Poblacion') +
                                             theme_light(base_size = 14) +
                                             theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                                             theme(legend.position = 'none')
                                       }
                                    )
                                    
                                    # Second SelectInput
                                    output$secondSelection <- renderUI({
                                       
                                       #  database Connect
                                       drv <- dbDriver("MySQL")
                                       con3 <- dbConnect(drv, dbname = "freedbtech_dbeaga",
                                                         host = "localhost",
                                                         port = 3306,
                                                         user = "root",
                                                         password = "password"
                                       )
                                       
                                       #querys for second SelectInput 
                                       continente_code <- input$continente_id
                                       query_data <- (paste0("SELECT Name
                                                              FROM world_tbl_country 
                                                              WHERE Continent = '", continente_code ,"'
                                                              ORDER BY NAME"))

                                       varcountry <-dbGetQuery(con3,query_data)
                                       
                                       dbDisconnect(con3)
                                       
                                       selectInput( inputId = 'country_id',
                                                    label = 'Paises:',
                                                    choices = varcountry$Name,
                                                    selected = 'Anguilla'
                                       )
                                      
                                    })
                                    
                                    #selectinput + dynamic query
                                    filtered_data_paises <- reactive({ 
                                       
                                       #  database Connect
                                       drv <- dbDriver("MySQL")
                                       con2 <- dbConnect(drv, dbname = "freedbtech_dbeaga",
                                                         host = "localhost",
                                                         port = 3306,
                                                         user = "root",
                                                         password = "password"
                                       )
                                       
                                       #querys for select data
                                       paises_continente_code <- input$continente_id
                                       query_data <- (paste0("SELECT NAME,Region,SurfaceArea,Population
                                                              FROM world_tbl_country 
                                                              WHERE Continent = '", paises_continente_code ,"'
                                                              ORDER BY NAME"))

                                       paises_continente_data<-dbGetQuery(con2,query_data)
                                       
                                       dbDisconnect(con2)
                                       
                                       return(paises_continente_data)
                                       
                                    })
                                    
                                    #datatable  
                                    output$paises_continente <- renderDataTable(
                                       
                                       
                                       filtered_data_paises() %>% DT::datatable(options = list(lengthMenu = list(c(7,15, -1),c('5','10','Todos')),
                                                                                        pageLength = 10),
                                                                         colnames = c('Pais', 'Region', 'Superficie', 'Poblacion'),
                                                                         filter = "top",
                                                                         selection = "multiple",
                                                                         style = "bootstrap")
                                       
                                    )
                                    
                                    
                                    #paismap
                                    output$paismap <- renderPlotly(
                                       {
                                          map_data("world", input$country_id) %>%
                                             group_by(group) %>%
                                             plot_geo(x = ~long, y = ~lat) %>%
                                             add_markers(size = I(1))

                                             
                                       } 
                                    )
                                    
                                    
                                    #second selectinput + dynamic query
                                    filtered_data_distrito <- reactive({ 
                                       
                                       #  database Connect
                                       drv <- dbDriver("MySQL")
                                       con1 <- dbConnect(drv, dbname = "freedbtech_dbeaga",
                                                         host = "localhost",
                                                         port = 3306,
                                                         user = "root",
                                                         password = "password"
                                       )
                                       
                                       #querys for select data
                                       pais_code <- input$country_id
                                       query_data <- (paste0("SELECT a.District,a.Population
                                                              FROM world_tbl_city a
                                                              LEFT JOIN world_tbl_country b ON  a.CountryCode = b.Code
                                                              WHERE b.Name = '", pais_code ,"'"))
                                       
                                       distrito_pais_data<-dbGetQuery(con1,query_data)
                                       
                                       dbDisconnect(con1)
                                       
                                       return(distrito_pais_data)
                                       
                                    })
                                    
                                    #datatable distrito por pais
                                    output$distrito_pais <- renderDataTable(
                                       
                                       
                                       filtered_data_distrito() %>% DT::datatable(options = list(lengthMenu = list(c(7,15, -1),c('5','10','Todos')),
                                                                                        pageLength = 10),
                                                                         colnames = c('Distrito', 'Poblacion'),
                                                                         filter = "top",
                                                                         selection = "multiple",
                                                                         style = "bootstrap")
                                       
                                    )
                                    
                                    
}

shinyApp(ui, server)
