

library(shiny)
library(shinydashboard)
library(RMySQL)
library(DT)
library(ggplot2)

#  database Connect
drv <- dbDriver("MySQL")
con <- dbConnect(drv, dbname = "dbname",
                 host = "localhost",
                 port = 3306,
                 user = "user",
                 password = "password"
)

#querys for select input
query_operacion <- 'SELECT topr_id,topr_descripcion FROM realtor_tbl_tipo_operacion WHERE topr_estatus = 1'

#query for infobox
query_infobox <- 'SELECT count(regv_id) AS ordenes, SUM(regv_precio) AS ventas, SUM(regv_superficie) AS superficie
FROM realtor_tbl_reg_ventas'

varoperacion <- dbGetQuery(con,query_operacion)
varinfobox <- dbGetQuery(con,query_infobox)


dbDisconnect(con)

# Define UI for application that draws a histogram
ui <- dashboardPage(
                    dashboardHeader(title = "Panel de Ventas"
                                    ),
                    dashboardSidebar(
                                    sidebarMenu(
                                                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                                         
                                                selectInput( inputId = 'opr_type_id',
                                                             label = 'Tipo Operacion:',
                                                             choices = varoperacion$topr_descripcion,
                                                             selected = 'Alquiler'
                                                             )

                                                #menuItem("Widgets", tabName = "widgets", icon = icon("th"))
                                    )
                    ),
                    dashboardBody(
                                    fluidRow(
                                        valueBoxOutput("ventascantidadbox"),
                                        valueBoxOutput("ventasmontobox"),
                                        valueBoxOutput("superficiebox"),
                                        hr(),
                                        column(width = 6,
                                               box(
                                                   title = "Resumen Inmuebles" , 
                                                   solidHeader = TRUE, 
                                                   status = "primary",
                                                   width = 12,
                                                   plotOutput(
                                                       outputId = 'plotchartventas'
                                                   )
                                               )
                                        ),
                                        column(width = 6,
                                               box(
                                                   title = "Resumen Vendedor", 
                                                   solidHeader = TRUE, 
                                                   status = "primary",
                                                   width = 12,
                                                   plotOutput(
                                                       outputId = 'plotchartvendedor'
                                                   )
                                               )
                                        ),
                                    ),
                                    fluidRow(
                                             box(
                                                 title = "Resumen Ventas", 
                                                 status = "warning",
                                                 width = 12,
                                                 dataTableOutput(
                                                    outputId = 'ventas_transactions',
                                                    height = 150
                                                 )
                                             )
                                        
                                    )
                        
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #selectinput + dynamic query
    filtered_data <- reactive({ 
        
        #  database Connect
        drv <- dbDriver("MySQL")
        con2 <- dbConnect(drv, dbname = "dbname",
                 host = "localhost",
                 port = 3306,
                 user = "user",
                 password = "password"
        )
        
        #querys for select data
        operacion_code <- input$opr_type_id
        query_data <- (paste0("SELECT a.regv_fecha_alta, b.col_nombre, c.prv_descripcion, d.tinm_descripcion, e.topr_descripcion, a.regv_superficie, a.regv_precio, a.regv_fecha_venta
                 FROM realtor_tbl_reg_ventas a
                 LEFT JOIN realtor_tbl_colaboradores b ON a.regv_vendedor = b.col_id 
                 LEFT JOIN realtor_tbl_provincias c ON a.regv_provincia = c.prv_id
                 LEFT JOIN realtor_tbl_tipo_inmueble d ON a.regv_tipo_inmueble = d.tinm_id 
                 LEFT JOIN realtor_tbl_tipo_operacion e ON a.regv_tipo_operacion = e.topr_id 
                 WHERE e.topr_descripcion = '", operacion_code ,"'"))
        
        
        ventas_data<-dbGetQuery(con2,query_data)
        
        dbDisconnect(con2)
        
        return(ventas_data)
        
    })
    
    #datatable  
    output$ventas_transactions <- renderDataTable(
        
        
        filtered_data() %>% DT::datatable(options = list(lengthMenu = list(c(7,15, -1),c('5','10','Todos')),
                                                         pageLength = 10),
                                          colnames = c('Fecha Alta', 'Vendedor', 'Provincia', 'T. Inmueble', 'T. Operacion', 'Superficie', 'Monto Venta', 'Fecha Venta'),
                                          filter = "top",
                                          selection = "multiple",
                                          style = "bootstrap")
        
    )

    
    #valbox cantidad de ventas
    output$ventascantidadbox <- renderValueBox({
        valueBox(
            format((as.numeric(varinfobox$ordenes)), nsmall=2, big.mark=","),
            "Inmuebles:", icon = icon("list"),
            color = "blue",
            width = 820
        )
    })
    
    #valbox monto total de ventas
    output$ventasmontobox <- renderValueBox({
        valueBox(
            format((as.numeric(varinfobox$ventas)), nsmall=2, big.mark=","),
            "Dolares:", icon = icon("money"),
            color = "green",
            width = 820
        )
    })
    
    #valbox superficie total
    output$superficiebox <- renderValueBox({
        valueBox(
            format((as.numeric(varinfobox$superficie)), nsmall=2, big.mark=","),
             "M2:", icon = icon("area-chart"),
            color = "yellow",
            width = 820
        )
    })
    
    #plot ventas 
    output$plotchartventas <- renderPlot(
        {
            #  database Connect
            drv <- dbDriver("MySQL")
            con3 <- dbConnect(drv, dbname = "dbname",
                 host = "localhost",
                 port = 3306,
                 user = "user",
                 password = "password"
            )
            
            #querys for plot data
            operacion_code <- input$opr_type_id
            query_data_plot <- (paste0("SELECT COUNT(a.regv_id) as qty, c.tinm_descripcion as inmueble
                                        FROM realtor_tbl_reg_ventas a
                                        LEFT JOIN realtor_tbl_tipo_operacion b ON a.regv_tipo_operacion = b.topr_id 
                                        LEFT JOIN realtor_tbl_tipo_inmueble c ON a.regv_tipo_inmueble = c.tinm_id
                                        WHERE b.topr_descripcion = '", operacion_code ,"'
                                        GROUP BY c.tinm_descripcion"))

            ventasplot_data<-dbGetQuery(con3,query_data_plot)
            
            dbDisconnect(con3)
            
            ggplot(data = ventasplot_data, aes(x = inmueble, y = qty)) +
                geom_col(size = 16) +
                labs(x = 'Tipo Inmueble', y = 'QTY') +
                theme_light(base_size = 16) + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                theme(legend.position = 'none')
        }
    )
    
    #plot vendedor 
    output$plotchartvendedor <- renderPlot(
        {
            #  database Connect
            drv <- dbDriver("MySQL")
            con4 <- dbConnect(drv, dbname = "dbname",
                 host = "localhost",
                 port = 3306,
                 user = "user",
                 password = "password"
            )
            
            #querys for plot data
            operacion_code <- input$opr_type_id
            query_vendedores_plot <- (paste0("SELECT COUNT(a.regv_id) as qty, d.col_nombre as vendedor
                                              FROM realtor_tbl_reg_ventas a
                                              LEFT JOIN realtor_tbl_tipo_operacion b ON a.regv_tipo_operacion = b.topr_id 
                                              LEFT JOIN realtor_tbl_colaboradores d ON	a.regv_vendedor = d.col_id
                                              WHERE b.topr_descripcion = '", operacion_code ,"'
                                              GROUP BY d.col_nombre"))
            
            vendedoresplot_data<-dbGetQuery(con4,query_vendedores_plot)
            
            dbDisconnect(con4)
            
            ggplot(data = vendedoresplot_data, aes(x = vendedor, y = qty)) +
                geom_col(size = 16) +
                labs(x = 'Vendedor', y = 'QTY') +
                theme_light(base_size = 16) + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                theme(legend.position = 'none')
        }
    )

   
}

# Run the application 
shinyApp(ui = ui, server = server)
