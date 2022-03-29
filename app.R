library(shiny)
library(shinythemes)
library(plotly)
library(reshape2)
library(tidyverse)

daten <- mtcars

ui <- navbarPage("LinReg",theme = shinytheme("cerulean"),
                 
                 
                 
                 tabPanel("Einfache lineare Regression",
                          sidebarLayout(
                          sidebarPanel(
                              selectInput('xcol','X Variable -- Regressor', names(daten)),
                              selectInput('ycol','Y Variable -- Regressand', names(daten)),
                              selected = names(daten)[[2]],
                          #Einzeichnen einer Ausgleichsgerade
                          checkboxInput("smooth",label = "Ausgleichsgerade",value = FALSE),
                          #Einblenden des R-Outputs
                          checkboxInput("regoutput", "R-Output", FALSE),
                          verbatimTextOutput("routput")),
                          #withMathJax(),
                          #uiOutput("des"),
                          
                 mainPanel(plotlyOutput("scatterplot",width = "1200px", height = "700px"))
                          )),
                 
                 
                 
                 tabPanel("Multiple lineare Regression",
                          sidebarLayout(
                          sidebarPanel(
                              selectInput('xcol13','X.1 Variable', names(daten)),
                              selectInput('zcol13','X.2 Variable', names(daten)),
                              selectInput('ycol13','Y Variable', names(daten)),
                              selected = names(daten)[[3]],
                          checkboxInput("smooth1",label = "Ausgleichsebene",value = FALSE),
                          checkboxInput("regoutput2", "R-Output", FALSE),
                          verbatimTextOutput("routput2"),
                          ),
                 mainPanel(plotlyOutput("plane",width = "1000px", height = "1000px"))
                 ))
                 
)




# Define server logic 
server <- function(input, output,session) {

    ###Kapitel 12
    
    ##Lineare Regression
    # #Auswahl Element und lineare Modell
    d <- reactive(data.frame(x=daten[,input$xcol],y=daten[,input$ycol]))
    m <- reactive(lm(data = d(), y~x))

    
    #xy-Werte für Auswhal der Ausgleichsgerade
    dpreic <- reactive({
        data.frame(x=seq(min(daten[,input$xcol]),max(daten[,input$xcol]),length.out = 174)) %>%
            mutate(y=(m()[["coefficients"]][["(Intercept)"]]+x*m()[["coefficients"]][["x"]]))
    })
    
    
    #Plot
    output$scatterplot <- renderPlotly({
        plot_ly(daten, x = ~daten[,input$xcol], y = ~daten[,input$ycol]) %>%
            add_markers(color = I("black"), alpha = 0.7) %>%
            layout(xaxis = list(title=input$xcol),yaxis = list(title=input$ycol)) %>% 
            toWebGL()
    })
    
    
    ##Auswahl der Ausgleichgerade
    observe({
        if (input$smooth) {
            plotlyProxy("scatterplot", session) %>%
                plotlyProxyInvoke(
                    "addTraces", 
                    list(
                        x = dpreic()[,1],
                        y = dpreic()[,2],
                        type = "scattergl",
                        mode = "lines",
                        line = list(color = "blue")
                    )
                )
        } else {
            plotlyProxy("scatterplot", session) %>%
                plotlyProxyInvoke("deleteTraces", 1)
        }
    })
    
    ##Auswahl des Routputs
    observe({
        if (input$regoutput){
            output$routput <- renderPrint(summary(m())) 
            
        } else {
            output$routput <- renderPrint(("R-Output:"))
            
            
        }
    })
    
    
    # output$des <- renderUI({
    #     withMathJax("$$ Y_i = \beta_0 + \beta_1 x_i  $$")
    # 
    # })
    # 
    
    
    
    
    ###Kapitel 13
    d1 <- reactive(data.frame(x=daten[,input$xcol13],y=daten[,input$ycol13],z=daten[,input$zcol13]))
    
    m1 <- reactive(lm(data = d1(),z~y+x))
    
    axis_x <- reactive(seq(min(d1()$x), max(d1()$x)))
    axis_y <- reactive(seq(min(d1()$y), max(d1()$y)))
    
    olsp <- reactive(expand.grid(x = axis_x(),y = axis_y(),KEEP.OUT.ATTRS = F))
    
    
    
    olsp1 <- reactive({data.frame(x=olsp()[,1],
                                  y=olsp()[,2]) %>% 
            mutate(z=m1()[["coefficients"]][["(Intercept)"]]+x*m1()[["coefficients"]][["x"]]+y*m1()[["coefficients"]][["y"]])
    })
    
    
    olsp2 <- reactive(acast(olsp1(), olsp1()$y ~ olsp1()$x, value.var = "z"))
    
    
    
    output$plane <- renderPlotly({
        plot_ly(data=daten,z = ~daten[,input$zcol13], x = ~daten[,input$xcol13], y = ~daten[,input$ycol13], opacity = 0.7) %>%
            add_markers(color = I("black"),marker=list(size=3.5)) %>% 
            layout(
                scene = list(
                    xaxis = list(title = input$xcol13),
                    yaxis = list(title = input$ycol13),
                    zaxis = list(title = input$zcol13)
                ))
        
        
    })
    
    observe({
        if (input$smooth1) {
            plotlyProxy("plane", session) %>%
                plotlyProxyInvoke(
                    "addTraces",
                    list(z = olsp2(),
                         x = axis_x(),
                         y = axis_y(),
                         type = "surface",colorscale=list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)")),opacity=0.7
                    ) 
                ) 
        } else {
            plotlyProxy("plane", session) %>%
                plotlyProxyInvoke("deleteTraces", 1)
        }
    })
    
    

    observe({
        if (input$regoutput2){
            output$routput2 <- renderPrint(summary(m1()))

        } else {
            output$routput2 <- renderPrint(("R-Output:"))
                    

        }
    })
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)

