library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)


#load data
setwd("D:/course/sta610/sta610sl")
file = 'bioassay.txt'
data <- read.delim(file, header = TRUE, sep=' ')
data$protocol <- as.factor(data$protocol)
data$uterus <- as.numeric(data$uterus)
data$weight <- as.numeric(data$weight)
data$lab <- as.factor(data$lab)
# remove na
data <- data %>% filter(!is.na(uterus))

# create plots in EDA


ui <- dashboardPage(
    dashboardHeader(title = "Study of Estrogens"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "Introduction", icon = icon("th")),
            menuItem("Data", tabName = "Data", icon = icon("th")),
            menuItem("EDA", tabName = "EDA", icon = icon("th")),
            menuItem("Model", tabName = "Model", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # Introduction
            tabItem(tabName = "Introduction",
                    tags$div(class = "header",
                             align="center",
                             style = "font-size:30px;",
                             tags$p("Ready to take the Shiny tutorial? If so")),
                    h2("this is introduction")
            ),
            
            # Data
            tabItem(tabName = "Data",
                    DT::dataTableOutput("TBL"),
                    h2(align="center", "About Data"),
                    
                    tags$div(
                        tags$ul(
                            tags$li("point 1")
                        )
                    ),
                    
                    tags$div(
                        tags$ul(
                            tags$li("point 2")
                        )
                    )
            ),
            
            # EDA
            # include plots of uterus, loguterus, by prot, weight, EE, ZM against log uterus
            tabItem(tabName = "EDA",
                    fluidRow(
                        plotOutput("EDA_plot", height = 250),
                        
                        column(5,
                               h2("Choose EDA plots"),
                               selectInput("plot_select", label = h3("Select Plot"), 
                                           choices = list("uterus" = 1, 
                                                          "log uterus" = 2, 
                                                          "log uterus by protocol" = 3,
                                                          "log uterus vs weight" = 4,
                                                          "log uterus vs EE" = 5,
                                                          "log uterus vs ZM" = 6,
                                                          "log uterus by labs" = 7), 
                                           selected = 1)
                        )
                        
                    ),
                    
                    h2('Insights'),
                    textOutput("description")
            ),
            
            tabItem(tabName = "Model",
                    h2('Base model'),
                    uiOutput("formula"),
                    h4('The base model are the least complex model that is required to answer the research questions. Models with more features are fitted and computed. The final model
                       is based on the BIC criteria.')
            )
            
        )
    )
)

server <- function(input, output) {
    
    # Data
    output$TBL <- DT::renderDataTable({
        datatable(data)
    }) 
    
    # EDA
    plot <- reactive({
        if (input$plot_select == 1) return(ggplot(data=data, aes(x=uterus))+geom_histogram())
        if (input$plot_select == 2) return(ggplot(data=data, aes(x=log(uterus)))+
                                               geom_histogram())
        if (input$plot_select == 3) return(data %>%
                                               mutate(text = fct_reorder(protocol, log(uterus))) %>%
                                               ggplot( aes(x=log(uterus), color=protocol, fill=protocol)) +
                                               geom_histogram(alpha=0.6) +
                                               xlab("") +
                                               ylab("count") +
                                               labs(x = "log(uterus)")+
                                               facet_wrap(~text))
        if (input$plot_select == 4) return(ggplot(data, aes(x=weight, y=log(uterus)))+
                                               geom_point())
        if (input$plot_select == 5) return(ggplot(data, aes(x=as.factor(EE), y=log(uterus)))+
                                               geom_boxplot()+
                                               labs(x = "EE"))
        if (input$plot_select == 6) return(ggplot(data, aes(x=as.factor(ZM), y=log(uterus)))+
                                               geom_boxplot()+
                                               labs(x = "ZM"))
        if (input$plot_select == 7) return(ggplot(data, aes(x=reorder(lab, log(uterus), median), y=log(uterus)))+
                                               geom_boxplot()+
                                               theme(axis.text.x=element_text(angle=30, hjust=1))+
                                               labs(x = "lab"))
    })
    
    des <- reactive({
        if (input$plot_select == 1) return('The distribution of uterus is extremely skewed. It might not be a good idea to use it as target variable')
        if (input$plot_select == 2) return('The distribution of log(uterus) is more balanced. However, it is bimodal and I believe this is due to protocol since mature rats tend to have heavier uterus')
        if (input$plot_select == 3) return('d3')
        if (input$plot_select == 4) return('d4')
        if (input$plot_select == 5) return('d5')
        if (input$plot_select == 6) return('d6')
        if (input$plot_select == 7) return('d7')
    })
    
    output$EDA_plot <- renderPlot({
        dataplots = plot()
        print(dataplots)
    })
    
    output$description <- renderText({ 
        des()
    })
    
    # model
    output$formula <- renderUI({
        b0 = '\\beta_0=b_0+b_{0j}+b_{0k}\\\\'
        b1 = '\\beta_{1k} = b_1+b_{1k}\\\\'
        b2 = '\\beta_{2k} = b_2+b_{2k}\\\\'
        withMathJax(paste0("$$log(uterus_{ijk}) = \\beta_0+\\beta_{1k}EE_{ijk}+\\beta_{2k}ZM_{ijk}\\\\",b0,b1,b2,"$$"))
    })
}

shinyApp(ui, server)
