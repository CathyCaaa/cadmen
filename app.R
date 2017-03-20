load("DataSources_16Aug.RData")
library(shiny)
library(shinydashboard)
library(devtools)
library(xts)
library(dplyr)
library(googleVis)
library(dygraphs)
library(treemap)
library(ggplot2)
library(plotly)
library(reshape2)
library(sunburstR)

# Load auxiliary functions (local and the chartconstant package which is not on CRAN)
source("build_charts_functions.R", local = TRUE)
source("plot_functions.R", local = TRUE)
source("utils.R", local = TRUE)
source("https://raw.githubusercontent.com/mick001/chartconstants/master/chartconstants/R/auxiliary_functions.R")
source("https://raw.githubusercontent.com/mick001/chartconstants/master/chartconstants/R/constant_functions.R")
source("https://raw.githubusercontent.com/mick001/chartconstants/master/chartconstants/R/main_function.R")
default_df <- mtcars

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title="虎門科技有限公司",titleWidth = 230),
  dashboardSidebar(
    radioButtons("radio", "Choose visualization period:",
                 c("Last 3 months" = "90",
                   "Last 6 months" 
                   = "180")),
    sidebarMenu(
      menuItem("首頁", tabName = "homepage", icon = icon("telegram")),
      menuItem("帳號登入", tabName = "login", icon = icon("user-circle")),
      menuItem("儀表板", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("供應商管理", tabName = "supplier", icon = icon("group"),
               menuSubItem("SPC品質管制",tabName = "spc",icon = icon("check-square")),
               menuSubItem("SPC品質管制_2",tabName = "sqc",icon = icon("check-square")),
               menuSubItem("SPC品質管制_3",tabName = "sqm",icon = icon("check-square")),
               menuSubItem("SPC品質管制_4",tabName = "scc",icon = icon("check-square")),
               menuSubItem("SPC品質管制_5",tabName = "sc",icon = icon("check-square")),
               menuSubItem("SPC品質管制_6",tabName = "sb",icon = icon("check-square")),
               menuSubItem("SPC品質管制_7",tabName = "sa",icon = icon("check-square"))
      ),
      menuItem("範例一", href="https://johnyagecic.shinyapps.io/BoatRunExplorer/",  icon = icon("th")),
      menuItem("範例二",href="https://johnyagecic.shinyapps.io/ManningsMC/",icon = icon("th")),
      menuItem("範例三", href="https://jbkunst.shinyapps.io/highcharter-googleanalytics/", icon = icon("th")),
      menuItem("範例四", tabName = "four", icon = icon("th")),
      menuItem("圖控系統", tabName = "control", icon = icon("cogs")),
      menuItem("生產工單管理", tabName = "production", icon = icon("file-text-o")),
      menuItem("全廠監控", tabName = "insight", icon = icon("eye")),
      menuItem("全廠能源管理", tabName = "energy", icon = icon("circle-o-notch")),
      menuItem("日月季報表", tabName = "report", icon = icon("line-chart")),
      menuItem("庫存分析預測", tabName = "analysis", icon = icon("shopping-cart")),
      menuItem("零件健康狀態監控", tabName = "health", icon = icon("wrench")),
      menuItem("機台壽命預測", tabName = "life", icon = icon("window-restore")),
      menuItem("Website", href="http://www.cadmen.com/Page/", icon = icon("th"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("sessionBox"),
                valueBoxOutput("goalBox"),
                valueBoxOutput("goalCRBox")
              ),
              fluidRow(
                box(title="Main KPI's trend (sessions & transactions on left axis; conversion rate on right axis)",
                    status="primary",solidHeader = TRUE,dygraphOutput("dygraph"), height=480, width=12),
                box(title="Channels Performance (Transactions = size of bubbles)",status="warning",solidHeader = TRUE,
                    htmlOutput("chart2"),width=12,height=450),
                
                box(title="Devices & OS (Sessions = size of rectangles)",status="danger",solidHeader = TRUE,
                    plotOutput("tree"), height=500)
              )
      ),
      tabItem((tabName = "spc"),
              sidebarPanel(
                fileInput(inputId = "uploaded_file",
                          label = "Upload a .csv file:"),
                # Select chart type
                selectInput(inputId = "plot_type",
                            label = "SPC管制圖:",
                            choices = c("xbar_s", "s", "xbar_r", "r","p","np","c","u")),
                selectInput(inputId = "plot_rule",
                            label = "檢定規則:",
                            choices = c("Test1","Test2","Test3","Test4","Test5","Test6","Test7","Test8")),
                # Select group size
                numericInput(inputId = "group",
                             label = "Group size:",
                             value = 2,
                             min = 2,
                             step = 1
                ),
                # Select number of sigmas for chart boundaries
                numericInput(inputId = "n_sigma",
                             label = "設定標準差:",
                             value = 3,
                             min = 1,
                             max = 10,
                             step = 0.25
                ),
                # Available variables
                uiOutput('variables')
              ),
              mainPanel(
                
                # Chart plot
                plotOutput(outputId = "chartplot")),
              
              # Table output
              dataTableOutput("data_table"))
                
              )
  )
)




server <- function(input, output) {
  
  ####Dasshboard
  
  output$sessionBox <- renderValueBox({
    valueBox(
      format(sum(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),sessions)),format="d",big.mark=","), 
      "Sessions", icon = icon("area-chart"), color = "blue")
  })
  
  output$goalBox <- renderValueBox({
    valueBox(
      format(sum(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),goal10Completions)),format="d",big.mark=","), 
      "Transactions", icon = icon("shopping-cart"), color = "blue")
  })
  
  output$goalCRBox <- renderValueBox({
    valueBox(
      paste(round(mean(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),goal10ConversionRate)[,1]),2),"%"), "Conversion rate", 
      icon = icon("shopping-cart"), color = "blue")
  })
  
  
  output$chart2<- renderGvis({
    
    ga3Sub<-select(subset(ga3, date>=max(ga3$date)-as.numeric(input$radio)),-date)
    by_channel <- group_by(ga3Sub, channelGrouping)
    ga3Sub<-summarise(by_channel, sum(sessions), mean(pageviewsPerSession),sum(goal10Completions))
    names(ga3Sub)<-c("channelGrouping","sessions","pageviewsPerSession","goal10Completions")
    Bubble <- gvisBubbleChart(ga3Sub, idvar="channelGrouping", 
                              xvar="sessions", yvar="pageviewsPerSession",
                              colorvar="channelGrouping", sizevar="goal10Completions",
                              options=list( 
                                #title="Channels Performance (Transactions = Size of the Bubble)",
                                vAxis="{title: 'Pages per Session'}",
                                hAxis="{title: 'Sessions'}",
                                width=990, height=350,
                                legend = T))
    Bubble
  })
  
  output$dygraph <- renderDygraph({
    
    ga1Sub<-subset(ga1, date>=max(ga1$date)-as.numeric(input$radio))
    
    ses <- zoo(ga1Sub$sessions,ga1Sub$date)
    con<-zoo(ga1Sub$goal10Completions,ga1Sub$date)
    cr<-zoo(ga1Sub$goal10ConversionRate,ga1Sub$date)
    ga2Sub<-cbind(ses, con,cr)
    
    
    dygraph(ga2Sub)%>% dySeries("ses", label = "sessions",axis="y",stepPlot=TRUE,fillGraph=TRUE) %>% dySeries("con",fillGraph=TRUE, label = "transactions",axis="y") %>% dySeries("cr", label = "conversion rate",axis="y2",strokePattern = "dashed") %>% dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% dyLegend(width = 400)%>% dyRangeSelector()
  })
  
  output$tree <- renderPlot({
    
    ga4Sub<-subset(ga4, date>=max(ga4$date)-as.numeric(input$radio))
    treemap(ga4Sub, 
            index=c("deviceCategory","operatingSystem"), 
            vSize="sessions", 
            type="index",fontsize.labels = c(12,9))
  })
  
  ####SPC
  
  load_file <- reactive({
    file <- input$uploaded_file
    file
  })
  
  # Get variables names reactive
  get_var_names <- reactive({
    file <- load_file()
    if( is.null(file) )
    {
      var_names <- names(default_df)
      
    }else
    {
      var_names <- names(read.csv(file$datapath))
      var_names
    }
  })
  
  # Render available variables to be used
  output$variables <- renderUI({
    choices <- get_var_names()
    selectInput(inputId = "my_var", label = "Choose variable:", choices = choices)
  })
  
  # Render plot
  output$chartplot <- renderPlot({
    
    # Load file
    file <- load_file()
    
    if( is.null(file) )
    {
      data <- default_df
    }else
    {
      data <- read.csv(file$datapath)
    }
    
    y <- input$my_var
    
    data <- data %>% select_(.dots = list(y = y))
    # Return chart data
    plot_data <- control_chart(type = input$plot_type,
                               data = data,
                               group = input$group,
                               n_sigma = input$n_sigma)
    # Return plot
    pl <- plot_chart(plot_data = plot_data, plot_type = input$plot_type, var_name = y)
    # Print out plot
    print(pl)
    
  })
  # Data table
  output$data_table <- renderDataTable({
    
    # Load file
    file <- load_file()
    
    if( is.null(file) )
    {
      data <- default_df
    }else
    {
      data <- read.csv(file$datapath)
    }
    
    y <- input$my_var
    
    data <- data %>% select_(.dots = list(y = y))
    
    # Return chart data
    plot_data <- control_chart(type = input$plot_type,
                               data = data,
                               group = input$group,
                               n_sigma = input$n_sigma)
    # Format data as data.frame
    out_df <- as.data.frame(plot_data)
    # Return to table
    out_df
  })
}

shinyApp(ui, server)
