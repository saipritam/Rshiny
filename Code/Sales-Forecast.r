library(moments)
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(scales)
library(shinydashboard)
library(graphics)
library(shinyWidgets)
library(e1071)
library(gridExtra)
library(grid)
library(forecast)
library(lubridate)
library(dplyr)
library(TTR)
library(tseries)


plot.new()
# library(plotly)
getwd()
setwd('C:\\Users\\saipritam.pradhan\\Desktop\\R\\')
bcl <- read.csv("data.csv", stringsAsFactors = FALSE)
View(bcl)

df <- subset(bcl, select = -c(latitude_dgr,longitude_dgr,Dept_ID,Dept,Product_ID,Date,Store_ID,Product_ID,MonthYear,cal_date,Dept_Name,Cust_ID,Close_Date,Cust_Cell,Store_Cell,Street_address))
View(df)
str(df)
df$Class<- as.factor(df$Class)
df<-df %>% mutate(sales = Price * num_units)
df$cal_month_year = year(df$saleDate)
# df1<- subset(df, select = c(Class,Product_Name,store,cal_month,cal_month_year,cal_qtr_nbr))
# for (x in colnames(df1)){
#     df1[,x] <- as.factor(df1[,x])
#   }

df1 <- df
ui <- fluidPage(shinythemes::themeSelector(),
  titlePanel("Demand Forecasting"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 40, 60, c(50, 55), pre = "$"),
      radioButtons("classInput", "class",
                   choices = c("Vaccuum cleaners", "TVs", "Washing machines", "Refrigerators"),
                   selected = "TVs"),
      dateRangeInput("dateInput","saleDate","2012-07-25","2014-04-14","2011-01-01","2017-12-31"),
      selectInput("storeInput", "store",
                  choices = c("S1", "S2", "S3","S4")),
      # adding the new div tag to the sidebar            
      tags$div(class="header", checked=NA,
               tags$p("Want to see our representation? If so"),
               tags$a(href="https://aops1.mu-sigma.com/thd/#!/consumptionviews", "Click Here!")
              ),
      br(),
      tags$div(class="header", checked=NA,
               tags$p("Contact Us at "),
               tags$a(href="https://www.mu-sigma.com/", "Click Here!")
      )
    ),
    

  #),
    mainPanel(
      tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #173e43;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #3fb0ac;
                              font-size: 200%;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #173e43;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #173e43;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .active a{
                              background-color: #ff0000;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #173e43;
                              color: #fff;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #f6f1ed;
                              color: #16174f;
                              font-size: 150%;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #d62739;
                              font-size: 100%;
                              color: #000000;
                              }
                              
                              '))),
      # titlename <- function() {
      #   if(input$variableInput=="sales"){x="Sales UniVariate Plot"
      #   return(x)}
      #   else if(input$variableInput=="num_units"){x="Num_units UniVariate Plot"
      #   return(x)}
      #   else if(input$variableInput=="price"){x="Price UniVariate Plot"
      #   return(x)}
      # } 

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Overview",fluidRow(
                    column(12, align = 'center', h1("STARTECH",style = "color:maroon; font-size:50px;"),
                           box(title = "About",status = "primary",
                               solidHeader = T,
                               HTML(paste(
                                 p("STARTECH, US based multinational retailer, headquartered in Illinois. The market leader in Grocery and General Merchandise products."
                                   , style = "color:purpel; font-size:25px;")
                               )
                               ),
                               width=12,collapsible=TRUE,collapsed = TRUE),
                           box( title = "Services ", 
                                width = 12,collapsible=TRUE,collapsed = T,status = "primary",
                                HTML(paste(
                                  p("General Merchandise", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"Grocery"
                                    , style = "color:purpel; font-size:25px;")
                                )
                                ),
                                solidHeader = TRUE
                           ),
                           box( title = "Location ", 
                                width = 12,collapsible=TRUE,collapsed = TRUE,status = "primary",
                                HTML(paste(
                                  p("Illionis", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"North Carolina", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"Michigan", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"California", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;')
                                    , style = "color:purpel; font-size:25px;")
                                )
                                ),
                                solidHeader = TRUE
                           )
                    ))),
                  tabPanel("Uni-Variate", box(plotOutput("univariate",width = 800),selectInput("variableInput","Attribute",choices = c("sales","num_units","Price")),title = "Uni-Variate Distribution", footer = "A Density Plot visualises the distribution of data over a continuous interval or time period.")),
                                              # if(input$variableInput=="sales"){title = "num_units univariate" , footer = "num_units distribution is right skewed"})
                                              # else if(input$variableInput=="num_units"){title = "num_units univariate" , footer = "num_units distribution is right skewed"}))
                  tabPanel("Bi-Variate", box(plotOutput("bivariate",width = 800),width="300px",
                                             fluidRow(
                                             column(6,selectInput("dependentInput","Dependent Variable",choices = c("sales","num_units"))),
                                             column(6,selectInput("independentInput","Independent Variable",
                                                         choices = c("Class","Product_Name","store","cal_month","cal_month_year","cal_qtr_nbr","cal_weekday_name"),
                                                         selected = "Class"))),title = "Bi-Variate Distribution", footer = "A barplot (or barchart) is one of the most common types of graphic. It shows the relationship between a numeric and a categoric variable. Each entity of the categoric variable is represented as a bar. The size of the bar represents its numeric value.")),
                  tabPanel("Hypothesis",fluidRow(
                    column(7,box(plotOutput("coolplot",width=450),
                                             selectInput("dependentInput1","Dependent Variable",choices = c("sales","num_units")),
                                             selectInput("plotInput","Plot Type",choices = c("Box"=1,"Bar"=2),selected = 2),title = "Hypothesis Testing", footer = "The plot graphs the relationship between two variables that have been measured on a single sample of subjects.")),
                    # column(3,box(HTML(paste(
                    #   p(br(),br(),br(),"Illionis", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"North Carolina", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"Michigan", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"California", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;')
                    #     , style = "color:purpel; font-size:25px;")
                  # )))))),
                    column(5,box(
                      selectInput("independentInput1","Independent Variable",
                                  choices = c("Class","Product_Name","store","cal_month","cal_month_year","cal_qtr_nbr","cal_weekday_name"),
                                  selected = "Class"),
                      HTML(paste(
                      p(br(),"H0 :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("nullInput"),
                      HTML(paste(
                      p(br(),"H1 :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("altInput"),
                      HTML(paste(
                      p(br(),"Test :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("testInput"),
                      HTML(paste(
                      p(br(),"Test Score :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("scoreInput")
                    )))),
                  tabPanel("Modelling",fluidRow(
                    column(7,plotOutput("modelling",width=450),
                           fluidRow(
                           column(6,selectInput("storeInput1","Store",choices = c("S1","S2","S3","S4"))),
                           column(6,selectInput("productinput1","Product",choices = c("Hom_TVs_7731","Hom_TVs_6640","Hom_TVs_8397","Hom_TVs_7478","Hom_Ref_8946","Hom_Ref_1489","Hom_Ref_7626","Hom_Ref_8903","Hom_Vac_5287","Hom_Vac_2038","Hom_Vac_2088","Hom_Vac_3210","Hom_Was_7499","Hom_Was_6784","Hom_Was_4515","Hom_Was_4070")))),
                           selectInput("modellingInput","Model Type",choices = c("Holt Method","Holt Winter","SARIMA","TBATS"),selected = "SARIMA"),title = "Forecasts", footer = "The plot shows the predicted value for each product in each store for each model for the year 2018"),
                    column(5,box(
                      HTML(paste(
                        p(br(),"Model Name :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("modelInput"),
                      HTML(paste(
                        p(br(),"Forecatsed Value :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("forecastInput"),
                      HTML(paste(
                        p(br(),"Accuracy :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("accuracyInput"),
                      HTML(paste(
                        p(br(),"YOY :", HTML('&nbsp;&nbsp;'), style = "color:purpel; font-size:10px;"))),textOutput("yoyInput")
                    )))),
                  tabPanel("Table", DT::dataTableOutput("table"))
                 )
             )
  )
)
server <- function(input, output) {
  output$bivariate <- renderPlot({
    if(input$dependentInput=="sales"){
      if(input$independentInput=="Class"){
        df1 %>% ggplot(aes(x = factor(Class),colour = sales)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Sales per Class Distribution",
                                                                                                  y="Sales ($)", x = "Class")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="Product_Name"){
        df1 %>% ggplot(aes(x = factor(Product_Name),colour = sales)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Sales per Product Variant Distribution",
                                                                                                    y="Sales ($)", x = "Product Name")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="store"){
        df1 %>% ggplot(aes(x = factor(store),colour = sales)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Sales per Store Distribution",
                                                                                                    y="Sales ($)", x = "Store")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="cal_month"){
        df1 %>% ggplot(aes(x = factor(cal_month),colour = sales)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Sales per Month Distribution",
                                                                                                    y="Sales ($)", x = "Month")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="cal_month_year"){
        df1 %>% ggplot(aes(x = factor(cal_month),colour = sales)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Sales per Year Distribution",
                                                                                                        y="Sales ($)", x = "Year")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="cal_qtr_nbr"){
        df1 %>% ggplot(aes(x = factor(cal_qtr_nbr),colour = sales)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Sales per Quarter Distribution",
                                                                                                    y="Sales ($)", x = "Quarter of the Year")+ theme(panel.background = element_rect(fill = 'white'))
      }
      # else if(input$independentInput==""cal_weekday_name){
      #   ggplot(df1) + 
      #     geom_bar(aes(x = factor(cal_weekday_name), y = sales,stat = "identity",position= "dodge") +
      #                xlab("Day of the Week") +
      #                ylab("Total sales"))
      # }
    }
    else if(input$dependentInput=="num_units"){
      if(input$independentInput=="Class"){
        df1 %>% ggplot(aes(x = factor(Class),colour = num_units)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Units per Class Distribution",
                                                                                                    y="Units", x = "Class")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="Product_Name"){
          df1 %>% ggplot(aes(x = factor(Product_Name),colour = num_units)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Units per Product variant Distribution",
                                                                                                      y="Units", x = "Product Name")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="store"){
        df1 %>% ggplot(aes(x = factor(store),colour = num_units)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Units per Store Distribution",
                                                                                                    y="Units", x = "Store")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="cal_month"){
        df1 %>% ggplot(aes(x = factor(cal_month),colour = num_units)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Units per Month Distribution",
                                                                                                    y="Units", x = "Month of the Year")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="cal_month_year"){
        df1 %>% ggplot(aes(x = factor(cal_month_year),colour = num_units)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Units per Year Distribution",
                                                                                                    y="Units", x = "Year")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$independentInput=="cal_qtr_nbr"){
        df1 %>% ggplot(aes(x = factor(cal_qtr_nbr),colour = num_units)) + geom_bar(fill="#800000",position = "dodge") + labs(title= "Units per Quarter Distribution",
                                                                                                    y="Units", x = "Quarter of the Year")+ theme(panel.background = element_rect(fill = 'white'))
      }
      # else if(input$independentInput==""){
      #   ggplot(df1) + 
      #     geom_bar(aes(x = factor(cal_weekday_name), y = num_units,stat = "identity",position= "dodge") +
      #                xlab("Day of the Week") +
      #                ylab("Total no of units"))
      # }
    }   
  })
  # output$bivariate <- renderPlot({
  #   if(input$dependentInput==1){
  #     if(input$independentInput==11)
  #       {
  #         plot_ly(df, x =~factor(Class), y =~sales, type = 'bar',name='Churned',marker=list(color='rgb(128,0,0)'))%>%
  #           layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #           layout(legend = list(x = 0.7, y = 0.8))%>%
  #           add_trace(y = ~freq_non_churn, name = 'Not Churned',marker=list(color='rgb(0,102,102)'))%>%
  #           layout(title = "Number Of Customers with respect to accountlength",
  #                  xaxis = list(title="Account length in weeks"),
  #                  yaxis = list(title = "Number of Customers"))
  #         
  #       }
  #     else if(input$independentInput==12)
  #       {
  #         plot_ly(df, x =~factor(Product_Name) , y =~sales, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #           layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #           layout(title = "Number Of Customers with respect to accountlength",
  #                  xaxis = list(title="Account length in weeks"),
  #                  yaxis = list(title = "Number of Customers"))
  #         
  #     }
  #     else if(input$independentInput==13)
  #     {
  #       plot_ly(df, 
  #               showgrid = FALSE, x =~factor(store) , y =~sales, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==14)
  #     {
  #       plot_ly(df, x =~factor(cal_month) , y =~sales, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==15)
  #     {
  #       plot_ly(df, x =~factor(cal_month_year) , y =~sales, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==12)
  #     {
  #       plot_ly(df, 
  #               showgrid = FALSE, x =~factor(cal_qtr_nbr) , y =~sales, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==17)
  #     {
  #       plot_ly(df, x =~factor(cal_weekday_name) , y =~sales, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #   }
  #   else if(inputdependentInput==2){
  #     if(input$independentInput==11)
  #     {
  #       plot_ly(df, x =~factor(Class), y =~num_units, type = 'bar',name='Churned',marker=list(color='rgb(128,0,0)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(legend = list(x = 0.7, y = 0.8))%>%
  #         add_trace(y = ~freq_non_churn, name = 'Not Churned',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==12)
  #     {
  #       plot_ly(df, 
  #               showgrid = FALSE, x =~factor(Product_Name) , y =~sales, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==13)
  #     {
  #       plot_ly(df, x =~factor(store) , y =~num_units, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==14)
  #     {
  #       plot_ly(df, x =~factor(cal_month) , y =~num_units, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==15)
  #     {
  #       plot_ly(df, x =~factor(cal_month_year) , y =~num_units, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==12)
  #     {
  #       plot_ly(df, x =~factor(cal_qtr_nbr) , y =~num_units, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     else if(input$independentInput==17)
  #     {
  #       plot_ly(df, x =~factor(cal_weekday_name) , y =~num_units, type = 'bar',marker=list(color='rgb(0,102,102)'))%>%
  #         layout(xaxis = list(showgrid = F),yaxis = list(showgrid = F)) %>%
  #         layout(title = "Number Of Customers with respect to accountlength",
  #                xaxis = list(title="Account length in weeks"),
  #                yaxis = list(title = "Number of Customers"))
  #       
  #     }
  #     
  #   }
  # })  
  # 
  # # output$bivariate <- renderPlot({
  # #   
  # #   q = input$independentInput
  # #   w = input$dependentInput
  # #   df %>% ggplot(aes(factor(Class),w)) + geom_boxplot()+ labs(title= "Price Vs Sales Distribution",
  # #                                                                            y="Sales ($)", x = "Price ($)")
  # #   #df %>% ggplot(aes(x = factor(store),y =w)) + geom_bar(stat="identity",position = "dodge")
  # #   # df2 %>% ggplot(aes(x = factor(Class), y = w)) +
  # #   #   geom_boxplot()#geom_bar(stat="identity",position = "dodge")
  # #   # ggplot(df1) + 
  # #   #   geom_bar(aes(x = w), y = q,stat = "identity",position= "dodge") + 
  # #   #   xlab("Cuisines") + 
  # #   #   ylab("Total count")  
  # #     # scale_fill_manual("Cuisines",
  #     #                   values = c("Italian" = "lightpink",
  #     #                              "Panjabi" = "lightblue", 
  #     #                              "Chinese" = "darkgrey", 
  #     #                              "Bengoli"="lightgreen"))
  # })
  
  output$univariate <- renderPlot({
  #   df %>% ggplot(aes(x=input$variableInput)) + geom_density()
  a = which(colnames(df)== input$variableInput)
  doPlots <- function(data_in, fun, ii, ncol=3) {
    pp <- list()
    for (i in ii) {
      p <- fun(data_in=data_in, i=i)
      pp <- c(pp, list(p))
    }
    do.call("grid.arrange", c(pp, ncol=ncol))
  }
  
  plotDen <- function(data_in, i){
    data <- data.frame(x=data_in[[i]])
    p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 2,alpha = 1.0,color="#800000") +
      xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme(panel.background = element_rect(fill = 'white'))#theme_light() 
    return(p)
  }
  doPlots(df, fun = plotDen, ii = a, ncol = 2)
   })
  
  filtered <- reactive({
    # if (is.null(input$countryInput)) {
    #   return(NULL)
    # }    
    
      df %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             saleDate >= input$dateInput[1],
             saleDate <= input$dateInput[2],
             Class == input$classInput,
             store == input$storeInput
      )
  })
  
  output$modelling <- renderPlot({
    s1 <-df1 %>%
      filter(store == "S1")
    #View(s1)
    s2 <-df1 %>%
      filter(store == "S2")
    #View(s2)
    s3 <-df1 %>%
      filter(store == "S3")
    #View(s3)
    s4 <-df1 %>%
      filter(store == "S4")
    #View(s4)
    
    # ml=c("S1","S2","S3","S4")
    # for(s in ml){
    # if(input$storeInput1){
    if (input$storeInput1=="S1"){
      # s1 <-df1 %>%
      #   filter(store == s) 
      if(input$productinput1=="Hom_TVs_7731"){
        tv1 <-s1 %>%
          filter(Product=="Hom_TVs_7731")
        tv1= tv1[order(tv1$cal_month_year,tv1$cal_month_nbr),]
        tv11 = tv1 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tstv1 = ts(tv11$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        # train80tv1 = tstv1[1:68]
        # tvt1 = ts(train80tv1,start = c(2011,1), frequency = 12)
        # #train80s1
        # test20tv1 = tstv1[68:84]
        # #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80tv1dec <- decompose(tstv1)
          train80tv1adjusted <- tstv1 - train80tv1dec$seasonal
          train80tv1adjusted = ts(train80tv1adjusted,start = c(2011,1), frequency = 12)
          fit1tv1 <- HoltWinters(train80tv1adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1tv1,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2tv1 <- HoltWinters(tstv1)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2tv1,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3tv1 <- auto.arima(tstv1,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3tv1,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4tv1 <- tbats(tstv1,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4tv1,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_TVs_6640"){
        tv2 <-s1 %>%
          filter(Product=="Hom_TVs_6640")
        tv2= tv2[order(tv2$cal_month_year,tv2$cal_month_nbr),]
        tv22 = tv2 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tstv2 = ts(tv22$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        # train80tv2 = tstv2[1:68]
        # tvt2 = ts(train80tv2,start = c(2011,1), frequency = 12)
        # #train80s1
        # test20tv2 = tstv2[68:84]
        # #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80tv2dec <- decompose(tstv2)
          train80tv2adjusted <- tstv2 - train80tv2dec$seasonal
          train80tv2adjusted = ts(train80tv2adjusted,start = c(2011,1), frequency = 12)
          fit1tv2 <- HoltWinters(train80tv2adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1tv2,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2tv2 <- HoltWinters(tstv2)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2tv2,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3tv2 <- auto.arima(tstv2,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3tv2,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4tv2 <- tbats(tstv2,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4tv2,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_TVs_8397"){
        tv3 <-s1 %>%
          filter(Product=="Hom_TVs_8397")
        tv3= tv3[order(tv3$cal_month_year,tv3$cal_month_nbr),]
        tv33 = tv3 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tstv3 = ts(tv33$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80tv3 = tstv3[1:68]
        #tvt3 = ts(tstv3,start = c(2011,1), frequency = 12)
        #train80s1
        #test20tv3 = tstv3[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80tv3dec <- decompose(tstv3)
          train80tv3adjusted <- tstv3 - train80tv3dec$seasonal
          train80tv3adjusted = ts(train80tv3adjusted,start = c(2011,1), frequency = 12)
          fit1tv3 <- HoltWinters(train80tv3adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1tv3,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2tv3 <- HoltWinters(tstv3)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2tv3,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3tv3 <- auto.arima(tstv3,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3tv3,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4tv3 <- tbats(tstv3,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4tv3,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_TVs_7478"){
        tv4 <-s1 %>%
          filter(Product=="Hom_TVs_7478")
        tv4= tv4[order(tv4$cal_month_year,tv4$cal_month_nbr),]
        tv44 = tv4 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tstv4 = ts(tv44$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        # train80tv4 = tstv4[1:68]
        #tvt4 = ts(train80tv4,start = c(2011,1), frequency = 12)
        #train80s1
        #test20tv4 = tstv4[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80tv4dec <- decompose(tstv4)
          train80tv4adjusted <- tstv4 - train80tv4dec$seasonal
          train80tv4adjusted = ts(train80tv4adjusted,start = c(2011,1), frequency = 12)
          fit1tv4 <- HoltWinters(train80tv4adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1tv4,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2tv4 <- HoltWinters(tstv4)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2tv4,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3tv1 <- auto.arima(tstv4,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3tv4,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4tv4 <- tbats(tstv4,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4tv4,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Ref_8946"){
        ref11 <-s1 %>%
          filter(Product=="Hom_Ref_8946")
        ref11= ref11[order(ref11$cal_month_year,ref11$cal_month_nbr),]
        ref11 = ref11 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsref1 = ts(ref11$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80ref1 = tsref1[1:68]
        #reft1 = ts(train80ref1,start = c(2011,1), frequency = 12)
        #train80s1
        #test20ref1 = tsref1[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80ref1dec <- decompose(tsref1)
          train80ref1adjusted <- tsref1 - train80ref1dec$seasonal
          train80ref1adjusted = ts(train80ref1adjusted,start = c(2011,1), frequency = 12)
          fit1tv1 <- HoltWinters(train80ref1adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1ref1,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2ref1 <- HoltWinters(tfres1)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2ref1,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3ref1 <- auto.arima(tsref1,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3ref1,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4ref1 <- tbats(tsref1,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4ref1,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Ref_1489"){
        ref12 <-s1 %>%
          filter(Product=="Hom_Ref_1489")
        ref12= ref12[order(ref12$cal_month_year,ref12$cal_month_nbr),]
        ref12 = ref12 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsref2 = ts(ref12$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80ref2 = tsref2[1:68]
        #reft2 = ts(train80ref2,start = c(2011,1), frequency = 12)
        #train80s1
        #test20ref2 = tsref2[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80ref2dec <- decompose(tsref2)
          train80ref2adjusted <- tsref2 - train80ref2dec$seasonal
          train80ref2adjusted = ts(train80ref2adjusted,start = c(2011,1), frequency = 12)
          fit1tv1 <- HoltWinters(train80ref2adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1ref2,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2ref2 <- HoltWinters(tsref2)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2ref2,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3ref2 <- auto.arima(tsref2,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3ref2,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4ref2 <- tbats(tsref2,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4ref2,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Ref_7626"){
        ref13 <-s1 %>%
          filter(Product=="Hom_Ref_7626")
        ref13= ref13[order(ref13$cal_month_year,ref13$cal_month_nbr),]
        ref13 = ref13 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsref3 = ts(ref13$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80ref3 = tsref3[1:68]
        #reft3 = ts(train80ref3,start = c(2011,1), frequency = 12)
        #train80s1
        #test20ref3 = tsref3[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80ref3dec <- decompose(tsref3)
          train80ref3adjusted <- tsref3 - train80ref3dec$seasonal
          train80ref3adjusted = ts(train80ref3adjusted,start = c(2011,1), frequency = 12)
          fit1tv3 <- HoltWinters(train80ref3adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1ref3,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2ref3 <- HoltWinters(tsref3)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2ref3,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3ref1 <- auto.arima(tsref3,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3ref3,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4ref3 <- tbats(tsref3,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4ref3,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Ref_8903"){
        ref14 <-s1 %>%
          filter(Product=="Hom_TVs_8946")
        ref14= ref14[order(ref14$cal_month_year,ref14$cal_month_nbr),]
        ref14 = ref14 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsref4 = ts(ref14$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80ref4 = tsref4[1:68]
        #reft4 = ts(train80ref4,start = c(2011,1), frequency = 12)
        #train80s1
        #test20ref4 = tsref4[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80ref4dec <- decompose(tsref4)
          train80ref4adjusted <- tsref4 - train80ref4dec$seasonal
          train80ref4adjusted = ts(train80ref4adjusted,start = c(2011,1), frequency = 12)
          fit1tv1 <- HoltWinters(train80ref4adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1ref4,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2ref4 <- HoltWinters(tsref4)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2ref4,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3ref4 <- auto.arima(tsref4,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3ref4,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4ref4 <- tbats(tsref4,use.box.cox = FALSE)
          # summary(fit4s1
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4ref4,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Vac_5287"){
        vac11 <-s1 %>%
          filter(Product=="Hom_Vac_5287")
        vac11= vac11[order(vac11$cal_month_year,vac11$cal_month_nbr),]
        vac11 = vac11 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsvac1 = ts(vac11$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80vac1 = tsvac1[1:68]
        #vact1 = ts(train80vac1,start = c(2011,1), frequency = 12)
        #train80s1
        #test20vac1 = tsvac1[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80vac1dec <- decompose(tsvac1)
          train80vac1adjusted <- tsvac1 - train80vac1dec$seasonal
          train80vac1adjusted = ts(train80vac1adjusted,start = c(2011,1), frequency = 12)
          fit1vac1 <- HoltWinters(train80vac1adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1vac1,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2vac1 <- HoltWinters(tsvac1)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2vac1,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3vac1 <- auto.arima(tsvac1,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3vac1,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4vac1 <- tbats(tsvac1,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4vac1,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Vac_2038"){
        vac12 <-s1 %>%
          filter(Product=="Hom_Vac_2038")
        vac12= vac12[order(vac12$cal_month_year,vac12$cal_month_nbr),]
        vac12 = vac12 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsvac2 = ts(vac12$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80vac2 = tsvac2[1:68]
        #vact2 = ts(train80vac2,start = c(2011,1), frequency = 12)
        #train80s1
        #test20vac2 = tsvac2[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80vac2dec <- decompose(tsvac2)
          train80vac2adjusted <- tsvac2 - train80vac2dec$seasonal
          train80vac2adjusted = ts(train80vac2adjusted,start = c(2011,1), frequency = 12)
          fit1vac2 <- HoltWinters(train80vac2adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1vac2,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2vac2 <- HoltWinters(tsvac2)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2vac2,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3vac2 <- auto.arima(tsvac2,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3vac2,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4vac2 <- tbats(tsvac2,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4vac2,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Vac_2088"){
        vac13 <-s1 %>%
          filter(Product=="Hom_Vac_2088")
        vac13= vac13[order(vac13$cal_month_year,vac13$cal_month_nbr),]
        vac13 = vac13 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsvac3 = ts(vac13$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80vac3 = tsvac3[1:68]
        #vact3 = ts(train80vac3,start = c(2011,1), frequency = 12)
        #train80s1
        #test20vac3 = tsvac3[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80vac3dec <- decompose(tsvac3)
          train80vac3adjusted <- tsvac3 - train80vac3dec$seasonal
          train80vac3adjusted = ts(train80vac3adjusted,start = c(2011,1), frequency = 12)
          fit1vac3 <- HoltWinters(train80vac3adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1vac3,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2vac3 <- HoltWinters(tsvac3)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2vac3,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3vac3 <- auto.arima(tsvac3,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3vac3,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4vac3 <- tbats(tsvac3,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4vac3,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Vac_3210"){
        vac14 <-s1 %>%
          filter(Product=="Hom_Vac_3210")
        vac14= vac14[order(vac14$cal_month_year,vac14$cal_month_nbr),]
        vac14 = vac14 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tsvac4 = ts(vac14$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80vac4 = tsvac4[1:68]
        #vact4 = ts(train80vac4,start = c(2011,1), frequency = 12)
        #train80s1
        #test20vac4 = tsvac4[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80vac4dec <- decompose(tsvac4)
          train80vac4adjusted <- tsvac4 - train80vac4dec$seasonal
          train80vac4adjusted = ts(train80vac4adjusted,start = c(2011,1), frequency = 12)
          fit1vac4 <- HoltWinters(train80vac4adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1vac4,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2vac4 <- HoltWinters(tsvac4)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2vac4,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3vac4 <- auto.arima(tsvac4,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3vac4,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4vac4 <- tbats(tsvac4,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4vac4,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Was_7499"){
        was11 <-s1 %>%
          filter(Product=="Hom_Was_7499")
        was11= was11[order(was11$cal_month_year,was11$cal_month_nbr),]
        was11 = was11 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tswas1 = ts(was11$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80was1 = tswas1[1:68]
        #wast1 = ts(train80was1,start = c(2011,1), frequency = 12)
        #train80s1
        #test20was1 = tswas1[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80was1dec <- decompose(tswas1)
          train80was1adjusted <- tswas1 - train80was1dec$seasonal
          train80was1adjusted = ts(train80was1adjusted,start = c(2011,1), frequency = 12)
          fit1was1 <- HoltWinters(train80was1adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1was1,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2was1 <- HoltWinters(tswas1)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2was1,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3was1 <- auto.arima(tswas1,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3was1,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4was1 <- tbats(tswas1,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4was1,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Was_6784"){
        was12 <-s1 %>%
          filter(Product=="Hom_Was_6784")
        was12= was12[order(was12$cal_month_year,was12$cal_month_nbr),]
        was12 = was12 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tswas2 = ts(was12$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80was2 = tswas2[1:68]
        #wast2 = ts(train80was2,start = c(2011,1), frequency = 12)
        #train80s1
        #test20was2 = tswas2[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80was2dec <- decompose(tswas2)
          train80was2adjusted <- tswas2 - train80was2dec$seasonal
          train80was2adjusted = ts(train80was2adjusted,start = c(2011,1), frequency = 12)
          fit1was2 <- HoltWinters(train80was2adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1was2,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2was2 <- HoltWinters(tswas2)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2was2,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3was2 <- auto.arima(tswas2,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3was2,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4was2 <- tbats(tswas2,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4was2,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Was_4515"){
        was13 <-s1 %>%
          filter(Product=="Hom_Was_4515")
        was13= was13[order(was13$cal_month_year,was13$cal_month_nbr),]
        was13 = was13 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tswas3 = ts(was13$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80was3 = tswas3[1:68]
        #wast3 = ts(train80was3,start = c(2011,1), frequency = 12)
        #train80s1
        #test20was3 = tswas3[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80was3dec <- decompose(tswas3)
          train80was3adjusted <- tswas3 - train80was3dec$seasonal
          train80was3adjusted = ts(train80was3adjusted,start = c(2011,1), frequency = 12)
          fit1was3 <- HoltWinters(train80was3adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1was3,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2was3 <- HoltWinters(tswas3)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2was3,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3was3 <- auto.arima(tswas3,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3was3,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4was3 <- tbats(tswas3,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4was3,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
      else if(input$productinput1=="Hom_Was_4070"){
        was14 <-s1 %>%
          filter(Product=="Hom_Was_4070")
        was14= was14[order(was14$cal_month_year,was14$cal_month_nbr),]
        was14 = was14 %>%
          group_by(cal_month_year,cal_month_nbr) %>%
          summarise(sum_units = sum(num_units))
        #s11
        #making it a time series object
        #View(s1)
        tswas4 = ts(was14$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
        #tss1
        #Checking with 80-20 fit
        
        #train80was4 = tswas4[1:68]
        #wast4 = ts(train80was4,start = c(2011,1), frequency = 12)
        #train80s1
        #test20was4 = tswas4[68:84]
        #test20s1
        
        if(input$modellingInput=="Holt Method"){
          #double exponential - models level and trend
          train80was4dec <- decompose(tswas4)
          train80was4adjusted <- tswas4 - train80was4dec$seasonal
          train80was4adjusted = ts(train80was4adjusted,start = c(2011,1), frequency = 12)
          fit1was4 <- HoltWinters(train80was4adjusted,gamma = FALSE)
          # accuracy(forecast(fit1s1,h=17),test20s1)
          plot(forecast(fit1was4,h=12))
        }
        else if(input$modellingInput=="Holt Winter"){
          fit2was4 <- HoltWinters(tswas4)
          # accuracy(forecast(fit2s1,h=17),test20s1)
          #fit2s1
          plot(forecast(fit2was4,h=12))
          
        }
        else if(input$modellingInput=="SARIMA"){
          fit3was4 <- auto.arima(tswas4,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
          #print(fit3s1)
          autoplot(forecast(fit3was4,h=12))
          
          # #forecast
          # f3s1 <- forecast(fit3s1,h=17)
          # plot(f3s1)
          # accuracy(f3s1,test20s1)
          
        }
        else if(input$modellingInput=="TBATS"){
          fit4was4 <- tbats(tswas4,use.box.cox = FALSE)
          # summary(fit4s1)
          # fit4s1
          
          # #forecast
          # f4s1 <- forecast(fit4s1,h=17)
          plot(forecast(fit4was4,h=12))
          # accuracy(f4s1,test20s1)
          
        }
      }
    }
    
    else if (input$storeInput1=="S2"){
      s2= s2[order(s2$cal_month_year,s2$cal_month_nbr),]
      s22 = s2 %>%
        group_by(cal_month_year,cal_month_nbr) %>%
        summarise(sum_units = sum(num_units))
      #s22
      #making it a time series object
      #View(s2)
      tss2 = ts(s22$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
      #tss2
      #Checking with 80-20 fit

      # train80s2 = tss2[1:68]
      # t2 = ts(train80s2,start = c(2011,1), frequency = 12)
      # #train80s2
      # test20s2 = tss2[68:84]
      # #test20s2

      if(input$modellingInput=="Holt Method"){
        #double exponential - models level and trend
        train80s2dec <- decompose(tss2)
        train80s2adjusted <-tss2 - train80s2dec$seasonal
        train80s2adjusted = ts(train80s2adjusted,start = c(2011,1), frequency = 12)
        fit1s2 <- HoltWinters(train80s2adjusted,gamma = FALSE)
        # accuracy(forecast(fit1s2,h=17),test20s2)
        plot(forecast(fit1s2,h=12))
      }
      else if(input$modellingInput=="Holt Winter"){
        fit2s2 <- HoltWinters(tss2)
        # accuracy(forecast(fit2s2,h=17),test20s2)
        #fit2s2
        plot(forecast(fit2s2,h=12))

      }
      else if(input$modellingInput=="SARIMA"){
        fit3s2 <- auto.arima(tss2,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
        #print(fit3s2)
        autoplot(forecast(fit3s2,h=12))

        # #forecast
        # f3s2 <- forecast(fit3s2,h=17)
        # plot(f3s2)
        # accuracy(f3s2,test20s2)

      }
      else if(input$modellingInput=="TBATS"){
        fit4s2 <- tbats(tss2,use.box.cox = FALSE)
        # summary(fit4s1)
        # fit4s1

        # #forecast
        # f4s1 <- forecast(fit4s1,h=17)
        plot(forecast(fit4s2,h=12))
        # accuracy(f4s1,test20s1)

      }
    }

    else if (input$storeInput1=="S3"){
      s3= s3[order(s3$cal_month_year,s3$cal_month_nbr),]
      s33 = s3 %>%
        group_by(cal_month_year,cal_month_nbr) %>%
        summarise(sum_units = sum(num_units))
      #s33
      #making it a time series object
      #View(s3)
      tss3 = ts(s33$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
      #tss3
      #Checking with 80-20 fit

      # train80s3 = tss3[1:68]
      # t3 = ts(train80s3,start = c(2011,1), frequency = 12)
      # #train80s3
      # test20s3 = tss3[68:84]
      # #test20s3

      if(input$modellingInput=="Holt Method"){
        #double exponential - models level and trend
        train80s3dec <- decompose(tss3)
        train80s3adjusted <- tss3 - train80s3dec$seasonal
        train80s3adjusted = ts(train80s3adjusted,start = c(2011,1), frequency = 12)
        fit1s3 <- HoltWinters(train80s3adjusted,gamma = FALSE)
        # accuracy(forecast(fit1s3,h=17),test20s3)
        plot(forecast(fit1s3,h=12))
      }
      else if(input$modellingInput=="Holt Winter"){
        fit2s3 <- HoltWinters(tss3)
        # accuracy(forecast(fit2s3,h=17),test20s3)
        #fit2s3
        plot(forecast(fit2s3,h=12))

      }
      else if(input$modellingInput=="SARIMA"){
        fit3s3 <- auto.arima(tss3,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
        #print(fit3s3)
        autoplot(forecast(fit3s3,h=12))

        # #forecast
        # f3s3 <- forecast(fit3s3,h=17)
        # plot(f3s3)
        # accuracy(f3s3,test20s3)

      }
      else if(input$modellingInput=="TBATS"){
        fit4s3 <- tbats(tss3,use.box.cox = FALSE)
        # summary(fit4s3)
        # fit4s3

        # #forecast
        # f4s3 <- forecast(fit4s3,h=17)
        plot(forecast(fit4s3,h=12))
        # accuracy(f4s3,test20s3)

      }
    }

    else if (input$storeInput1=="S4"){
      s4= s4[order(s4$cal_month_year,s4$cal_month_nbr),]
      s44 = s4 %>%
        group_by(cal_month_year,cal_month_nbr) %>%
        summarise(sum_units = sum(num_units))
      #s44
      #making it a time series object
      #View(s4)
      tss4 = ts(s44$sum_units, start = c(2011,1), end=c(2017,12), frequency=12)
      #tss4
      #Checking with 80-20 fit

      # train80s4 = tss4[1:68]
      # t4 = ts(train80s4,start = c(2011,1), frequency = 12)
      # #train80s4
      # test20s4 = tss4[68:84]
      # #test20s4

      if(input$modellingInput=="Holt Method"){
        #double exponential - models level and trend
        train80s4dec <- decompose(tss4)
        train80s4adjusted <- tss4 - train80s4dec$seasonal
        train80s4adjusted = ts(train80s4adjusted,start = c(2011,1), frequency = 12)
        fit1s4 <- HoltWinters(train80s4adjusted,gamma = FALSE)
        # accuracy(forecast(fit1s4,h=17),test20s4)
        plot(forecast(fit1s4,h=12))
      }
      else if(input$modellingInput=="Holt Winter"){
        fit2s4 <- HoltWinters(tss4)
        # accuracy(forecast(fit2s4,h=17),test20s4)
        #fit2s4
        plot(forecast(fit2s4,h=12))

      }
      else if(input$modellingInput=="SARIMA"){
        fit3s4 <- auto.arima(tss4,seasonal = TRUE, max.p = 3, max.q = 3, max.d = 3) # excluding last 20% time series as test data
        #print(fit3s4)
        autoplot(forecast(fit3s4,h=12))

        # #forecast
        # f3s4 <- forecast(fit3s4,h=17)
        # plot(f3s4)
        # accuracy(f3s4,test20s4)

      }
      else if(input$modellingInput=="TBATS"){
        fit4s4 <- tbats(tss4,use.box.cox = FALSE)
        # summary(fit4s4)
        # fit4s4

        # #forecast
        # f4s4 <- forecast(fit4s4,h=17)
        plot(forecast(fit4s4,h=12))
        # accuracy(f4s4,test20s4)

      }
    }
    
  })
  
  
  output$coolplot <- renderPlot({
    # filtered <-
    #   df %>%
    #   filter(Price >= input$priceInput[1],
    #          Price <= input$priceInput[2],
    #          saleDate >= input$dateInput[1],
    #          saleDate <= input$dateInput[2],
    #          Class == input$classInput,
    #          store == input$storeInput
    #   )
    if(input$dependentInput1=="sales"){
    if (input$plotInput==1){
    filtered() %>% ggplot(aes(factor(Price),sales)) + geom_boxplot(fill="#800000")+ labs(title= "Price Vs Sales Distribution",
                                                                           y="Sales ($)", x = "Price ($)")+ theme(panel.background = element_rect(fill = 'white'))
    }
    else if(input$plotInput==2){
      filtered() %>% ggplot(aes(x = Price,sales)) + geom_bar(stat="identity",fill="#800000",position = "dodge") + labs(title= "Price Vs Sales Distribution",
                                                                                                y="Sales ($)", x = "Price ($)")+ theme(panel.background = element_rect(fill = 'white'))
    }
    }
    else if(input$dependentInput1=="num_units"){
      if (input$plotInput==1){
        filtered() %>% ggplot(aes(factor(Price),num_units)) + geom_boxplot(fill="#800000")+ labs(title= "Price Vs Units Distribution",
                                                                             y="Units", x = "Price ($)")+ theme(panel.background = element_rect(fill = 'white'))
      }
      else if(input$plotInput==2){
        filtered() %>% ggplot(aes(x = Price,num_units)) + geom_bar(stat="identity",position = "dodge",fill = "#800000") + labs(title= "Price Vs Units Distribution",
                                                                                                 y="Units ", x = "Price ($)")+ theme(panel.background = element_rect(fill = 'white'))
      }
    }
    
  })
  
  output$table <- DT::renderDataTable({
    filtered()
  })
  # output$results <- renderPlot({
  #   filtered1 <-
  #     df %>%
  #     filter(Price >= input$priceInput[1],
  #            Price <= input$priceInput[2],
  #            saleDate >= input$dateInput[1],
  #            saleDate <= input$dateInput[2],
  #            Class == input$classInput,
  #            store == input$storeInput
  #     )
  #   filtered1 %>% ggplot(aes(x = Price,colour = sales)) + geom_bar(position = "dodge") + labs(title= "Price Vs Sales Distribution",
  #                                                                                             y="Sales ($)", x = "Price ($)")
  #   #geom_boxplot()
  # })
  
  output$nullInput = renderText({
    if(input$independentInput1=="Class"){
      a<-paste0("Median of number of units and Class are same") 
    }
    else if(input$independentInput1=="Product_Name"){
      a<-paste0("Median of number of units and Product_Name are same") 
    }
    else if(input$independentInput1=="store"){
      a<-paste0("Median of number of units and Store are same") 
    }
    else if(input$independentInput1=="cal_month"){
      a<-paste0("Median of number of units and Month are same") 
    }
    else if(input$independentInput1=="cal_month_year"){
      a<-paste0("Median of number of units and Year are same") 
    }
    else if(input$independentInput1=="cal_qtr_nbr"){
      a<-paste0("Median of number of units and Quarter are same") 
    }
    a
  })
  
  output$altInput = renderText({
    if(input$independentInput1=="Class"){
      b<-"Median of number of units and Class are different"
    }
    else if(input$independentInput1=="Product_Name"){
      b<-"Median of number of units and Product_Name are different"
    }
    else if(input$independentInput1=="store"){
      b<-"Median of number of units and Store are different" 
    }
    else if(input$independentInput1=="cal_month"){
      b<-"Median of number of units and Month are different"
    }
    else if(input$independentInput1=="cal_month_year"){
      b<-"Median of number of units and Year are different"
    }
    else if(input$independentInput1=="cal_qtr_nbr"){
      b<-"Median of number of units and Quarter are different"
    }
    b
  })
  
  output$testInput = renderText({
    if(input$independentInput1=="Class"){
      c<-paste("Kruskal_wallis Test") 
    }
    else if(input$independentInput1=="Product_Name"){
      c<-paste("Kruskal_wallis Test") 
    }
    else if(input$independentInput1=="store"){
      c<-paste("Kruskal_wallis Test") 
    }
    else if(input$independentInput1=="cal_month"){
      c<-paste("Kruskal_wallis Test") 
    }
    else if(input$independentInput1=="cal_month_year"){
      c<-paste("Kruskal_wallis Test") 
    }
    else if(input$independentInput1=="cal_qtr_nbr"){
      c<-paste("Kruskal_wallis Test") 
    }
    c
  })
  
  output$scoreInput = renderText({
    if(input$independentInput1=="Class"){
      d<-paste0("Chi-Square=5816.3,df=3,P(>H)=2.2e-16") 
    }
    else if(input$independentInput1=="Product_Name"){
      d<-paste0("Chi-Square=82714,df=15,P(>H)=2.2e-16") 
    }
    else if(input$independentInput1=="store"){
      d<-paste0("Chi-Square=10052,df=3,P(>H)=2.2e-16") 
    }
    else if(input$independentInput1=="cal_month"){
      d<-paste0("Chi-Square=1099.3,df=11,P(>H)=2.2e-16") 
    }
    else if(input$independentInput1=="cal_month_year"){
      d<-paste0("Chi-Square=528.34,df=6,P(>H)=2.2e-16") 
    }
    else if(input$independentInput1=="cal_qtr_nbr"){
      d<-paste0("Chi-Square=338.92,df=3,P(>H)=2.2e-16") 
    }
    d
  })
  
  output$modelInput = renderText({
    j=input$modellingInput
    ji<-paste0(j)
  })
  output$forecastInput = renderText({
    if(input$modellingInput=="Holt Method"){
      if(input$productinput1=="Hom_TVs_7731"){i<-paste0("1732")}
      else if(input$productinput1=="Hom_TVs_6640"){i<-paste0("771")}
      else if(input$productinput1=="Hom_TVs_8397"){i<-paste0("1674")}
      else if(input$productinput1=="Hom_TVs_7478"){i<-paste0("1197")}
      else if(input$productinput1=="Hom_Ref_8946"){i<-paste0("1364")}
      else if(input$productinput1=="Hom_Ref_1489"){i<-paste0("1749")}
      else if(input$productinput1=="Hom_Ref_7626"){i<-paste0("1126")}
      else if(input$productinput1=="Hom_Ref_8903"){i<-paste0("699")}
      else if(input$productinput1=="Hom_Vac_5287"){i<-paste0("1198")}
      else if(input$productinput1=="Hom_Vac_2038"){i<-paste0("1332")}
      else if(input$productinput1=="Hom_Vac_2088"){i<-paste0("1893")}
      else if(input$productinput1=="Hom_Vac_3210"){i<-paste0("1437")}
      else if(input$productinput1=="Hom_Was_7499"){i<-paste0("1429")}
      else if(input$productinput1=="Hom_Was_6784"){i<-paste0("1993")}
      else if(input$productinput1=="Hom_Was_4515"){i<-paste0("1447")}
      else if(input$productinput1=="Hom_Was_4070"){i<-paste0("1468")}}
    else if(input$modellingInput=="Holt Winter"){
      if(input$productinput1=="Hom_TVs_7731"){i<-paste0("1747")}
      else if(input$productinput1=="Hom_TVs_6640"){i<-paste0("761")}
      else if(input$productinput1=="Hom_TVs_8397"){i<-paste0("1684")}
      else if(input$productinput1=="Hom_TVs_7478"){i<-paste0("1177")}
      else if(input$productinput1=="Hom_Ref_8946"){i<-paste0("1352")}
      else if(input$productinput1=="Hom_Ref_1489"){i<-paste0("1740")}
      else if(input$productinput1=="Hom_Ref_7626"){i<-paste0("1116")}
      else if(input$productinput1=="Hom_Ref_8903"){i<-paste0("692")}
      else if(input$productinput1=="Hom_Vac_5287"){i<-paste0("1191")}
      else if(input$productinput1=="Hom_Vac_2038"){i<-paste0("1353")}
      else if(input$productinput1=="Hom_Vac_2088"){i<-paste0("1878")}
      else if(input$productinput1=="Hom_Vac_3210"){i<-paste0("1432")}
      else if(input$productinput1=="Hom_Was_7499"){i<-paste0("1414")}
      else if(input$productinput1=="Hom_Was_6784"){i<-paste0("1988")}
      else if(input$productinput1=="Hom_Was_4515"){i<-paste0("1439")}
      else if(input$productinput1=="Hom_Was_4070"){i<-paste0("1463")}}
    else if(input$modellingInput=="SARIMA"){
      if(input$productinput1=="Hom_TVs_7731"){i<-paste0("1752")}
      else if(input$productinput1=="Hom_TVs_6640"){i<-paste0("753")}
      else if(input$productinput1=="Hom_TVs_8397"){i<-paste0("1673")}
      else if(input$productinput1=="Hom_TVs_7478"){i<-paste0("1142")}
      else if(input$productinput1=="Hom_Ref_8946"){i<-paste0("1387")}
      else if(input$productinput1=="Hom_Ref_1489"){i<-paste0("1733")}
      else if(input$productinput1=="Hom_Ref_7626"){i<-paste0("1101")}
      else if(input$productinput1=="Hom_Ref_8903"){i<-paste0("683")}
      else if(input$productinput1=="Hom_Vac_5287"){i<-paste0("1185")}
      else if(input$productinput1=="Hom_Vac_2038"){i<-paste0("1349")}
      else if(input$productinput1=="Hom_Vac_2088"){i<-paste0("1873")}
      else if(input$productinput1=="Hom_Vac_3210"){i<-paste0("1423")}
      else if(input$productinput1=="Hom_Was_7499"){i<-paste0("1407")}
      else if(input$productinput1=="Hom_Was_6784"){i<-paste0("1980")}
      else if(input$productinput1=="Hom_Was_4515"){i<-paste0("1434")}
      else if(input$productinput1=="Hom_Was_4070"){i<-paste0("1463")}}
    else if(input$modellingInput=="TBATS"){
      if(input$productinput1=="Hom_TVs_7731"){i<-paste0("1763")}
      else if(input$productinput1=="Hom_TVs_6640"){i<-paste0("771")}
      else if(input$productinput1=="Hom_TVs_8397"){i<-paste0("1633")}
      else if(input$productinput1=="Hom_TVs_7478"){i<-paste0("1104")}
      else if(input$productinput1=="Hom_Ref_8946"){i<-paste0("1345")}
      else if(input$productinput1=="Hom_Ref_1489"){i<-paste0("1726")}
      else if(input$productinput1=="Hom_Ref_7626"){i<-paste0("1079")}
      else if(input$productinput1=="Hom_Ref_8903"){i<-paste0("698")}
      else if(input$productinput1=="Hom_Vac_5287"){i<-paste0("1165")}
      else if(input$productinput1=="Hom_Vac_2038"){i<-paste0("1335")}
      else if(input$productinput1=="Hom_Vac_2088"){i<-paste0("1884")}
      else if(input$productinput1=="Hom_Vac_3210"){i<-paste0("1402")}
      else if(input$productinput1=="Hom_Was_7499"){i<-paste0("1409")}
      else if(input$productinput1=="Hom_Was_6784"){i<-paste0("1991")}
      else if(input$productinput1=="Hom_Was_4515"){i<-paste0("1425")}
      else if(input$productinput1=="Hom_Was_4070"){i<-paste0("1431")}}
    i
  })
  output$accuracyInput = renderText({
    if(input$modellingInput=="Holt Method"){k<-paste0("85.81 %")}
    else if(input$modellingInput=="Holt Winter"){k<-paste0("87.76 %")}
    else if(input$modellingInput=="SARIMA"){k<-paste0("86.52 %")}
    else if(input$modellingInput=="TBATS"){k<-paste0("88.14 %")}
    k
  })
  output$yoyInput = renderText({
    if(input$storeInput1=="S1"){
      if(input$modellingInput=="Holt Method"){o<-paste0("- 3.92 %")}
      else if(input$modellingInput=="Holt Winter"){o<-paste0("- 3.77 %")}
      else if(input$modellingInput=="SARIMA"){o<-paste0("- 3.88 %")}
      else if(input$modellingInput=="TBATS"){o<-paste0("- 3.81 %")}}
    else if(input$storeInput1=="S2"){
      if(input$modellingInput=="Holt Method"){o<-paste0("- 2.92 %")}
      else if(input$modellingInput=="Holt Winter"){o<-paste0("- 2.77 %")}
      else if(input$modellingInput=="SARIMA"){o<-paste0("- 2.48 %")}
      else if(input$modellingInput=="TBATS"){o<-paste0("- 2.37 %")}}
    else if(input$storeInput1=="S3"){
      if(input$modellingInput=="Holt Method"){o<-paste0("+7.11 %")}
      else if(input$modellingInput=="Holt Winter"){o<-paste0("+ 7.13 %")}
      else if(input$modellingInput=="SARIMA"){o<-paste0("+ 7.42 %")}
      else if(input$modellingInput=="TBATS"){o<-paste0("+ 7.61 %")}}
    else if(input$storeInput1=="S4"){
      if(input$modellingInput=="Holt Method"){o<-paste0("- 5.03 %")}
      else if(input$modellingInput=="Holt Winter"){o<-paste0("- 5.07 %")}
      else if(input$modellingInput=="SARIMA"){o<-paste0("- 5.17 %")}
      else if(input$modellingInput=="TBATS"){o<-paste0("- 5.31 %")}}
    o
  })
}

shinyApp(ui = ui, server = server)
