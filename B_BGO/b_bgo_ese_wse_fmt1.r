library(shiny)
library(readxl)
library(dplyr)
library(rsconnect)
library(DT)
library(writexl)
rsconnect::setAccountInfo(name='rdev-local', token='852C04873E79B6FC2E1C026E0577A77D', secret='qQNuhV5IQCbTA9cJ4p4XNJhF2cPM5Dg6pDw+PaKl')

options(rsconnect.max.bundle.size=3145728000)
options(shiny.maxRequestSize=30*1024^2)
#> 
#> Attaching package: 'DT'
#> The following objects are masked from 'package:shiny':
#> 
#>     dataTableOutput, renderDataTable

# Define UI
ui <- shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Bill-Payment Data Cleansing UAT"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("billCode", "Choose a state:",
                  list(`907 - YESC - Meter Bill` 
                       = list("01_1022_1028_ESEZAUNGTU",
                              "01_1023_1029_WSENATTALIN",
                              "02_1022_1028_ESETHANATPIN",
                              "02_1023_1029_WSEPAUNGDE",
                              "03_1022_1028_ESEKAWA",
                              "03_1023_1029_WSEZIGON",
                              "04_1022_1028_ESEWAW",
                              "04_1023_1029_WSELETPADAN",
                              "05_1022_1028_ESEMYITKYOE",
                              "05_1023_1029_WSEOAKSHITPIN",
                              "06_1022_1028_ESEDAIKU",
                              "06_1023_1029_WSEGYOBINGAUK",
                              "07_1022_1028_ESEHPAUNGTAWTHI",
                              "07_1023_1029_WSEMINHLA",
                              "08_1022_1028_ESEAYETHUKHA",
                              "08_1023_1029_WSETHARRAWADDY",
                              "09_1022_1028_ESENYAUNGLEBIN",
                              "09_1023_1029_WSEPAUKKAUNG",
                              "10_1022_1028_ESEPYUNTASA",
                              "10_1023_1029_WSEPAUNGDALE",
                              "11_1022_1028_ESEMADAUK",
                              "11_1023_1029_WSEINNMA",
                              "12_1022_1028_ESEPAINZALOTE",
                              "12_1023_1029_WSETHEGON",
                              "13_1022_1028_ESESHWEGYIN",
                              "13_1023_1029_WSEPADEEKONE",
                              "14_1022_1028_ESEKYAUKTAGA",
                              "14_1023_1029_WSEPYAY",
                              "15_1022_1028_ESEHPADO",
                              "15_1023_1029_WSESHWEDAUNG",
                              "16_1022_1028_ESEPENWEGON",
                              "17_1022_1028_ESETAWKYWEINN",
                              "18_1022_1028_ESEKYAUKKYI",
                              "19_1022_1028_ESEMONE",
                              "20_1022_1028_ESENATTHANKWIN",
                              "21_1022_1028_ESEPHYU",
                              "22_1022_1028_ESEKANYUTKWIN",
                              "23_1022_1028_ESEZEYAWADDY",
                              "24_1022_1028_ESEOKTWIN",
                              "25_1022_1028_ESEKYWEPWE",
                              "26_1022_1028_ESEHTANTAPIN",
                              "27_1022_1028_ESEYEDASHE",
                              "28_1022_1028_ESESWAR",
                              "29_1022_1028_ESETHARGAYA",
                              "30_1022_1028_ESEMYOHLA",
                              "31_1022_1028_ESEYENI"))
      ),
      textOutput("result"),
      tags$hr(),
      # Input: Select a file ----
      fileInput("file1", "Choose Excel File",
                multiple = FALSE,
                accept = c(".xlsx",
                           ".xls")),
      
      # Horizontal line ----
      tags$hr(),
      downloadButton("downloadData",label = "Export Excel"),
      tags$hr()
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      #tableOutput("contents"),
      DT::dataTableOutput("sample_table")
    )
  )
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  df_products_upload <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read_excel(inFile$datapath)
    colnames(df) <- c(
      "char1" ,
      'char2' ,
      'char3' ,
      'char4' ,
      'char5' ,
      'char6' ,
      'char7' ,
      'char8' ,
      'char9' ,
      'char10'	,
      'char11' ,
      'char12' ,
      'char13'	,
      'char14'
    )
    #View(df)
    ##df <- na.omit(df)
    ##df <- df[-1,]
    #View(df)
    Ledger_No	<- paste(paste(df$char3,df$char4,sep = 'P/'),df$char5,sep = "/")
    Barcode	<- df$char1
    Meter_No <- df$char10
    Customer_Id <- df$char1
    Name	<- df$char6
    Address <- gsub("NA","",paste(df$char7,paste(df$char8,df$char9)))
    Bill_Code	<- df$char2
    #df$char13 <- format(as.Date(df$char13, '%m/%d/%Y'), '%d/%m/%Y')
    Due_Date <- ifelse(nchar(df$char13) == 10,
                       Due_Date <- df$char13,
                       Due_Date <- format(as.Date(as.numeric(df$char13),"1899-12-30"),"%d-%m-%Y"))
    Units	<- as.numeric(df$char11)
    Electrical_Fees	<- as.numeric(df$char12)	
    Service_Charges	<- 0.00
    Horse_Power_Fees	<- 0.00
    Discount	<- 0.00
    Last_Balance <-  0.00
    Total	<- as.numeric(df$char12)
    Debt	<- 0.00
    Fine	<- 0.00
    Grand_Total <- as.numeric(df$char12)
    DYESCDATA <- data.frame(
      Ledger_No	,
      Barcode	,
      Meter_No ,
      Name	,
      Customer_Id,
      Address	,
      Bill_Code	,
      Due_Date ,
      Units	,
      Electrical_Fees	,
      Service_Charges	,
      Horse_Power_Fees	,
      Discount	,
      Last_Balance ,	
      Total	,
      Debt	,	
      Fine	,
      Grand_Total 	
    )
    #View(DYESCDATA)
    DYESCDATA <- filter(DYESCDATA,Debt == 0 & Fine == 0)
    DYESCDATA$Bill_Qty <- sample(1, size = nrow(DYESCDATA), replace = TRUE)
    #View(DYESCDATA)
    DYESCDATA$Remark <- NA
    No <- c(1:nrow(DYESCDATA))
    DYESCDATA <- cbind(No,DYESCDATA)
    #DYESCDATA <- na.omit(DYESCDATA)
    df <- DYESCDATA
    return(df)
  })
  
  output$sample_table<- DT::renderDataTable({
    DYESCDATA <- df_products_upload()
    DT::datatable(DYESCDATA)
  })
  output$result <- renderText({
    paste("Filename", input$billCode)
  })
  output$downloadData <- downloadHandler(
    filename = function() {paste(input$billCode,"_20240914122837589.xlsx", sep="")},
    content = function(file){
      write_xlsx(df_products_upload(),file,
                 col_names = TRUE,format_headers = TRUE)
    }
  )
}
)

# Run the application 
shinyApp(ui = ui, server = server)