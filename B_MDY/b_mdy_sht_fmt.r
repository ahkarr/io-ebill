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
                  list(`1021 - MESC - Meter Bill` 
                       = list(
                         "01_1021_1027_MESCAUNGMYAYTHAZAN",
                         "02_1021_1027_MESCCHANAYETHAZAN",
                         "03_1021_1027_MESCMAHAAUNGMYAY",
                         "04_1021_1027_MESCCHANMYATHAZI",
                         "05_1021_1027_MESCPYIGYITAGON",
                         "06_1021_1027_MESCAMARAPURA",
                         "07_1021_1027_MESCPATHEINGYI",
                         "08_1021_1027_MESCPYINOOLWIN",
                         "09_1021_1027_MESCMOGOK",
                         "10_1021_1027_MESCMADAYA",
                         "11_1021_1027_MESCSINGU",
                         "12_1021_1027_MESCTHABEIKKYIN",
                         "13_1021_1027_MESCTAKAUNG",
                         "14_1021_1027_MESCKYAUKSE",
                         "15_1021_1027_MESCSINTGAING",
                         "16_1021_1027_MESCPALATE",
                         "17_1021_1027_MESCTADA-U",
                         "18_1021_1027_MESCMYITTHA",
                         "19_1021_1027_MESCKUME",
                         "20_1021_1027_MESCMEIKTILA",
                         "21_1021_1027_MESCTHAZI",
                         "22_1021_1027_MESCWUNDWIN",
                         "23_1021_1027_MESCMAHLAING",
                         "24_1021_1027_MESCMYINGYAN",
                         "25_1021_1027_MESCTAUNGTHA",
                         "26_1021_1027_MESCNATOGYI",
                         "27_1021_1027_MESCNGAZUN",
                         "28_1021_1027_MESCNYAUNG-U",
                         "29_1021_1027_MESCBAGAN",
                         "30_1021_1027_MESCNGATHAYOUT",
                         "31_1021_1027_MESCKYAUKPADAUNG",
                         "32_1021_1027_MESCYAMETHIN",
                         "33_1021_1027_MESCPYAWBWE",
                         "34_1021_1027_MESCYANAUNG"
                       ))
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
      'char14'	,
      'char15'	,
      'char16'	,
      'char17'	,
      'char18' 
    )
    #View(df)
    df <- na.omit(df)
    df <- df[-1,]
    Ledger_No	<- df$char2
    Barcode	<- df$char1
    Meter_No <- df$char3
    Name	<- df$char4
    Address	<- df$char5
    Bill_Code	<- df$char17
    ifelse(nchar(df$char10) == 10,
           Due_Date <- df$char10,
           Due_Date <- format(as.Date(as.numeric(df$char10),"1899-12-30"),"%d-%m-%Y"))
    Units	<- as.numeric(df$char8)
    Electrical_Fees	<- as.numeric(df$char14)	- as.numeric(df$char11)
    Service_Charges	<- as.numeric(df$char11)
    Horse_Power_Fees	<-as.numeric( df$char12)
    Discount	<- as.numeric(df$char16)
    Last_Balance <- as.numeric(df$char16)
    Total	<- as.numeric(df$char14)	
    Debt	<- as.numeric(df$char16)	
    Fine	<- as.numeric(df$char16)
    Grand_Total <- as.numeric(df$char14)	
    DYESCDATA <- data.frame(
      Ledger_No	,
      Barcode	,
      Meter_No ,
      Name	,
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
    View(DYESCDATA)
    DYESCDATA <- filter(DYESCDATA,Debt == 0 & Fine == 0)
    DYESCDATA$Bill_Qty <- sample(1, size = nrow(DYESCDATA), replace = TRUE)
    View(DYESCDATA)
    DYESCDATA$Remark <- 1
    DYESCDATA$Remark <- NA
    No <- c(1:nrow(DYESCDATA))
    DYESCDATA <- cbind(No,DYESCDATA)
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
    filename = function() {paste(input$billCode,"_20240613092649052.xlsx", sep="")},
    content = function(file){
      write_xlsx(df_products_upload(),file,
                 col_names = TRUE,format_headers = TRUE)
    }
  )
}
)

# Run the application 
shinyApp(ui = ui, server = server)
