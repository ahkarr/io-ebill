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
  titlePanel("IO-BILL YGN Service),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("billCode", "Choose a state:",
                  list(`907 - YESC - Meter Bill` 
                       = list("01_907_907_YESCTHINGANGYUN",
                              "02_907_907_YESCYANKIN",
                              "03_907_907_YESCSOUTHOAKKALAPA",
                              "04_907_907_YESCNORTHOAKKALAPA",
                              "05_907_907_YESCDAWBON",
                              "06_907_907_YESCTHARKAYTA",
                              "07_907_907_YESCTAMWE",
                              "08_907_907_YESCMINGALARTAUNGNYUNT",
                              "09_907_907_YESCPAZUNTAUNG",
                              "10_907_907_YESCBOTAHTAUNG",
                              "11_907_907_YESCSOUTHDAGON",
                              "12_907_907_YESCNORTHDAGON",
                              "13_907_907_YESCDAGONSEIKKAN",
                              "14_907_907_YESCEASTDAGON",
                              "15_907_907_YESCSHWEPOUKKAN",
                              "16_907_907_YESCKYAUKTADA",
                              "17_907_907_YESCPABEDAN",
                              "18_907_907_YESCLATHA",
                              "19_907_907_YESCLANMADAW",
                              "20_907_907_YESCAHLONE",
                              "21_907_907_YESCSANCHAUNG",
                              "22_907_907_YESCKAMAYUT",
                              "23_907_907_YESCKYEEMYINDAING",
                              "24_907_907_YESCAYEYWAR",
                              "25_907_907_YESCBAHAN",
                              "26_907_907_YESCDAGON",
                              "27_907_907_YESCHLAING",
                              "28_907_907_YESCMAYANGON",
                              "29_907_907_YESCTHANLYIN",
                              "30_907_907_YESCKYAUKTAN",
                              "31_907_907_YESCKHAYAN",
                              "32_907_907_YESCTHONEGWA",
                              "33_907_907_YESCTWANTAY",
                              "34_907_907_YESCKAWHMU",
                              "35_907_907_YESCKUNCHANKONE",
                              "36_907_907_YESCDALA",
                              "37_907_907_YESCSEIKKYIKANAUNGTO",
                              "38_907_907_YESCKYAIKHTAW",
                              "39_907_907_YESCLETKHOTEKONE",
                              "40_907_907_YESCTADA",
                              "41_907_907_YESCINSEIN",
                              "42_907_907_YESCEASTHLAINGTHAYA",
                              "43_907_907_YESCWESTHLAINGTHAYA",
                              "44_907_907_YESCSHWELINBAN",
                              "45_907_907_YESCSHWEPYITHA",
                              "46_907_907_YESCHTANTABIN",
                              "47_907_907_YESCMINGALARDON",
                              "48_907_907_YESCHTAUKKYANT",
                              "49_907_907_YESCHLEGU",
                              "50_907_907_YESCDARPEN",
                              "51_907_907_YESCINDAING",
                              "52_907_907_YESCPHAUNGGYI",
                              "53_907_907_YESCHMAWBI",
                              "54_907_907_YESCMYAUNGTAGAR",
                              "55_907_907_YESCTAIKKYI",
                              "56_907_907_YESCOKKAN",
                              "57_907_907_YESCAHPYAUK"))
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
      'char18' ,
      'char19' ,
      'char20' ,
      'char21'
    )
    #df <- na.omit(df)
    df <- df[-1,]
    #No <- df$char1
    Ledger_No	<- df$char2
    Barcode	<- df$char3
    Meter_No <- df$char4
    Name	<- df$char5
    Address	<- df$char6
    Bill_Code	<- df$char7
    Due_Date <- ifelse(nchar(df$char8) == 10,
           Due_Date <- df$char8,
           Due_Date <- format(as.Date(as.numeric(df$char8),"1899-12-30"),"%d-%m-%Y"))
    Units	<- as.numeric(df$char9)
    Electrical_Fees	<- as.numeric(gsub(",","",df$char10))
    Service_Charges	<- as.numeric(df$char11)
    Horse_Power_Fees	<-as.numeric( df$char12)
    Discount	<- as.numeric(df$char13)
    Last_Balance <- as.numeric(df$char14)
    Total	<- as.numeric(df$char15)	
    Debt	<- as.numeric(df$char16)	
    Fine	<- as.numeric(df$char17)
    Grand_Total <- as.numeric(df$char18)	
    DYESCDATA <- data.frame(
      #No ,
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
    DYESCDATA <- filter(DYESCDATA,Debt == 0 & Fine == 0)
    DYESCDATA$Bill_Qty <- sample(1, size = nrow(DYESCDATA), replace = TRUE)
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
    filename = function() {paste(input$billCode,"_202407121654260387.xlsx", sep="")},
    content = function(file){
      write_xlsx(df_products_upload(),file,
                 col_names = TRUE,format_headers = TRUE)
    }
  )
}
)

# Run the application 
shinyApp(ui = ui, server = server)