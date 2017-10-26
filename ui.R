library(readxl)

workdir <- "/home/cdesantana/DataSCOUT/Culturismo/MVP/dashboard"
database <- read_xlsx(file.path(workdir,"data/database.xlsx"))

dashboardPage(
  dashboardHeader(title = "Culturismo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("From Books to Route", tabName = "books2route"),
      menuItem("From Route to Books", tabName = "route2books")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("route2books",
        fluidRow(
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "From Router to Books",
            selectInput("Destination","Destination",
                        as.character(database$cidade), multiple = TRUE),
            plotOutput("reactionsPlot", width="100%", height= 600)
          )
        )
      ),
      tabItem("books2route",
              fluidRow(
                 box(
                    width = 8, status = "info", solidHeader = TRUE,
                    title = "From Books to Route",
                    selectInput("Books","Books",
                                as.character(database$livro), multiple = TRUE),
                    plotOutput("reactionsPlot", width="100%", height= 600)
                 )
              )
      )
      #tabItem("rawdata",
      #  numericInput("maxrows", "Rows to show", 25),
      #  verbatimTextOutput("rawtable"),
      #  downloadButton("downloadCsv", "Download as CSV")
      #)
    )
  )
)
