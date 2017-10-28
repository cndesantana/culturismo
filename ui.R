library(readxl)

#workdir <- "/srv/shiny-server/cns/Culturismo"
database <- read_xlsx(file.path(workdir,"data/database.xlsx"))

dashboardPage(
  dashboardHeader(title = "Culturismo"),skin="purple",
  dashboardSidebar(
    sidebarMenu(      
      menuItem("From Route to Books", tabName = "route2books"),
      menuItem("From Books to Route", tabName = "books2route")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("route2books",
        fluidRow(
          box(
            width = 8, status = "info", solidHeader = TRUE, 
            title = "From Route to Books",
            selectInput("Destination","Destination",
                        as.character(database$cidade), multiple = TRUE),
            plotOutput("route2booksPlot", width="100%", height= 600)
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
                    plotOutput("route2booksPlot", width="100%", height= 600)
                 )
              )
      )
   )
  )
)

