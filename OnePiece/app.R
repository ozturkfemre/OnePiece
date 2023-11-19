library(shiny)
library(DT)
library(readxl)
library(tidyverse)

df <- read_excel("dataset/OnePieceBounties.xlsx", 
                               col_types = c("text", "text", "numeric", 
                                             "text"), na = "Unknown")
str(df)
class(df$Bounty)
df <- na.omit(df)
getwd()
length(unique(df$Character))
df$Character

names(df)[4] <- "Flag"

df$Flag <- gsub("\"", "", df$Flag)
df$Bounty <- format(df$Bounty, scientific = FALSE)
df$Bounty <- as.numeric(df$Bounty)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: Helvetica, sans-serif */
        background-color: #FFFFFF; /* Arka plan rengi */
      }
      .navbar {
        background-color: #000000; /* Menü arka plan rengi */
        color: white; /* Menü metin rengi */
      }
    "))
  ), 
  navbarPage(
    title = div(
      img(src = "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/ec719f46-44d6-4593-ab1e-952b01909a4e/d56tmy9-d0a821c8-63cf-4898-b51c-6ef1a55fdb01.png/v1/fill/w_900,h_411/chibi_straw_hat_pirates_by_sergiart_d56tmy9-fullview.png?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9NDExIiwicGF0aCI6IlwvZlwvZWM3MTlmNDYtNDRkNi00NTkzLWFiMWUtOTUyYjAxOTA5YTRlXC9kNTZ0bXk5LWQwYTgyMWM4LTYzY2YtNDg5OC1iNTFjLTZlZjFhNTVmZGIwMS5wbmciLCJ3aWR0aCI6Ijw9OTAwIn1dXSwiYXVkIjpbInVybjpzZXJ2aWNlOmltYWdlLm9wZXJhdGlvbnMiXX0.Z5nSoIisPsQvt7o8DBP05GNnodmUthDs1BvTVXRU1XE", height = "30"),
      "One Piece Bounties"
    ),
    tabPanel(
      "Bounty of Pirate",
      sidebarLayout(
        sidebarPanel(
          selectInput("character", "Choose a pirate:", choices = unique(df$Character)),
          br(),
          hr()
        ),
        mainPanel(
          br(),
          hr(),
          DTOutput("bountyOutput"),
          textOutput("textOut")
        )
      )
    ),
    tabPanel(
      "Top Bounties",
      sidebarLayout(
        sidebarPanel(
          sliderInput("topCount", "Top Bounties Count:", min = 1, max = nrow(df), value = 5)
        ),
        mainPanel(
          DTOutput("topBountiesTable")
        )
      )
    ),
    tabPanel(
      "Crew Bounties",
      sidebarLayout(
        sidebarPanel(
          selectInput("crew", "Choose a Pirate Crew:", choices = unique(df$Crew)),
          br()
        ),
        mainPanel(
          DTOutput("crewBountiesTable")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # 1. Seçilen karakterin ödül miktarını gösterme
  output$bountyOutput <- renderDT({
    selected_bounty <- df %>% filter(Character == input$character)
    header.style <- "th { font-family: 'Helvetica'; font-weight: bold; color: white; background-color: #000000;}"
    #pull header names from the table
    header.names <- c(" ", colnames(df))
    datatable(
      selected_bounty,
      style = "bootstrap4",
      options = list(
        autoWidth = FALSE, #smart width handling
        searching = FALSE, #search box above table
        ordering = TRUE, #whether columns can be sorted
        lengthChange = FALSE, #ability to change number rows shown on page in table
        lengthMenu = FALSE, #options lengthChange can be changed to
        pageLength = 10, #initial number of rows per page of table
        paging = TRUE, #whether to do pagination
        info = TRUE, #notes whether or not table is filtered
        columnDefs = list(list(
          targets = 4, # Image column
          render = JS(
            "function(data, type, row, meta) {",
            "  return '<img src=\"' + row[4] + '\" height=\"50\">';", # 3: Images sütununun indeksi
            "}"
          )))
      ),
      container = withTags(table(
        style(type = "text/css", header.style),
        thead(
          tr(
            lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
          )
        )
      )))%>%formatStyle(columns = c(1,2,3), #blank columns means row labels
                        backgroundColor = "#ffffff",
                        borderBottomColor = "#ffffff",
                        borderBottomStyle = "solid",
                        borderBottomWidth = "1px",
                        borderCollapse = "collapse",
                        borderRightColor = "#ffffff",
                        borderRightStyle = "solid",
                        borderRightWidth = "1px",
                        color = "#000000",
                        fontFamily = "Helvetica",
                        fontSize = "13px",
                        fontWeight = "bold",
                        lineHeight = "normal",
                        paddingBottom = "2.6px",
                        paddingLeft = "5.2px",
                        paddingRight = "5.2px",
                        paddingTop = "2.6px",
                        textAlign = "middle",
                        verticalAlign = "middle") 
  })
  
  # 2. Ödül Miktarı Sıralaması Tablosu
  output$topBountiesTable <- renderDT({
    sorted_by_bounty <- df %>% arrange(desc(Bounty)) %>% slice(1:input$topCount)
    header.style <- "th { font-family: 'Helvetica'; font-weight: bold; color: white; background-color: #000000;}"
    #pull header names from the table
    header.names <- c(" ", colnames(df))
    datatable(
      sorted_by_bounty,
      style = "bootstrap4",
      options = list(
        autoWidth = FALSE, #smart width handling
        searching = FALSE, #search box above table
        ordering = TRUE, #whether columns can be sorted
        lengthChange = FALSE, #ability to change number rows shown on page in table
        lengthMenu = FALSE, #options lengthChange can be changed to
        pageLength = 10, #initial number of rows per page of table
        paging = TRUE, #whether to do pagination
        info = TRUE, #notes whether or not table is filtered
        columnDefs = list(list(
          targets = 4, # Image column
          render = JS(
            "function(data, type, row, meta) {",
            "  return '<img src=\"' + row[4] + '\" height=\"50\">';", # 3: Images sütununun indeksi
            "}"
          )))
        ),
      container = withTags(table(
        style(type = "text/css", header.style),
        thead(
          tr(
            lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
          )
        )
      )),
      rownames = TRUE)%>%formatStyle(columns = c(1,2,3), #blank columns means row labels
                         backgroundColor = "#ffffff",
                         borderBottomColor = "#ffffff",
                         borderBottomStyle = "solid",
                         borderBottomWidth = "1px",
                         borderCollapse = "collapse",
                         borderRightColor = "#ffffff",
                         borderRightStyle = "solid",
                         borderRightWidth = "1px",
                         color = "#000000",
                         fontFamily = "Helvetica",
                         fontSize = "13px",
                         fontWeight = "bold",
                         lineHeight = "normal",
                         paddingBottom = "2.6px",
                         paddingLeft = "5.2px",
                         paddingRight = "5.2px",
                         paddingTop = "2.6px",
                         textAlign = "middle",
                         verticalAlign = "middle") 
    
  })
  
  # 3. Mürettebata Göre Ödül Miktarı Sıralaması Tablosu
  output$crewBountiesTable <- renderDT({
    sorted_by_crew_bounty <- df %>%
      filter(Crew == input$crew) %>%
      arrange(desc(Bounty))
    header.style <- "th { font-family: 'Helvetica'; font-weight: bold; color: white; background-color: #000000;}"
    #pull header names from the table
    header.names <- c(" ", colnames(df))
    datatable(
      sorted_by_crew_bounty,
      style = "bootstrap4",
      options = list(
        autoWidth = FALSE, #smart width handling
        searching = FALSE, #search box above table
        ordering = TRUE, #whether columns can be sorted
        lengthChange = FALSE, #ability to change number rows shown on page in table
        lengthMenu = FALSE, #options lengthChange can be changed to
        pageLength = 10, #initial number of rows per page of table
        paging = TRUE, #whether to do pagination
        info = TRUE, #notes whether or not table is filtered
        columnDefs = list(list(
          targets = 4, # Image column
          render = JS(
            "function(data, type, row, meta) {",
            "  return '<img src=\"' + row[4] + '\" height=\"50\">';", # 3: Images sütununun indeksi
            "}"
          )))
      ),
      container = withTags(table(
        style(type = "text/css", header.style),
        thead(
          tr(
            lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
          )
        )
      )))%>%formatStyle(columns = c(1,2,3), #blank columns means row labels
                              backgroundColor = "#ffffff",
                              borderBottomColor = "#ffffff",
                              borderBottomStyle = "solid",
                              borderBottomWidth = "1px",
                              borderCollapse = "collapse",
                              borderRightColor = "#ffffff",
                              borderRightStyle = "solid",
                              borderRightWidth = "1px",
                              color = "#000000",
                              fontFamily = "Helvetica",
                              fontSize = "13px",
                              fontWeight = "bold",
                              lineHeight = "normal",
                              paddingBottom = "2.6px",
                              paddingLeft = "5.2px",
                              paddingRight = "5.2px",
                              paddingTop = "2.6px",
                              textAlign = "middle",
                              verticalAlign = "middle") 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
