library(shiny)
library(plotly)
library(sf)
#----------------------------------------
# read data
#----------------------------------------
# maps
map_county <-readRDS("gadm36_SWE_1_sf.rds")
# order rows by counties' name, it will match income data
map_county <- map_county[order(map_county$NAME_1), ]
map_muni <-readRDS("gadm36_SWE_2_sf.rds")
# order rows by municipalities' name, it will match income data
map_muni <- map_muni[order(map_muni$NAME_2), ]
# income
income_couty <- read.csv("income.csv", encoding="latin1", stringsAsFactors = FALSE)
income_muni <- read.csv("income_muni.csv", encoding="latin1", stringsAsFactors = FALSE)

# filter and order data
manage_data <- function(income) {
  income <- income[which(income$age == "18-29 years"), ]
  names(income) [3] <- "income"
  
  income$NAME <- substr(income$region, c(regexpr(" ", income$region)), nchar(income$region))
  # order rows by region, match map data
  income <- income[order(income$NAME), ]
  
  return (income)
}
# get final final data frame
income_couty <- manage_data(income_couty)
income_muni <- manage_data(income_muni)




#----------------------------------------
# shiny
#----------------------------------------
# ui
ui <- fluidPage(
  
  titlePanel("Swedish Young Age Average Income in 2016"),
  fluidRow(
    column(
      width = 6, plotlyOutput(outputId = "counties", height = "600px")
    ),
    column(
      width = 6, plotlyOutput(outputId = "municipalities", height = "600px")
    )
  )
  
)


# server
server <- function(input, output) {
  
  #----------------------------------------
  # counties
  #----------------------------------------
  cols <- rainbow(length(unique(map_county$GID_1)), s = 0.8)
  
  output$counties <- renderPlotly({
    
    map_county$income <- income_couty$income
    plot_ly(map_county, source = "subset") %>% 
      add_sf(split = ~NAME_1, #color=~income, 
             # color = I("black"),
             color = ~NAME_1, colors = cols,
             showlegend = FALSE, alpha = 0.7, 
             text = ~paste0(NAME_1, ": ", income, "k SEK"), hoverinfo = "text", hoveron = "fill") %>%
      layout(title = "Map of Counties")
  })
  
  #----------------------------------------
  # municipalities
  #----------------------------------------
  output$municipalities <- renderPlotly({
    
    # click event by user
    eventdata <- event_data("plotly_click", source = "subset")
    gid <- as.character(eventdata$curveNumber + 1)
    
    if(is.null(eventdata) == T) {
      map_muni$income <- income_muni$income
      plot_ly(map_muni) %>% 
        add_sf(split=~NAME_2, 
               color = ~NAME_1, colors = cols,
               showlegend = FALSE, alpha = 0.7,
               text = ~paste0(NAME_2, ": ", income, "k SEK"), 
               hoverinfo = "text", hoveron = "fill") %>%
        layout(title = "Map of Municipalities")
      
    } else {
      
      # get the clicked county rows
      counties_id <- which(substr(map_muni$GID_1, 5, nchar(map_muni$GID_1)-2) == gid)
      # get the municipalities of the clicked county
      map_zoom <- map_muni[counties_id, ]
      # income of the municipalities
      map_zoom$income <- income_muni[counties_id, ]$income
      
      plot_ly(map_zoom) %>% 
        add_sf(split=~NAME_2, 
               color=~income, 
               showlegend = FALSE, alpha = 0.7, stroke = I("black"),
               text = ~paste0(NAME_2, ": ", income, "k SEK"), hoverinfo = "text", hoveron = "fill") %>%
        colorbar(title = "Income\n(SEK thousands)", alpha = 0.7) %>%
        # hide_colorbar() %>% 
        layout(title = paste("Incomes of Young Group in", map_muni$NAME_1[1]))
    }
    
  })
  
}


shinyApp(ui = ui, server = server)


