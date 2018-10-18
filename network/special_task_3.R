library(openxlsx)
library(ggraph)
library(igraph)
library(visNetwork)
library(shiny)
library(shinythemes)




# ---------------------------------------------
# manage data
# ---------------------------------------------
# disable scientific notation
options(scipen=999)
d <- read.xlsx("marbas.xlsx")

### calculate the amount for each ID ###
product_customer <- as.data.frame.matrix(table(d$TRANS_ID, d$PRODUCT))
### calculate the total amount for each products ###
products_amount <- colSums(product_customer)


### get links dataframe ###
nm <- names(product_customer)
links <- data.frame(from = rep(nm, 16), to = rep(nm, each = 16))
links <- links[which(links$from != links$to), ]
links <- links[-which(links$to == "beer"), ]
delete <- c()
for (i in 2:length(nm)) {
  delete <- c(delete, -which(links$to == nm[i])[-(1:(i-1))])
}
links <- links[delete, ]


amount <- rep(0, nrow(links))
# get the width of products
for(i in 1:nrow(product_customer)) {
  
  pos <- which(product_customer[i, ] != 0)
  if (length(pos) == 1) {
    next()
  }
  
  connection <- nm[pos]
  
  pos <- which(links$from %in% connection & links$to %in% connection)
  amount[pos] <- amount[pos] + 1
  
}

links$width <- amount
# links$width <- (links$width-mean(links$width))/mean(links$width)
# links$width <- scale(links$width)[, 1]
links$width <- links$width*1.5/mean(links$width)

### nodes ###
nodes <- data.frame(products_amount)
nodes$id <- rownames(nodes)
# nodes$value <- round(nodes$products_amount*100/sum(nodes$products_amount), 1)
nodes$value <- nodes$products_amount
nodes$label <- nodes$id

# hover text 
nodes$title <- paste0("Value: ", round(nodes$value, 2))
links$title <- paste0("Frequent: ", amount)


# ---------------------------------------------
# Shiny App
# ---------------------------------------------
library(plotly)

product_customer$TRANS_ID <- rownames(product_customer)
product_customer <- product_customer[, c(ncol(product_customer), 1:(ncol(product_customer)-1))]

ui <- fluidPage(
  
  theme = shinytheme("spacelab"),
  
  # Some help text
  h2("Transaction Networking"),
  h4("This network plot shows how often customers bought a product(nodes) and how often customers bought two products together(links)"),
  tags$ol(
    tags$li("Select a network node"),
    tags$li("the bar chart shows how often all other products were bought with the selected product"),
    tags$li("the datatable below shows the transactions which contain the selected product")
  ),
  
  # fluidRow(
  fixedRow(
    column(
      # network
      width = 7, visNetworkOutput("network", height = "600px")
    ),
    column(
      # barchart
      width = 5, plotlyOutput(outputId = "barchart", height = "600px")
    )
  ),
  
  # verbatimTextOutput("zoom"),
  
  fixedRow(
    htmlOutput("table_text"),
    # data table
    dataTableOutput('table')
  )
  
)


server <- function(input, output) {
  
  nodes$group <- "B"
  
  # network, event click
  output$network <- renderVisNetwork({
    visNetwork(nodes, links) %>% 
      visPhysics(solver = "repulsion") %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
      visGroups(groupname = "B", color = list(background = "#EEE8AA", border = "#808080",
                                              highlight = "#FFD700", hover = "#FFD700")) %>%
      visInteraction(multiselect = T) %>% 
      visEvents(
        # click = "function(nodes){
        #         Shiny.onInputChange('click', nodes);
        #         ;}")
        click = "function(nodes) {
        console.info('click')
        console.info(nodes)
        Shiny.onInputChange('click', {nodes : nodes.nodes, links : nodes.links});
        ;}")
    
})
  
  # barchart
  output$barchart <- renderPlotly({
    col <- rep("#C0C0C0", 16)
    
    if (!is.null(input$click$nodes) &&  length(input$click$nodes) != 0) {
      # col <- rep("#EEE8AA", 16)
      i <- which(nodes$label == input$click$nodes[[1]])
      show <- product_customer[which(product_customer[, i] != 0), -1]
      show <- colSums(show)
      show <- data.frame(show)
      show$label <- rownames(show)
      col[i] <- "#FFD700" # select color
      plot_ly(show, x = ~label, y = ~show,
              name = "Amount", type = "bar",
              marker = list(color = col)) %>%
        layout(xaxis = list(title = "Products"), yaxis = list(title = "Amount"))
    } else {
      # col <- rep("#C0C0C0", 16)
      plot_ly(nodes, x = ~label, y = ~products_amount,
              name = "Amount", type = "bar",
              marker = list(color = col)) %>%
        layout(xaxis = list(title = "Products"), yaxis = list(title = "Amount"))
    }
    
  })
  
  # Print the selected node name
  output$table_text <- renderText({
    if (!is.null(input$click$nodes) &&  length(input$click$nodes) != 0) {
      HTML(" You've selected <code>", input$click$nodes[[1]], "</code>",
           " <br><br>Here are the transactions that ",
           " contain ", input$click$nodes[[1]], ":")
    }
    else {
      return("")
    }
  })
  
  # data table
  output$table <- renderDataTable({
    
    if (!is.null(input$click$nodes) && length(input$click$nodes) != 0) {
      col <- which(names(product_customer) == input$click$nodes[[1]])
      show <- product_customer[which(product_customer[, col] != 0), ]
      show[, c(1, col, setdiff(2:ncol(product_customer), col))]
    } else {
      data.frame()
    }
    
  })
  
  # output$zoom <- renderPrint({
  #   !is.null(input$click$nodes) && length(input$click$nodes) != 0
  # })
  
  
  }



shinyApp(ui = ui, server = server)





