

# https://plot.ly/r/shinyapp-linked-click/







library(shiny)
library(ggplot2)
library(plotly)

# ---------------------------------------------
# read data
# ---------------------------------------------
senic <- read.table("SENIC.txt")
names(senic) <- c("id", "len.of.stay", "age", "infection.risk", "culturing.ratio",
                  "chest.Xray.ratio", "no.beds", "med.school.aff", "region", 
                  "avg.daily.census", "no.nurses", "avble.faci")
# senic data labels
senic_labels <- c("Identification Number", "Length of Stay", "Age", "Infection Risk", "Routine Culturing Ratio", "Routine Chest X-ray Ratio", "Number of Beds", "Medical School Affiliation", "Region", "Average Daily Census", "Number of Nurses", "Available Facilities & Services")

# ---------------------------------------------
# get outliers function
# ---------------------------------------------
get_outliers <- function(df, col.name) {
  # Give a data.frame and a column's name to get the column's outliers
  #
  # Args:
  #  df: a data.frame
  #  col.name: character, one of the column name in df. Has to be numeric
  #
  # Return:
  #  The rows of outliers
  #
  if (!is.character(col.name)) {
    stop(paste(col.name, "has to be charactor."))
  }
  # check if the value of column is numreic
  if (!is.numeric(df[, col.name])) {
    stop(paste("The value of ", col.name, " is not numreic."))
  }
  Q1 <- quantile(df[, col.name], .25)
  Q3 <- quantile(df[, col.name], .75)
  # upper outliers
  outlier.end <- which(df[, col.name] > Q3+1.5*(Q3-Q1))
  # lowers outliers
  outlier.start <- which(df[, col.name] < Q1-1.5*(Q3-Q1))
  return (c(outlier.end, outlier.start))
}


ui <- fluidPage(
  # plot
  plotlyOutput(outputId = "density", height = "600px")
)



server <- function(input, output) {
  
  # ---------------------------------------------
  # response output
  # ---------------------------------------------
  output$density <- renderPlotly({
    
    # eventdata <- event_data("plotly_hover")
    eventdata <- event_data("plotly_click")
    # column name for plots
    columns <- colnames(senic)[c(-1, -8, -9)]
    # if no eventdata it will be TRUE and plot the original graph, otherwise, plot the interact graph
    start <- TRUE
    
    #----------------------------------------
    ##### event(hover or click) happens #####
    #----------------------------------------
    if (!is.null(eventdata)) {
      start <- FALSE

      plots <- lapply(columns, function(column) {
        print (column)
        
        ##### density #####
        # density for plot limits
        density <- density(senic[, column])
        
        # density graphs
        p <- ggplot(senic) +
          theme_bw() 
        name <- senic_labels[which(names(senic) == column)]
        den <- p + geom_density(aes_string(column), colour = "black", fill = "gold1") +
          scale_x_continuous(name = name, limits = range(density$x))

        ##### add outliers #####
        # get outliers
        outlier_rows <- get_outliers(senic, column)
        if (length(outlier_rows != 0)) {
          outliers <- data.frame(OL = senic[outlier_rows, column],
                                 outrow = outlier_rows,
                                 col = "black",
                                 shape = 5)
          outliers$col <- as.character(outliers$col)
        }
        
        
        # get eventdata information
        # click(hover) point's x value
        x <- as.numeric(eventdata$x)[1]
        # click(hover) point's row number in senic data.frame
        r <- which(senic[, columns[(as.numeric(eventdata$curveNumber)[1]-1)/2+1]] == round(x, 2))
        
        if (length(r) != 0) {
          # if the column has no outliers
          # then show the point on the column density's graph as blue point 
          if (length(outlier_rows) == 0) {
            pp <- den + geom_point(aes(senic[r, column], 0), colour = "blue", shape = 1, size = rel(2.3))
          } else {
            # if click point is not the column's outlier point, 
            if (r %in% outliers$outrow) {
              outliers[which(outliers$outrow == r), "col"] <- "red"
              pp <- den + geom_point(data = outliers, aes(OL, 0), colour = outliers$col, shape = outliers$shape, size = rel(2.3))
            } else {
              # if click point is not the column's outlier point, 
              # then show the point on the column density's graph as blue point 
              outliers <- rbind(outliers, data.frame(OL = senic[r, column], outrow = r, col = "blue", shape = 1))
              pp <- den + geom_point(data = outliers, aes(OL, 0), colour = outliers$col, shape = outliers$shape, size = rel(2.3))
            }
          }
          
          ggplotly(pp, height = 700, width = 900)
          
        } else {
          # if click other points instead of outlier
          start <<- TRUE
        }
      })
    }
    
    
    #------------------------------------------------
    ##### no event(hover or click)/ initialized #####
    #------------------------------------------------
    if (start) {
      plots <- lapply(columns, function(column) {
        ##### density #####
        density <- density(senic[, column])
        p <- ggplot(senic) +
          theme_bw() 
        name <- senic_labels[which(names(senic) == column)]
        den <- p + geom_density(aes_string(column), colour = "black", fill = "gold1") +
          scale_x_continuous(name = name, limits = range(density$x)) 
        
        ##### outliers #####
        outliers <- data.frame(OL = senic[get_outliers(senic, column), column])
        if (nrow(outliers) != 0) {
          pp <- den + geom_point(data = outliers, aes(OL, 0), shape = 5, size = rel(2.3))
        } else {
          pp <- den
        }
        
        ggplotly(pp, height = 700, width = 900)
        
      })
      
    }
    # show plots
    subp <- subplot(plots, nrows = 3, shareX = FALSE, titleX = TRUE, margin = 0.05)
    
  })
}


shinyApp(ui = ui, server = server)














