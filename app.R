library(shinycssloaders)
library(shiny)
library(ggplot2)
library(reshape2)


ui <- fluidPage(
  titlePanel("Black-Scholes Option Pricing"),
  sidebarLayout(
    sidebarPanel(
      numericInput("K", "Strike Price (K)", value = 100),
      numericInput("t", "Time to Maturity (t)", value = 1),
      numericInput("r", "Risk-free Rate (r)", value = 0.05, step = 0.01),
      sliderInput("S0_range", "Spot Price Range (S0)", min = 50, max = 150, value = c(76, 114)),
      sliderInput("sigma_range", "Volatility Range (sigma)", min = 0.01, max = 0.5, value = c(0.09, 0.27)),
      actionButton("update", "Update Heatmaps"),
      downloadButton("downloadCall", "Download Call Heatmap"),
      downloadButton("downloadPut", "Download Put Heatmap")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Call Heatmap", withSpinner(plotOutput("callHeatmap"))),
        tabPanel("Put Heatmap", withSpinner(plotOutput("putHeatmap")))
      )
    )
  ),
  tags$footer(
    HTML('<div style="text-align:center; padding: 10px; background-color: #f8f9fa; position: fixed; bottom: 0; width: 100%;">
           Developed by Samuele Aglieco| 
           <a href="https://www.linkedin.com/in/samuele-aglieco-466263175?utm_source=share&utm_campaign=share_via&utm_content=profile&utm_medium=ios_app" target="_blank">LinkedIn</a> |
           <a href="https://github.com/samuxx99"_blank">GitHub</a>
         </div>')
  )
)


server <- function(input, output) {
  observeEvent(input$update, {
    S0_values <- seq(input$S0_range[1], input$S0_range[2], by = 4)
    sigma_values <- seq(input$sigma_range[1], input$sigma_range[2], by = 0.02)
    
    callPrices <- matrix(nrow = length(sigma_values), ncol = length(S0_values))
    for (i in 1:length(sigma_values)) {
      for (j in 1:length(S0_values)) {
        callPrices[i, j] <- bsPricing_call(S0_values[j], input$K, input$r, input$t, sigma_values[i])
      }
    }
    callPrices_df <- data.frame(callPrices)
    colnames(callPrices_df) <- S0_values
    callPrices_df$Volatility <- sigma_values
    melted_call <- melt(callPrices_df, id.vars = "Volatility")
    melted_call$formattedValues <- sprintf("%.2f", melted_call$value)
    
    output$callHeatmap <- renderPlot({
      ggplot(melted_call, aes(x = variable, y = Volatility, fill = value)) +
        geom_tile() +
        geom_text(aes(label = formattedValues), color = "white", size = 3) +
        scale_fill_gradient(low = "blue", high = "yellow") +
        labs(title = "CALL", x = "PRICE SPOT", y = "VOLATILITY", fill = "Price Call") +
        theme_minimal()
    })
    
    putPrices <- matrix(nrow = length(sigma_values), ncol = length(S0_values))
    for (i in 1:length(sigma_values)) {
      for (j in 1:length(S0_values)) {
        putPrices[i, j] <- bsPricing_put(S0_values[j], input$K, input$r, input$t, sigma_values[i])
      }
    }
    putPrices_df <- data.frame(putPrices)
    colnames(putPrices_df) <- S0_values
    putPrices_df$Volatility <- sigma_values
    melted_put <- melt(putPrices_df, id.vars = "Volatility")
    melted_put$formattedValues <- sprintf("%.2f", melted_put$value)
    
    output$putHeatmap <- renderPlot({
      ggplot(melted_put, aes(x = variable, y = Volatility, fill = value)) +
        geom_tile() +
        geom_text(aes(label = formattedValues), color = "white", size = 3) +
        scale_fill_gradient(low = "blue", high = "yellow") +
        labs(title = "PUT", x = "PRICE SPOT", y = "VOLATILITY", fill = "Price Put") +
        theme_minimal()
    })
  })
  
  output$downloadCall <- downloadHandler(
    filename = function() {
      paste("Call_Heatmap", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      ggplot(melted_call, aes(x = variable, y = Volatility, fill = value)) +
        geom_tile() +
        geom_text(aes(label = formattedValues), color = "white", size = 3) +
        scale_fill_gradient(low = "blue", high = "yellow") +
        labs(title = "CALL", x = "PRICE SPOT", y = "VOLATILITY", fill = "Price Call") +
        theme_minimal()
      dev.off()
    }
  )
  
  output$downloadPut <- downloadHandler(
    filename = function() {
      paste("Put_Heatmap", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      ggplot(melted_put, aes(x = variable, y = Volatility, fill = value)) +
        geom_tile() +
        geom_text(aes(label = formattedValues), color = "white", size = 3) +
        scale_fill_gradient(low = "blue", high = "yellow") +
        labs(title = "PUT", x = "PRICE SPOT", y = "VOLATILITY", fill = "Price Put") +
        theme_minimal()
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
