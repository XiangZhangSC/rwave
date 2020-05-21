#' Shiny app for Seahorse
#'
#' shiny_seahorse lauches a shiny app for visualization and communication of Seahorse analysis
#'
#' @param seahorse_rate_data Seahorse rate data produced by \code{import_seahorse}
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @export
shiny_seahorse <- function(seahorse_rate_data) {
  ###################
  # User interface
  ###################
  ui <- fluidPage(
    titlePanel("Shiny app for Seahorse"), 
    
    # Layout
    sidebarLayout(
      ## things users can manipulate
      sidebarPanel(
        selectInput(inputId = 'group_id', 
                    label = 'Which experimental condition are you interested?', 
                    choices = c("Background", "Group 1", "Group 2", "Group 3", "Group 4"), 
                    selected = "Background")
      ), 
      ## things users will see
      mainPanel(
        tabsetPanel(
          tabPanel(title = 'OCR curves', plotly::plotlyOutput('plot_ocr_line')), 
          tabPanel(title = 'OCR Summary', DT::DTOutput('table_ocr_summary')), 
          tabPanel(title = 'Bioenergetic space', plotly::plotlyOutput('plot_bioenergetic_space'))
        )
      )
    )
  )
  
  ##################
  # Server
  ##################
  
  server <- function(input, output) {
    output$plot_ocr_line <- plotly::renderPlotly({
      ggplot(seahorse_rate_data, aes(Time, OCR, group = Well)) + 
        geom_line(alpha = 0.1) + 
        geom_line(data = filter(seahorse_rate_data, Group == input$group_id), color = "steelblue") + 
        theme_classic()
    })
    
    output$table_ocr_summary <- DT::renderDT({
      summarize_ocr(seahorse_rate_data) %>% 
        DT::datatable()
    })
    
    output$plot_bioenergetic_space <- plotly::renderPlotly({
      summarize_apr(seahorse_rate_data, which_assay = "XFe96") %>% 
        sketch_bioenergetic_space()
    })
  }
  
  ###################
  # Launch the app
  ###################
  shinyApp(ui = ui, server = server)
}

