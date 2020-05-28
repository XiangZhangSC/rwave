#' Shiny app for Seahorse
#'
#' shiny_seahorse lauches a shiny app for visualization and communication of Seahorse analysis
#'
#' @param seahorse_rate_data Seahorse rate data produced by \code{import_seahorse}
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import shinyWidgets
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
        pickerInput(inputId = 'group_id', 
                    label = 'Which experimental condition are you interested?', 
                    choices = unique(seahorse_rate_data$Group), 
                    selected = "Background", 
                    multiple = TRUE), 
        pickerInput(inputId = 'ocr_id', 
                    label = 'Which OCR do you want to compare between different conditions?', 
                    choices = c("basal_ocr", "max_ocr", "atp_ocr", "proton_leak"), 
                    selected = "basal_ocr", 
                    multiple = TRUE)
      ), 
      ## things users will see
      mainPanel(
        tabsetPanel(
          tabPanel(title = 'Data', 
                   plotly::plotlyOutput('plot_ocr_line'), 
                   plotly::plotlyOutput(('plot_ecar_line'))), 
          tabPanel(title = 'Summary', DT::DTOutput('table_ocr_summary')), 
          tabPanel(title = 'Analysis', plotly::plotlyOutput('plot_group_comparison')), 
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
      seahorse_rate_data %>% 
        filter(Group %in% input$group_id) %>% 
        sketch_ocr()
    })
    
    output$plot_ecar_line <- plotly::renderPlotly({
      seahorse_rate_data %>% 
        filter(Group %in% input$group_id) %>% 
        sketch_ecar()
    })
    
    output$table_ocr_summary <- DT::renderDT({
      summarize_ocr(seahorse_rate_data, which_level = "group") %>% 
        DT::datatable()
    })
    
    output$plot_group_comparison <- plotly::renderPlotly({
      seahorse_rate_data %>% 
        compare_ocr() %>% 
        sketch_comparison_ocr(which_ocr = input$ocr_id)
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

