#' Launch shiny app
#'
#' @param ... Not used yet.
#'
#' @return Launch a shiny app.
#' @export
#'
editor <- function(...) {
  ui <- navbarPage("OMUEcon",
          tabPanel("Moodle Page Helper",
                   mdconvertUI("md2html"),
                   value = 1L),
          tabPanel("Code Samples",
                   fluidPage(
                     mainPanel(
                       includeHTML(system.file("www", "samples.html", package = .packageName))
                     )
                   ),
                   value = 2L),
          #tabPanel("Component 3", value = 3L))
  )
  ui <- cookies::add_cookie_handlers(ui)

  server <- function(input, output, session) {

    mdconvertServer("md2html")

  }

  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
