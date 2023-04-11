# Default documents ----

dir.create(.tdir <- tempfile(pattern = "dir"))
.md_file <- file.path(.tdir, "page.md")
.css_file <- file.path(.tdir, "style.css")
.html_file <- file.path(.tdir, "page.html")

.sample_md <-
  paste(readLines(system.file("text", "sample.md", package = "omuecon")),
        collapse = "\n")
.sample_css <-
  paste(readLines(system.file("css", "style.css", package = "omuecon")),
        collapse = "\n")

# UI fragments ----

left_panel <- function(.ns) {
  tabsetPanel(
    type = "tabs",
    #
    # TextArea for Markdown
    #
    tabPanel(
      "Markdown",
      div(
        class = "ta",
        textAreaInput(.ns("md"), "", value = .sample_md)
      ),
      actionButton(.ns("default_md"), "Reset to default document",
                   icon = icon("broom"))
    ),
    #
    # TextArea for CSS
    #
    tabPanel(
      "CSS",
      div(
        class = "ta",
        textAreaInput(.ns("css"), "", value = .sample_css)
      ),
      actionButton(.ns("default_css"), "Reset to default CSS",
                   icon = icon("broom"))
    )
  )
}


right_panel <- function(.ns) {
  fluidRow(
    rclipboard::rclipboardSetup(),
    div(
      id = "right-panel",
      fluidRow(
        column(9,
               actionButton(.ns("convert"),
                            "Convert", icon = icon("file-code")),
               uiOutput(.ns("clip"))
               ),
        column(3,
               downloadButton(.ns("downloadData"), "Download"))
      ),
      hr(),
      htmlOutput(.ns("preview"))
    )
  )
}


# Module UI ----

mdconvertUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(6, left_panel(ns)),
    column(6, right_panel(ns)),
    includeCSS(system.file("www", "css", "style.css", package = .packageName))
  )
}

# Server function ----

mdconvertServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    r <- reactiveValues(html = "")

    # Get cookies once
    observeEvent(
      get_cookie("markdown"), {
        cookie_val <- tryCatch(decode(get_cookie("markdown")),
                               error = function(e) .sample_md)
        updateTextAreaInput(inputId = "md", value = cookie_val)
      },
      once = TRUE
    )
    observeEvent(
      get_cookie("stylesheet"), {
        cookie_val <- tryCatch(decode(get_cookie("stylesheet")),
                               error = function(e) .sample_css)
        updateTextAreaInput(inputId = "css", value = cookie_val)
      },
      once = TRUE
    )


    # Convert button
    observeEvent(input$convert, {

      # Set cookies.
      set_cookie("markdown", encode(input$md))
      set_cookie("stylesheet", encode(input$css))

      # Do the conversion.
      md <- if (stringr::str_length(input$md) == 0) {
        .sample_md
      } else {
        input$md
      }
      writeLines(md, .md_file)

      css <- input$css
      writeLines(css, .css_file)

      r$html <- omuecon::moodle_html(
        .md_file, clip = FALSE, stylesheet = .css_file
      )
      output$preview <- renderText(r$html, sep = "\n")
    })

    # Download Button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("moodle-page-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".zip")
      },
      content = function(file) {
        writeLines(r$html, .html_file)
        utils::zip(file, c(.md_file, .html_file, .css_file), extras = '-j')
      }
    )

    # Reset to default
    observeEvent(input$default_md, {
      updateTextAreaInput(inputId = "md", value = .sample_md)
    })

    observeEvent(input$default_css, {
      updateTextAreaInput(inputId = "css", value = .sample_css)
    })

    # Copy to clipboard
    output$clip <- renderUI({
      output$clip <- renderUI({
        rclipboard::rclipButton(
          inputId = NS(id, "clipbtn"),
          label = "Copy HTML to Clipboard",
          clipText = r$html,
          icon = icon("clipboard")
        )
      })
    })

    observeEvent(input$clipbtn, {
      showNotification("Copied!", type = "message", duration = 2)
    })
  })
}

mdconvertApp <- function() {
  ui <- tags$body(
    fluidPage(
      title = "md2html",
      fluidRow(
        mdconvertUI("test")
      )
    )
  )
  ui <- cookies::add_cookie_handlers(ui)
  server <- function(input, output, server) {
    mdconvertServer("test")
  }
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}


