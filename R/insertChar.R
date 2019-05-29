#' Insert Special Character
#'
#' A Shiny Gadget to select special character and insert into the cursor
#' position. Insertion can be for HTML named entities or hexadecimal or decimal codes.
#'
#' Other character sets not included in W3C may also be loaded if they are in the same
#' format as \code{\link{w3charref}}. Then the dataframe can be passed in to
#' \code{insertChar} as \code{insertChar(char_df = custom_charset)}.
#'
#' @param char_df dataframe of W3C character reference. See \code{?w3charref}.
#'
#' @return NULL value. Character codes are inserted when the "Insert *" button is
#' pressed
#' @export
#' @examples
#' if(interactive()){
#'   insertChar(char_df = insertChar::w3charref)
#' }

insertChar <- function(char_df = insertChar::w3charref){
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Insert Special Character"),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(width = 6, shiny::uiOutput("preview_symbol")),
        shiny::column(width = 6, shiny::radioButtons("char_code", "Character Encoding", choices = c("Hexadecimal" = "hex", "Decimal" = "dec", "HTML Entity" = "ent"), width = "200px"))
      ),
      shiny::tags$h1(""),
      DT::DTOutput("symbol_table")
    )
  )
  server <- function(input, output, session) {
    # data table instance
    output$symbol_table <- DT::renderDataTable({
      char_df[,c("character", "names", "block", "category")]
    }, escape = 1, rownames = F, selection = "single")
    # render ui of selected row
    output$preview_symbol <- shiny::renderUI({
      if(is.null(input$symbol_table_rows_selected)){
        shiny::actionButton("symbol",
                            shiny::HTML("Select a symbol<br>from table <br>below to preview"),
                            width = "150px",
                            height = "100px", `word-wrap` = "normal",)
      } else {
        shiny::actionButton("symbol", shiny::HTML(paste0("Insert <br><font size='120px'>",
                                           char_df$character[input$symbol_table_rows_selected],
                                           "</font>")), width = "100px", height = "100px")
      }

    })
    # observe and insert
    shiny::observeEvent(input$symbol, {
      cursor_location <- rstudioapi::getActiveDocumentContext()
      rstudioapi::insertText(location = cursor_location[["selection"]][[1]]$range,
                             text = as.character(char_df[input$symbol_table_rows_selected, input$char_code]),
                             id = cursor_location$id)
    })
    # gadget end
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Insert Special Character"))
}

