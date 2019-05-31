#' Insert Special Character
#'
#' A Shiny Gadget to select special character and insert into the cursor
#' position. Insertion can be for HTML named entities or hexadecimal or decimal codes.
#'
#' Other character sets not included in W3C may also be loaded if they are in the same
#' format as \code{\link{w3charref}}. Then the dataframe can be passed in to
#' \code{insertChar} as \code{insertChar(char_df = custom_charset)}.
#'
#' @param input_char_df dataframe of W3C character reference. See \code{?w3charref}.
#'
#' @return NULL value. Character codes are inserted when the "Insert *" button is
#' pressed
#' @export
#' @examples
#' if(interactive()){
#'   insertChar(input_char_df = insertChar::w3charref)
#' }

insertChar <- function(input_char_df = insertChar::w3charref){
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Insert Special Characters"),
    miniUI::miniContentPanel(
      shiny::tags$p("Click any button or", shiny::actionLink("help_btn", "Help"), "to begin."),
      shiny::tags$script("$(document).on('click', '.dynamic button', function () {
                              Shiny.onInputChange('lastSelectId',this.id);
                              // to report changes on the same selectInput
                              Shiny.onInputChange('lastSelect', Math.random());
                             });"),
      shiny::uiOutput("help_box"),
      shiny::uiOutput("go"),
      shiny::selectInput("block", "Character Set",
                         choices = c("All", levels(insertChar::w3charref$block)),
                         multiple = F,
                         selected = "C1 Controls and Latin-1 Supplement"),
      shiny::selectInput("char_code", "Character Encoding",
                         choices = c("Hexadecimal" = 3,
                                     "Decimal" = 4,
                                     "HTML Entity" = 5)),
      shiny::uiOutput("action_buttons")
    )
  )



  server <- function(input, output, session) {
    # input data frame
    input_char_df <- input_char_df
    # generate filtering data frame
    char_df <- shiny::reactive({
      char_df <- input_char_df
      if(input$block == "All"){
        char_df <- char_df
      } else {
        char_df <- char_df[char_df$block == input$block,]
      }
    })

    # set up action button list
    rvs = shiny::reactiveValues(buttons = list())
    shiny::observe({
      for (i in 1:(nrow(char_df()))){
        rvs$buttons[[i]] = shiny::actionButton(inputId = paste0("button-", i),
                                               label = shiny::HTML(paste0(char_df()$character[i])), width = "50px", style="height: 50px; white-space:normal; font-size:125%", title = paste0(char_df()$names[i]))

      }

    })
    # render action buttons
    output$action_buttons <- shiny::renderUI({
      shiny::withProgress(do.call(shiny::fluidRow, list(rvs$buttons[1:nrow(char_df())], class="dynamic")), message = "Loading")

    })

    # observeEvent(input$lastSelect, {
    #   if (!is.null(input$lastSelectId)) {
    #     cat("lastSelectId:", gsub("button-", "", input$lastSelectId), "\n")
    #     row_value <- gsub("button-", "", input$lastSelectId)
    #     cat("Selection:", input[[input$lastSelectId]], "\n\n")
    #     char_df <- char_df()
    #     print(char_df[as.numeric(gsub("button-", "", input$lastSelectId)), as.numeric(input$char_code)])
    #   }
    # })
    insertValue <- shiny::eventReactive(input$lastSelect, {
      if (!is.null(input$lastSelectId)) {
        #cat("lastSelectId:", gsub("button-", "", input$lastSelectId), "\n")
        row_value <- gsub("button-", "", input$lastSelectId)
        #cat("Selection:", input[[input$lastSelectId]], "\n\n")
        char_df <- char_df()
        return((char_df[as.numeric(gsub("button-", "", input$lastSelectId)), as.numeric(input$char_code)]))
      }
    })
    # actually inserts the code
    output$go <- shiny::renderUI({
      shiny::actionButton("insert", shiny::HTML(paste0("Insert ", insertValue())))
    })
    shiny::observeEvent(input$insert, {
      char_df <- char_df()
      cursor_location <- rstudioapi::getActiveDocumentContext()
      rstudioapi::insertText(location = cursor_location[["selection"]][[1]]$range,
                             text = as.character(char_df[as.numeric(gsub("button-", "", input$lastSelectId)), as.numeric(input$char_code)]),
                             id = cursor_location$id)
    })
    # shows help
    # observeEvent(input$help_btn,
    #              output$help_box <- renderUI({
    #                shiny::helpText("Hover over buttons for symbol descriptions. \n
    #                   Click button to select symbol. \n
    #                   Press the 'insert' button to insert at cursor position. \n
    #                   View different character sets and select between hexadecimal, decimal and HTML encoding insertion using the pulldown menus.")
    #
    # }))

    shiny::observeEvent(input$help_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Help",
        shiny::HTML("Hover over buttons for symbol descriptions. <br>
                    Click button to select symbol. <br>
                    Press the 'insert' button to insert at cursor position. <br>
                    View different character sets and select between hexadecimal, decimal and HTML encoding insertion using the pulldown menus."),
        easyClose = TRUE
      ))
    })
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })

  }


  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 400))

}



