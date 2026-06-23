library(stringr)
library(shiny)
library(plotly)


source("R/nw_algorithm.R")
source("R/nw_traceback.R")
source("R/nw_explanation.R")


ui <- fluidPage(

  titlePanel("Needleman–Wunsch Genome3D Tutorial"),

  sidebarLayout(

    sidebarPanel(

      # textInput("seq1", "Sequence 1", "GATTACA"),
      # textInput("seq2", "Sequence 2", "GACTACG"),

      textInput("seq1", "Sequence 1", "TACGAGACTCAATACA"),
      textInput("seq2", "Sequence 2", "TACAAGAAATACA"),

      numericInput("match", "Match", 1),
      numericInput("mismatch", "Mismatch", -1),
      numericInput("gap", "Gap", -2),

      actionButton("run", "Run"),

      sliderInput("step", "Step",
                  min = 1, max = 1, value = 1, round = 0),

      sliderInput(
            "tb_step",
            "Traceback step",
            min = 1,
            max = 1,
            value = 1,
            round = 0
          )
    ),

    mainPanel(

      tabsetPanel(
        tabPanel("Matrix",
                 plotlyOutput("matrix"),
                 verbatimTextOutput("explain"),
                 verbatimTextOutput("alignment")) 
                )
    )
  ),

  checkboxInput("show_arrows", "Show DP arrows", value = FALSE),
  
)

server <- function(input, output, session) {

  res <- eventReactive(input$run, {

    run_nw_matrix(
      input$seq1,
      input$seq2,
      input$match,
      input$mismatch,
      input$gap
    )

  })
  
  observeEvent(res(), {
    updateSliderInput(session,
                      "step",
                      min = 1,
                      max = length(res()$steps),
                      value = 1)
  })

  observe({
    updateSliderInput(
      session,
      "step",
      max = length(res()$steps)
    )
  })

  observeEvent(tb(), {
      updateSliderInput(
        session,
        "tb_step",
         min = 1,
         max = length(tb()$steps),
         value = 1
      )
    })

   
  output$matrix <- renderPlotly({

    cat("renderPlotly triggered\n")
    
    r <- res()
    k <- input$step

    mat <- build_matrix_step(
      n = nrow(r$score),
      m = ncol(r$score),
      steps = r$steps,
      k = k
    )
    
    step <- r$steps[[k]]

    zmin <- min(mat, na.rm = TRUE)
    zmax <- max(mat, na.rm = TRUE)

    df <- expand.grid(
      x = 1:ncol(mat),
      y = 1:nrow(mat)
    )

    df$z <- as.vector(mat)
    step <- r$steps[[input$step]]
    
    arrow_layer <- matrix("", nrow(mat), ncol(mat))
    if (input$show_arrows) {
        arrows <- r$arrow
        step_map <- r$arrow_step

        for (i in seq_len(nrow(mat))) {
          for (j in seq_len(ncol(mat))) {
            if (!is.na(step_map[i, j]) && step_map[i, j] <= input$step) {
              arrow_layer[i, j] <- arrows[i, j]
            }
          }
        }
      }
    
    tb_obj <- tb()

    step <- tb_obj$steps[[input$tb_step]]
    tb_path <- tb_obj$path[seq_len(input$tb_step), ]

    trace_layer <- matrix("", nrow(mat), ncol(mat))
    for (tb_k in seq_len(nrow(tb_path))) {
      trace_layer[tb_path$i[tb_k], tb_path$j[tb_k]] <- "●"
    }
    
    
    display <- matrix(as.character(mat), nrow = nrow(mat), ncol = ncol(mat))
    # Arrows first
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        if (arrow_layer[i, j] != "") {
          display[i, j] <- paste0(arrow_layer[i, j], " ", display[i, j])
        }
      }
    }

    # #  Then traceback info
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {

        if (trace_layer[i, j] != "") {
          display[i, j] <- paste0("● ", display[i, j])
        }
      }
    }
    display[is.na(mat)] <- ""

    # Now the axis
    # x axis
    xseq = input$seq1
    xtickvals = 1:ncol(mat)
    xticktext = str_split(xseq,"")[[1]]
    # y axis
    yseq = input$seq2
    ytickvals = 1:nrow(mat)
    yticktext = str_split(yseq,"")[[1]]

    plot <- plot_ly(
        z = mat,
        text = display,
        type = "heatmap",
        zmin = zmin,
        zmax = zmax,
        colorscale = list(
          c(0, "red"),
          c(0.5, "white"),
          c(1, "green")
        )
      ) |>
      layout(
        xaxis = list(side = "top",
                     tickmode = "array",
                     tickvals = xtickvals,
                     ticktext = xticktext),
        yaxis = list(autorange = "reversed",
                     tickmode = "array",
                     tickvals = ytickvals,
                     ticktext = yticktext)
        #title = paste("Filling step:", k)
      ) |>
      style(
        texttemplate = "%{text}",      # <- display numbers
        textfont = list(color = "black")
      )
    #str(plot$x$data)
    plot
  })

  output$explain <- renderText({
    
    explain_cell(
      res()$steps[[input$step]]
    )
  })

  tb <- reactive({

    req(res())
    cat("Here in tb reactive\n")
    traceback_nw(
      res()$pointer,
      res()$seq1,
      res()$seq2
    )
  })


  alignment <- reactive({
    r <- res()

    traceback_nw(
      pointer = r$pointer,
      seq1    = r$seq1,
      seq2    = r$seq2
    )
  })


  output$alignment <- renderText({
    
    cat("Alignment renderText triggered\n")

    tb_steps <- tb()$steps
    step <- tb_steps[[input$tb_step]]

    cat(step$partial1)
    cat(step$partial2)
    
    # prefix_size <- length(tb_steps) - round(input$tb_step)
    # prefix = strrep("*", prefix_size)
 
    paste(
      step$partial1,
      match_line(step$partial1, step$partial2),
      step$partial2,
      sep = "\n"
    )
  })
  output$path <- renderTable({
    tb()$path
  })
}

shinyApp(ui, server)