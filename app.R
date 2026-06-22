library(rms)
library(shiny)
library(shinyjs)        # for hiding/showing elements dynamically
library(shinydashboard) # dashboard layout
library(shinyBS)        # optional Bootstrap components

# Load pre-trained Cox model
load("model.Rdata")     # bw.model must exist in this file

# Define numeric inputs with metadata for UI sliders/numeric inputs
numeric_inputs <- list(
  potassium_baseline = list(label="Serum Potassium (mmol/L)", min=2, max=8, value=4, step=0.1, dec=1),
  scr = list(label="Pre-dialysis Serum Creatinine (mg/dL)", min=1, max=17, value=4, step=0.01, dec=2),
  albumin_baseline = list(label="Serum Albumin (g/dL)", min=0, max=5, value=3, step=0.1, dec=1),
  pth_baseline = list(label="Parathyroid hormone (pg/mL)", min=8, max=2800, value=175, step=1, dec=0),
  plt_baseline = list(label="Platelet count (x10E3/uL)", min=100, max=1000, value=241, step=1, dec=0),
  preweight = list(label="Weight (kg) after most recent dialysis", min=50, max=150, value=70, step=0.1, dec=1),
  postweight = list(label="Weight (kg) before current dialysis", min=50, max=150, value=70, step=0.1, dec=1)
)

# -------------------------
# User Interface (UI)
# -------------------------
ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title=span(icon("heartbeat"), "RID-AKI-90")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculate Risk", tabName="calculator", icon=icon("calculator")),
      menuItem("About", tabName="abstract", icon=icon("info-circle")),
      menuItem("Model", tabName="model", icon=icon("toolbox")),
      menuItem("Disclaimer", tabName="disclaimer", icon=icon("exclamation")),
      menuItem("Supplier", tabName="supplier", icon=icon("tools"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      /* General */
      .content-wrapper { background-color: #f4f6f9; }
      .content { padding: 20px 25px; }

      /* Box styling */
      .box { border-radius: 8px; border-top: 3px solid #3c8dbc; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
      .box-header { border-radius: 8px 8px 0 0; }
      .box.box-primary { border-top-color: #3c8dbc; }

      /* Disclaimer alert */
      .disclaimer-alert {
        background: #fff8e1; border-left: 4px solid #ffa000; border-radius: 6px;
        padding: 14px 18px; margin-bottom: 20px; color: #5d4037;
        font-size: 13.5px; line-height: 1.5;
      }
      .disclaimer-alert .fa { color: #ffa000; margin-right: 8px; }

      /* Section headers inside boxes */
      .box-title { font-weight: 600; font-size: 16px; color: #2c3e50; }

      /* Input labels */
      .control-label { font-weight: 500; color: #34495e; margin-bottom: 4px; }

      /* Calculate button */
      #calculateButton {
        background: linear-gradient(135deg, #3c8dbc, #2c6fa0); color: white;
        border: none; border-radius: 6px; padding: 12px 36px;
        font-size: 16px; font-weight: 600; letter-spacing: 0.5px;
        margin-top: 10px; transition: all 0.2s ease;
      }
      #calculateButton:hover { background: linear-gradient(135deg, #2c6fa0, #1a5276); transform: translateY(-1px); box-shadow: 0 4px 12px rgba(44,111,160,0.3); }

      /* Result display */
      .result-panel { margin-top: 15px; }
      .result-panel .info-box { border-radius: 8px; box-shadow: 0 3px 12px rgba(0,0,0,0.1); min-height: 90px; }
      .result-panel .info-box-icon { border-radius: 8px 0 0 8px; }
      .result-panel .info-box-content { padding: 12px 15px; }
      .result-panel .info-box-text { font-size: 14px; font-weight: 500; }
      .result-panel .info-box-number { font-size: 36px; font-weight: 700; color: #2c3e50; }

      /* Numeric inputs */
      .form-control { border-radius: 4px; border: 1px solid #cbd5e0; transition: border-color 0.2s; }
      .form-control:focus { border-color: #3c8dbc; box-shadow: 0 0 0 2px rgba(60,141,188,0.15); }

      /* Slider styling */
      .irs--shiny .irs-bar { background: #3c8dbc; border-top: 1px solid #3c8dbc; border-bottom: 1px solid #3c8dbc; }
      .irs--shiny .irs-single { background: #3c8dbc; }
      .irs--shiny .irs-from, .irs--shiny .irs-to { background: #3c8dbc; }
      .irs--shiny .irs-handle { border: 2px solid #3c8dbc; }

      /* Content tabs (About, Model, etc.) */
      .tab-content-page { max-width: 800px; }
      .tab-content-page h2 { color: #2c3e50; font-weight: 600; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px; display: inline-block; }
      .tab-content-page p { font-size: 15px; line-height: 1.7; color: #4a5568; }

      /* Sidebar styling */
      .sidebar-menu > li > a { font-size: 14px; padding: 12px 15px; }
      .sidebar-menu > li.active > a { border-left: 3px solid #3c8dbc; }
    "))),
    tabItems(
      tabItem(tabName="calculator",
              fluidRow(
                column(12,
                  div(class="disclaimer-alert",
                      icon("exclamation-triangle"),
                      "This model is provided for educational, training, and informational purposes only.",
                      "It must not be used to guide medical decisions or provide diagnostic services.",
                      "Tufts Medical Center is not responsible for any decisions made based on this tool."
                  )
                )
              ),
              fluidRow(
                box(width=6, status="primary", solidHeader=FALSE,
                    title=span(icon("user"), "Patient Characteristics"),
                    selectInput("sex", "Sex", choices=c("Male","Female")),
                    sliderInput("age", "Age (years)", min=18, max=97, value=67, step=1),
                    selectInput("chf_dci_7d", "History of congestive heart failure", choices=c("No","Yes")),
                    selectInput("htn_3meds_dci_7d", "Hypertension requiring ≥3 meds", choices=c("No","Yes"))
                ),
                box(width=6, status="primary", solidHeader=FALSE,
                    title=span(icon("flask"), "Lab Measurements"),
                    lapply(names(numeric_inputs)[1:5], function(id) {
                      ni <- numeric_inputs[[id]]
                      numericInput(id, ni$label, min=ni$min, max=ni$max, value=ni$value, step=ni$step)
                    })
                )
              ),
              fluidRow(
                box(width=12, status="primary", solidHeader=FALSE,
                    title=span(icon("weight-scale"), "Dialysis Characteristics"),
                    column(4,
                           numericInput("preweight", numeric_inputs$preweight$label,
                                        min=numeric_inputs$preweight$min,
                                        max=numeric_inputs$preweight$max,
                                        value=numeric_inputs$preweight$value,
                                        step=numeric_inputs$preweight$step)
                    ),
                    column(4,
                           numericInput("postweight", numeric_inputs$postweight$label,
                                        min=numeric_inputs$postweight$min,
                                        max=numeric_inputs$postweight$max,
                                        value=numeric_inputs$postweight$value,
                                        step=numeric_inputs$postweight$step)
                    ),
                    column(4,
                           div(style="padding-top: 25px;",
                               actionButton("calculateButton", "Calculate",
                                            icon=icon("calculator"), class="btn-lg btn-block"))
                    )
                )
              ),
              fluidRow(
                shinyjs::hidden(
                  div(id="results.panel", class="result-panel",
                      column(12, infoBoxOutput("recovery_box", width=12))
                  )
                )
              )
      ),
      tabItem(tabName="abstract",
              div(class="tab-content-page", h2("Abstract"), htmlOutput("abstract"))),
      tabItem(tabName="model",
              div(class="tab-content-page", h2("Model"), includeHTML("html/model.html"))),
      tabItem(tabName="disclaimer",
              div(class="tab-content-page", h2("Disclaimer"), includeHTML("html/disclaimer.html"))),
      tabItem(tabName="supplier",
              div(class="tab-content-page", includeHTML("html/supplier.html")))
    )
  )
)

# -------------------------
# Server logic
# -------------------------
server <- function(input, output, session) {

  # Ensure numeric inputs respect decimal limits dynamically
  observe({
    for (id in names(numeric_inputs)) {
      val <- input[[id]]
      if (!is.null(val) && is.numeric(val)) {
        updateNumericInput(session, id, value=round(val, numeric_inputs[[id]]$dec))
      }
    }
  })

  # Reactive expression to compute predicted probability
  pred.prob <- reactive({
    # obtain input from user
    input.patient <- data.frame(
      age               = input$age,
      sex               = ifelse(input$sex == "Male", 1, 0),
      chf_dci_7d           = ifelse(input$chf_dci_7d == "Yes", 1, 0),
      htn_3meds_dci_7d = ifelse(input$htn_3meds_dci_7d == "Yes", 1, 0),
      albumin_baseline  = input$albumin_baseline,
      scr_baseline      = input$scr,
      scr_baseline_2    = input$scr^2,
      pth_baseline      = input$pth_baseline,
      plt_baseline      = input$plt_baseline,
      potassium_baseline= input$potassium_baseline,
      IDWG_adj          = input$postweight - input$preweight
    )

    # linear predictor
    lp.patient <- -bw.model$center + sum(bw.model$coefficients * as.numeric(input.patient))

    # Compute predicted probability of recovery at 90 days using baseline hazard
    1 - exp(-mean(h0.shrunk) * exp(shrinkage.factor * lp.patient))
  })

  # Render the predicted probability in an infoBox
  output$recovery_box <- renderInfoBox({
    prob <- pred.prob() * 100
    infoBox(width=12, color="blue", icon=icon("chart-line"),
            title="Predicted probability of kidney recovery within 90 days",
            value=paste0(sprintf("%.1f", prob), "%"),
            subtitle="Dialysis-dependent AKI")
  })

  # Show results panel when calculate button is clicked
  observeEvent(input$calculateButton, { shinyjs::show("results.panel") })

  # Hide results panel if any input changes
  observeEvent({ c(input$age, input$sex, input$chf_dci_7d, input$htn_3meds_dci_7d,
                   input$albumin_baseline, input$scr, input$pth_baseline, input$plt_baseline,
                   input$potassium_baseline, input$preweight, input$postweight)}, {
                     shinyjs::hide("results.panel")
                   })
}

# Launch the Shiny app
shinyApp(ui, server)
