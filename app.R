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
  preweight = list(label="Weight (kg) after first dialysis", min=50, max=150, value=70, step=0.1, dec=1),
  postweight = list(label="Weight (kg) before second dialysis", min=50, max=150, value=70, step=0.1, dec=1)
)

# -------------------------
# User Interface (UI)
# -------------------------
ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="RID-AKI-90"), # app title
  dashboardSidebar(
    sidebarMenu(
      # Menu items corresponding to different tabs
      menuItem("Calculate Risk", tabName="calculator", icon=icon("calculator")),
      menuItem("About", tabName="abstract", icon=icon("info-circle")),
      menuItem("Model", tabName="model", icon=icon("toolbox")),
      menuItem("Disclaimer", tabName="disclaimer", icon=icon("exclamation")),
      menuItem("Supplier", tabName="supplier", icon=icon("tools"))
    )
  ),
  dashboardBody(
    useShinyjs(),  # enable shinyjs for hiding/showing UI elements dynamically
    tabItems(
      # -------------------------
      # Calculator tab
      # -------------------------
      tabItem(tabName="calculator",
              fluidRow(
                # Disclaimer box at top
                box(width=12, status="warning", collapsible=TRUE,
                    title="Disclaimer",
                    "This model is provided for educational, training, and informational purposes only. It must not be used to guide medical decisions or provide diagnostic services. Tufts Medical Center is not responsible for any decisions made based on this tool."
                ),
                # Patient demographic inputs
                box(width=6, status="primary", title="Patient Characteristics",
                    selectInput("sex","Sex", choices=c("Male","Female")),
                    sliderInput("age","Age (years)", min=18, max=97, value=67, step=1),
                    selectInput("htn_dci","History of hypertension", choices=c("No","Yes")),
                    selectInput("htn_3meds_dci_30d","Hypertension requiring ≥3 meds", choices=c("No","Yes"))
                ),
                # Lab measurement inputs (numeric)
                box(width=6, status="primary", title="Lab Measurements",
                    # dynamically generate numeric inputs from numeric_inputs list
                    lapply(names(numeric_inputs)[1:5], function(id) {
                      ni <- numeric_inputs[[id]]
                      numericInput(id, ni$label, min=ni$min, max=ni$max, value=ni$value, step=ni$step)
                    })
                ),
                # Dialysis weights + calculate button
                box(width=12, status="primary", title="Dialysis Characteristics",
                    column(3,
                           numericInput("preweight", numeric_inputs$preweight$label,
                                        min=numeric_inputs$preweight$min,
                                        max=numeric_inputs$preweight$max,
                                        value=numeric_inputs$preweight$value,
                                        step=numeric_inputs$preweight$step),
                           numericInput("postweight", numeric_inputs$postweight$label,
                                        min=numeric_inputs$postweight$min,
                                        max=numeric_inputs$postweight$max,
                                        value=numeric_inputs$postweight$value,
                                        step=numeric_inputs$postweight$step),
                           actionButton("calculateButton","Calculate", icon=icon("calculator"), class="btn-lg")
                    )
                ),
                # Hidden results panel (only shown after calculation)
                shinyjs::hidden(div(id="results.panel", infoBoxOutput("recovery_box", width=12)))
              )
      ),
      # -------------------------
      # Other tabs
      # -------------------------
      tabItem(tabName="abstract", h2("Abstract"), htmlOutput("abstract")),
      tabItem(tabName="model", h2("Model"), includeHTML("html/model.html")),
      tabItem(tabName="disclaimer", h2("Disclaimer"), includeHTML("html/disclaimer.html")),
      tabItem(tabName="supplier", includeHTML("html/supplier.html"))
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
      htn_dci           = ifelse(input$htn_dci == "Yes", 1, 0),
      htn_3meds_dci_30d = ifelse(input$htn_3meds_dci_30d == "Yes", 1, 0),
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
    infoBox(width=12, color="orange",
            title="Predicted probability of recovery in dialysis-dependent AKI at 90 days",
            value=paste0(sprintf("%.1f", pred.prob()*100), "%"))
  })

  # Show results panel when calculate button is clicked
  observeEvent(input$calculateButton, { shinyjs::show("results.panel") })

  # Hide results panel if any input changes
  observeEvent({ c(input$age, input$sex, input$htn_dci, input$htn_3meds_dci_30d,
                   input$albumin_baseline, input$scr, input$pth_baseline, input$plt_baseline,
                   input$potassium_baseline, input$preweight, input$postweight)}, {
                     shinyjs::hide("results.panel")
                   })
}

# Launch the Shiny app
shinyApp(ui, server)
