library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyBS)

# load model
load("model.Rdata")

# numeric input definitions
numeric_inputs <- list(
  pot = list(label="Serum Potassium (mmol/L)", min=2, max=8, value=4, step=0.1, dec=1),
  scr = list(label="Pre-dialysis Serum Creatinine (mg/dL)", min=1, max=17, value=4, step=0.01, dec=2),
  albu = list(label="Serum Albumin (g/dL)", min=0, max=5, value=3, step=0.1, dec=1),
  pth = list(label="Parathyroid hormone (pg/mL)", min=8, max=2800, value=175, step=1, dec=0),
  plt = list(label="Platelet count (x10E3/uL)", min=100, max=1000, value=241, step=1, dec=0),
  preweight = list(label="Weight (kg) before first dialysis", min=50, max=150, value=70, step=0.1, dec=1),
  postweight = list(label="Weight (kg) after first dialysis", min=50, max=150, value=70, step=0.1, dec=1)
)

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="RID-AKI-90"),
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
    tabItems(
      tabItem(tabName="calculator",
              fluidRow(
                box(width=12, status="warning", collapsible=TRUE,
                    title="Disclaimer",
                    "This model is provided for educational, training, and informational purposes only. It must not be used to guide medical decisions or provide diagnostic services. Tufts Medical Center is not responsible for any decisions made based on this tool."
                ),
                # Patient demographics
                box(width=6, status="primary", title="Patient Characteristics",
                    selectInput("sex","Sex", choices=c("Male","Female")),
                    sliderInput("age","Age (years)", min=18, max=97, value=67, step=1),
                    selectInput("htn","History of hypertension", choices=c("No","Yes")),
                    selectInput("htn_3_meds","Hypertension requiring ≥3 meds", choices=c("No","Yes"))
                ),
                # New Lab Measurements Box
                box(width=6, status="primary", title="Lab Measurements",
                    # dynamically generate lab numeric inputs
                    lapply(names(numeric_inputs)[1:5], function(id) {
                      ni <- numeric_inputs[[id]]
                      numericInput(id, ni$label, min=ni$min, max=ni$max, value=ni$value, step=ni$step)
                    })
                ),
                # Dialysis weights and calculate button
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
                shinyjs::hidden(div(id="results.panel", infoBoxOutput("recovery_box", width=12)))
              )
      ),
      tabItem(tabName="abstract", h2("Abstract"), htmlOutput("abstract")),
      tabItem(tabName="model", h2("Model"), includeHTML("html/model.html")),
      tabItem(tabName="disclaimer", h2("Disclaimer"), includeHTML("html/disclaimer.html")),
      tabItem(tabName="supplier", includeHTML("html/supplier.html"))
    )
  )
)

server <- function(input, output, session) {
  # enforce decimal limits dynamically
  observe({
    for (id in names(numeric_inputs)) {
      val <- input[[id]]
      if (!is.null(val) && is.numeric(val)) {
        updateNumericInput(session, id, value=round(val, numeric_inputs[[id]]$dec))
      }
    }
  })

  pred.prob <- reactive({
    IDWC <- input$postweight - input$preweight
    input.patient <- data.frame(
      age=input$age,
      sex=ifelse(input$sex=="Male", 1, 0),
      htn_dci=ifelse(input$htn=="No", 0, 1),
      htn_3meds_dci_30d=ifelse(input$htn_3_meds=="No", 0, 1),
      albumin_baseline=input$albu,
      scr_baseline=input$scr,
      scr_baseline_2=input$scr^2,
      pth_baseline=input$pth,
      plt_baseline=input$plt,
      potassium_baseline=input$pot,
      IDWG_adj=IDWC
    )
    lp.patient <- -2.576755 + sum(bw.model$coefficients * input.patient)
    1 - exp(-mean(h0.shrunk) * exp(shrinkage.factor * lp.patient))
  })

  output$recovery_box <- renderInfoBox({
    infoBox(width=12, color="orange",
            title="Predicted probability of recovery in dialysis-dependent AKI at 90 days",
            value=paste0(sprintf("%.1f", pred.prob()*100), "%"))
  })

  observeEvent(input$calculateButton, { shinyjs::show("results.panel") })
  observeEvent({ c(input$age, input$sex, input$htn, input$htn_3_meds,
                   input$albu, input$scr, input$pth, input$plt,
                   input$pot, input$preweight, input$postweight)}, {
                     shinyjs::hide("results.panel")
                   })
}

shinyApp(ui, server)
