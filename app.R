library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)
library(shinyjs)

source("global.R")

ui <- fluidPage(
    useShinyjs(),
    titlePanel("A Specification Curve Evaluation of NLP Classifiers for state-specific psychological
Mechanisms influencing Decisions under Risk and Uncertainty"),
    
    fluidRow(
        column(2, 
            radioButtons(
                "primary_measure",
                "Select primary Measure:",
                choices = filter_primary_measure,
                selected = "AUC-ROC"
              ),
              radioButtons(
                "secondary_measure",
                "Select secondary Measure:",
                choices = filter_secondary_measure,
                selected = "None"
              )
        ), 
        column(2,
            checkboxGroupInput(
                "filter_dataverse",
                "Select Data Preselection Strategy:",
                choices = filter_dataverse_options,
                selected = "All"
              )
        ), 
        column(2, 
            checkboxGroupInput(
                "filter_classifier",
                "Select Model:",
                choices = filter_classifier_options,
                selected = "All"
              ),
              checkboxGroupInput(
                "filter_spec_ds",
                "Select Specification for Deepseek R1 based Model(s):",
                choices = filter_specifications_ds_options,
                selected = "All"
              ),
              checkboxGroupInput(
                "filter_spec_other",
                "Select Specification for SBERT based Model(s):",
                choices = filter_specifications_other_options,
                selected = "All"
              )
        ), 
        column(2,
            checkboxGroupInput(
                "filter_preprocessing",
                "Select Pre-Processing Strategy:",
                choices = filter_preprocessing_options,
                selected = "All"
              )
        ),
        column(2, 
            checkboxGroupInput(
                "filter_type",
                "Select Filter Type:",
                choices = filter_type_options,
                selected = "All"
              ),
              checkboxGroupInput(
                "filter_mechanism",
                "Select Mechanism analyzed:",
                choices = filter_mechanism_options,
                selected = "All"
              )
        ),
        column(2,
            actionButton("submit", "Submit Selection")
        )
    ),
    mainPanel(
        fluidRow(
            column(12,
            h2("Specification Curve Plot"),
            div(
                class = "text-right",
                downloadButton("download_csv", "Download Measures", disabled = TRUE, class = "btn btn-sm btn-primary")
              ),
              plotOutput("filtered_plot",height="600px"),
              textOutput("test")
            )
        )
    ),
    tags$footer(
        style = "
          position: fixed;
          bottom: 0;
          width: 100%;
          background-color: #f8f9fa;
          text-align: center;
          padding: 10px;
          font-size: 12px;
          color: #6c757d;
        ",
        "[Licence] Sabou Rani Stocker - Source Code available on GitHub."
      ),
    
      tags$style(HTML("
        .container-fluid {
          padding-bottom: 60px; 
        }
      "))
)

source("server.R")

shinyApp(ui = ui, server = server)
