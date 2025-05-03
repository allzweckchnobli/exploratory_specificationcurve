library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)

ui <- navbarPage("",
  id = "main_tabs",
  # Footer shown on every page
  footer = div(
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
        p("Footer content goes here")
  ),
  useShinyjs(),
  tabPanel("About this App",
    div(
      class = "container mt-5 mb-5",
      h2("A Specification Curve Evaluation of NLP Classifiers for state-specific psychological
      Mechanisms influencing Decisions under Risk and Uncertainty"),
      p(HTML("This small app lets you explore the results from a specification curve (Simonsohn et al., <a href='https://www.nature.com/articles/s41562-020-0912-z'>2020</a>) evaluating 585 unique configurations of natural language processing algorithms diagnosing state-specific mechanisms influencing decisions under risk and uncertainty. It provides insights in the capabilities and constraints of Natural Language Processing as a method in processing verbal protocols as data.
      <br>
      <br>
      Our study investigated the effectiveness of Natural Language Processing (NLP) techniques in classifying verbal reports based on underlying psychological mechanisms influencing decision-making under risk and uncertainty using AUC-ROC as a performance measure and standardized entropy (see Tornetta, <a href='http://arxiv.org/abs/2103.15157'>2021</a>) to assess classification confidence. Using think-aloud protocols collected from an experimental between-subjects design with 320 participants, we tested four hypothesized mechanisms (derived from Lob et al. <a href='https://osf.io/preprints/psyarxiv/a73y4_v1'>2025</a>) across 585 unique model configurations, evaluating both classification performance and confidence. <br>
      Classification performance varied across models, with an average AUC-ROC of 0.65, indicating moderate predictive ability. Performance was most sensitive to the choice of large language model and data preselection strategy, while it remained relatively stable across the four mechanisms examined. Classification confidence was modest across all models (mean standardized entropy = 0.57), though higher confidence was generally associated with better performance. The findings highlight critical considerations for applying NLP to the classification of verbal reports and offer practical guidance for future implementations in behavioral research.
      <br>
      <br>
      The work displayed was produced in the scope of my Master's Thesis at Cognitive and Behavioral Decision Research (<a href='https://www.psychology.uzh.ch/en/areas/nec/cogres.html'>CBDR</a>) under the supervision of Olivia Fischer and Prof. Dr. Renato Frey. 
      "),style = "font-size: 20px;"), 
      div(class = "text-center", 
      actionButton("explore_results", "Explore Results",class="btn btn-primary btn-lg"),
      actionButton("more_info", "More Info", class="btn btn-info btn-lg")
      )
    )
  ),
  tabPanel("In more Detail",
  div(class = "container mt-5 mb-5",
    h2("Methodology and Measures"),
    h3("Model Choice and Model Specifications"), 
    h3("Data Preselection Strategy"),
    h3("Comparison Type"),
    h3("Mechanisms"),
    p(HTML("The mechanisms that inspired the study were identified in Lob et al. (<a href='https://osf.io/preprints/psyarxiv/a73y4_v1'>2025</a>)."),style = "font-size: 20px;"),
    h2("Evaluation Metrics"),
    h3("AUC-ROC"),
    h3("h-Score"),
    h3("Accuracy")
  )
),
  tabPanel("Explore",
    div(class = "container mt-5 mb-5",
      h2("Explore"),
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
                )                
          ), 
          column(2,
            checkboxGroupInput(
              "filter_mechanism",
              "Select Mechanism analyzed:",
              choices = filter_mechanism_options,
              selected = "All"
            )
          )
      ),
      fluidRow(
        column(12,
          div(class = "text-center",
          p("Chose from the selection of specifications in the menu above and press submit to generate a specification curve."),
          actionButton("submit", "Submit Selection")
          )
        )
      ),
      fluidRow(
          column(12,
          h2("Specification Curve Output"),
          div(
              class = "text-right",
              downloadButton("download_csv", "Download Measures", disabled = TRUE, class = "btn btn-sm btn-primary"),
              downloadButton("download_plot_svg","Download Graph",disabled = TRUE, class = "btn btn-sm btn-primary")
            ),
            plotOutput("filtered_plot",height="600px"),
            textOutput("test")
          )
      )
  ),
  )
)
