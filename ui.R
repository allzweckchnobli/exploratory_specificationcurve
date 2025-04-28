library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)

# Example data (you can replace this with your CSV)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  type = sample(c("All", "All vs. All", "One vs. All", "One vs. One"), 100, replace = TRUE)
)

data <- read.csv("plotdata_final.csv")

filter_primary_measure <- c("AUC-ROC","h-Score","Accuracy")
filter_secondary_measure <- c("None","AUC-ROC","h-Score","Accuracy")

filter_type_options <- c("All", "All vs. All", "One vs. All", "One vs. One")
filter_dataverse_options <- c("All","Full Data","GT different all","GT different baseline","SR different all","SR different baseline")
filter_classifier_options <- c("All","Deepseek R1","Mean Top-5 + Random Forest","Random Forest","Top-5 Similarity")
filter_specifications_ds_options <- c("All","Definitions","Examples","Both")
filter_specifications_other_options <- c("All","CLS","MEAN","MAX")
filter_preprocessing_options <- c("All","None","Normalized","Pre-Processed","Top200")
filter_mechanism_options <- c(
  "All" = "All",
  "Baseline" = "CA",
  "Knowledge" = "EN",
  "Need" = "GM",
  "Social Norms" = "SI")

## Helper-Tables
### measures and SD 
helper_measures <- data.frame(
  name = c("AUC-ROC","h-Score","Accuracy"),
  intname = c("aucroc","ent","accuracy"),
  sdname = c("sd_aucroc","sd_ent","sd_accuracy")
)

fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Exploratory visualization of Specification Curve"),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #e9ecf5;",
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
      ),

      checkboxGroupInput(
        "filter_type",
        "Select Filter Type:",
        choices = filter_type_options,
        selected = "All"
      ),
      checkboxGroupInput(
        "filter_dataverse",
        "Select Data Preselection Strategy:",
        choices = filter_dataverse_options,
        selected = "All"
      ),
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
      ),
      checkboxGroupInput(
        "filter_preprocessing",
        "Select Pre-Processing Strategy:",
        choices = filter_preprocessing_options,
        selected = "All"
      ),
      checkboxGroupInput(
        "filter_mechanism",
        "Select Mechanism analyzed:",
        choices = filter_mechanism_options,
        selected = "All"
      ),
      actionButton("submit", "Submit Selection")
    ),
    
    mainPanel(
      plotOutput("filtered_plot",height="600px"),
      textOutput("test"),
      downloadButton("download_csv", "Download CSV")
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
      "© 2025 My Company. All rights reserved."
    )
  )
)