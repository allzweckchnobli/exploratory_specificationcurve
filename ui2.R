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
    tags$div(
      class = "panel panel-primary",
      style = "border: none !important",
      
      tags$div(
        class = "panel-heading",
        style = "margin-bottom: 2px;",
        tags$h4(
          class = "panel-title",
          tags$a(
            "Model Choice and Model Specifications",
            "data-toggle" = "collapse",
            "href" = "#collapseModelSpecs",
            "aria-expanded" = "false"
          )
        )
      ),
      
      tags$div(
        id = "collapseModelSpecs",
        class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
        tags$div(
          class = "panel-body",
          div(
            style = "text-align: center;",
            tags$img(src = "model_config.svg", style = "width: 50%; ")
          ),
          p(HTML("
          The model configurations are based on two LLM architectures: Deepseek R1 (671b model accessed via API), and SBERT. Deepseek R1 was prompted with three types of prompts: Definitions of the mechanisms, examples of TAPs stemming from each mechanism, and prompts consisting of both. All other classifiers were based on SBERT embeddings either derived from CLS pooling, MEAN pooling, or MAX pooling (for details on the pooling strategies and their impacts, see Reimers and Gurevych, <a href = 'https://arxiv.org/abs/1908.10084'>2019</a>). Random Forest classifiers were trained and tested on the embeddings derived from each TAP; Top-5 Similarity were based on a dictionary approach comparing each TAP to a dictionary of tagged samples, and deriving the top-5 most similar samples; Mean-Top-5 + Random Forest averaged the embeddings of the TAP and the five most similar TAPs to enhance shared features, and subsequently trained and evaluated a Random Forest classifier.
          <br>
          Data used to create embeddings for all SBERT based classifiers was either not pre-processed, pre-processed by removing stopwords and lowercasing; normalized by stemming and lemmatizing, or normalized and additionally removing the top 200 shared words.
          "), style = "font-size: 16px;")
        )
      ), 

      tags$div(
        class = "panel-heading",
        style = "margin-bottom: 2px;",
        tags$h4(
          class = "panel-title",
          tags$a(
            "Data Preselection Strategy",
            "data-toggle" = "collapse",
            "href" = "#collapseDataPreselection",
            "aria-expanded" = "false"
          )
        )
      ),
      
      tags$div(
        id = "collapseDataPreselection",
        class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
        tags$div(
          class = "panel-body",
          tags$img(src = "data_preselection_strategy.svg", style = "width: 100%;"),
          p(HTML("
          Data preselection strategy referrs to the participants included in and excluded from the classification. No data preselection (<i>Full Data</i>) indicates that no participants were excluded from analysis. Data preselection strategies referring to <i>Ground Truth different from...</i> indicate that only participants who reported to perceive a measure outside of the 90%-percentile of the baseline group perception (or all other groups respectively) <i>and</i> who were assigned to the condition they perceived to report were included. <i>Self Report different from... </i> conditions are based on the same selection strategy, but the ground-truth condition assigned was not considered.
          "), style = "font-size: 16px;")
        )
      ), 

      tags$div(
        class = "panel-heading",
        style = "margin-bottom: 2px;",
        tags$h4(
          class = "panel-title",
          tags$a(
            "Comparison Type",
            "data-toggle" = "collapse",
            "href" = "#collapseComparison",
            "aria-expanded" = "false"
          )
        )
      ),
      
      tags$div(
        id = "collapseComparison",
        class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
        tags$div(
          class = "panel-body",
          tags$img(src = "comparison_type_long.svg", style = "width: 100%;"),
          p(HTML("
          The three comparison types evaluated are binary comparisons (<i>One vs. One</i>), one-vs-rest-comparisons (<i>One vs. All</i>), and multi-class-comparisons (<i>All vs. All</i>). The probabilities for each class as illustrated in the visualization are the probabilities underlying the AUC-ROC and h-score distribution. For accuracy, the mechanism with highest probability was valued as the positive class.
          "), style = "font-size: 16px;")
        )
      ), 

      tags$div(
        class = "panel-heading",
        style = "margin-bottom: 2px;",
        tags$h4(
          class = "panel-title",
          tags$a(
            "Mechanisms",
            "data-toggle" = "collapse",
            "href" = "#collapseMechanism",
            "aria-expanded" = "false"
          )
        )
      ),
      
      tags$div(
        id = "collapseMechanism",
        class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
        tags$div(
          class = "panel-body",
          p(HTML("
          The four mechanisms evaluated were <i>Baseline Condition</i> manipulating choice attributes; <i>Social Norms</i>, <i>Knowledge</i>, and <i>Need</i>. These mechanisms and their presumed influence on decision-making were not identified in this study, but in previous work (see Lob et al., <a href='https://osf.io/preprints/psyarxiv/a73y4_v1'>2025</a>). These endeavors also resulted in a mechanism-explorer much more elaborate than the shiny-app at hand, which can be found under <a href='https://explorer.cbdr-lab.net/'>https://explorer.cbdr-lab.net/</a>.
          "), style = "font-size: 16px;")
        )
      )
    ),

    h2("Evaluation Metrics"),
    tags$div(
      class = "panel panel-primary",
      style = "border: none !important",
      
      tags$div(
        class = "panel-heading",
        style = "margin-bottom: 2px;",
        tags$h4(
          class = "panel-title",
          tags$a(
            "AUC-ROC",
            "data-toggle" = "collapse",
            "href" = "#collapseAucRoc",
            "aria-expanded" = "false"
          )
        )
      ),
      
      tags$div(
        id = "collapseAucRoc",
        class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
        tags$div(
          class = "panel-body",
          p(HTML("
          The <i>area under the curve</i> for the <i>receiver-operator-characteristic</i> refers to the area under the curve when the true positive and the false positive rate are plotted against each other for various thressholds between 0 and 1. It is a measure fitting to assess both specificity and sensitivity of any given classifier, as both considerations are taken into account. AUC-ROC values of ~0.5 indicate a classifier as good as chance, AUC-ROC values of ~0.7 are generally accepted as adequate, values ~0.8 or higher as excellent, and values ~0.9 or higher as outstanding (see for example Hosmer, & Lemeshow, 2000).
          "), style = "font-size: 16px;")
        )
      ), 

      tags$div(
        class = "panel-heading",
        style = "margin-bottom: 2px;",
        tags$h4(
          class = "panel-title",
          tags$a(
            "h-Scores",
            "data-toggle" = "collapse",
            "href" = "#collapsehScore",
            "aria-expanded" = "false"
          )
        )
      ),
      
      tags$div(
        id = "collapsehScore",
        class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
        tags$div(
          class = "panel-body",
          p(HTML("
          h-Scores are a standardized measure for the typically used measure of <i>entropy<i> (see Tornetta, <a href='http://arxiv.org/abs/2103.15157'>2021</a>), and can assume values between 0 and 1, wherein a value of 0 means that a classifier cannot distinguish between alternatives, whereas a h-score of 1 indicates that a classifier distinguishes between alternatives with 100% confidence. It is important to note that high confidence in classification does not imply high performance of the classifier, although the results in this study display a moderate correlation between h-scores and AUC-ROC"), style = "font-size: 16px;")
        )
      ), 

      tags$div(
        class = "panel-heading",
        style = "margin-bottom: 2px;",
        tags$h4(
          class = "panel-title",
          tags$a(
            "Model Choice and Model Specifications",
            "data-toggle" = "collapse",
            "href" = "#collapseAccuracy",
            "aria-expanded" = "false"
          )
        )
      ),
      
      tags$div(
        id = "collapseAccuracy",
        class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
        tags$div(
          class = "panel-body",
          tags$img(src = "model_config.svg", style = "width: 100%;"),
          p(HTML("a"), style = "font-size: 16px;")
        )
      )
    )
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
