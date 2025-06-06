library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)
          
ui <- div(
  style = "padding-bottom: 80px;",
  navbarPage("",
  theme = shinytheme("sandstone"),
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
        p(HTML("Specification Curve for NLP Classifiers | Contact: <a href='mailto:sabourani.stocker@uzh.ch'>Sabou Rani Stocker</a> | <a href = 'https://www.psychology.uzh.ch/en/areas/nec/cogres.html'>CBDR</a> | Source code App: <a href='https://github.com/allzweckchnobli/exploratory_specificationcurve' target='_blank'> <i class='fab fa-github' role='presentation' aria-label='github icon'></i> </a> | <a href = 'imprint.html'>Imprint</a>"))
  ),
  useShinyjs(),

  tabPanel("About this App",
    div(
      class = "container mt-5 mb-5",
      h2("A Specification Curve Analysis of NLP Classifiers Diagnosing Psychological Mechanisms Influencing Decision-Making"),
      p(HTML("This app lets you explore the results from a specification curve (see Simonsohn et al., <a href='https://www.nature.com/articles/s41562-020-0912-z'>2020</a>) evaluating the results of natural language processing algorithms diagnosing state-specific mechanisms influencing decisions under risk and uncertainty. Our study investigated the effectiveness of Natural Language Processing (NLP) techniques in classifying verbal reports based on underlying psychological mechanisms influencing decision-making under risk and uncertainty using AUC-ROC as a performance measure and standardized entropy (see Tornetta, <a href='http://arxiv.org/abs/2103.15157'>2021</a>) to assess classification confidence. Using think-aloud protocols collected from an experimental between-subjects design with 320 participants, we tested four hypothesized mechanisms (derived from the <a href='https://explorer.cbdr-lab.net/'>taxonomy of state-specific psychological mechanisms</a> that may shape people's decisions under risk and uncertainty by Lob et al. <a href='https://osf.io/preprints/psyarxiv/a73y4_v1'>2025</a>) across 585 unique model configurations, evaluating both classification performance and confidence.
      <br><br>
      We identified 78 configurations of various parameters influencing NLP, and tested all configurations across four mechanisms, resulting in 315 specifications evaluated. The classification performance varied across models, averaging at AUC-ROC = 0.63, and a mean standardized entropy of 0.55, indicating limited, yet promising capabilities of NLP in classifying state-specific psychological mechanisms influencing decisions under risk and uncertainty. The high entropy hints at fuzzy boundaries between mechanisms resulting in low classification confidence. Higher confidence was generally associated with higher performance (correlation of 0.36). The findings highlight critical considerations for applying NLP to the classification of verbal reports and offer practical guidance for future implementations in behavioral research.
      <br><br>
      To assess robustness of results for additional configurations, additional parameter specifications were tested, resulting in a total of 585 configurations. All specifications, including those based on additional configurations, can be explored in this app. <br>
      Additional information on the configuration types, specifications, and measures employed can be found in the sections providing <b>more information</b>. 
      <br>
      <br>
      The work displayed was produced in the scope of Sabou Stockers Master's Thesis at the Cognitive and Behavioral Decision Research (<a href='https://www.psychology.uzh.ch/en/areas/nec/cogres.html'>CBDR</a>) lab under the supervision of Olivia Fischer and Prof. Dr. Renato Frey. This study was preregistered on <a href='https://osf.io/t5zbn?view_only=7da9d0d0c336473b9ea08021321aee3e'>OSF</a>. This study was reviewed and approved by the ethics committee of the Faculty of Arts and Social Sciences at the University of Zurich (#24.05.04).
      "),style = "font-size: 20px;"), 
      div(class = "text-center", 
      actionButton("explore_results", "Explore Results",class="btn btn-primary btn-lg"),
      actionButton("more_info", "More Information", class="btn btn-info btn-lg")
      )
    )
  ),
  
  tabPanel("In more Detail",
    div(
      class = "container mt-5 mb-5",
      p(HTML("Learn more about the various parameters manipulated in the configurations of the specification curve analysis (<b>Methodology and Measures</b>) as well as the <b>evaluation metrics</b> used to assess goodness of model performance and confidence in classification."),style="font-size: 20px;"),
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
              tags$img(src = "model_config.svg", style = "width: 50%;  background-color: white;")
            ),
            p(HTML("
            The model configurations are based on two LLM architectures: Deepseek R1 (671b model accessed via <a href='https://api-docs.deepseek.com/'>Deepseek API</a>), and <a href = 'https://arxiv.org/abs/1908.10084'>SBERT</a>. Deepseek R1 is a reasoning-based decoder model based on reinforcement learning (Deepseek AI, <a href='10.48550/arXiv.2501.12948'>2025</a>). SBERT (short for sentence-BERT), introduced by Reimers & Gurevych (<a href='https://arxiv.org/abs/1908.10084'>2019</a>), produces sentence embeddings based on BERT, and is a widely used Large Language Model in research. This study compares three model choices: one based on Deepseek R1 with three prompts, and the other three based on various applications of SBERT.
            <br>
            Deepseek R1 was prompted with three types of prompts: Definitions of the mechanisms, examples of TAPs stemming from each mechanism, and prompts consisting of both. 
            <br>
            All other classifiers were based on SBERT embeddings either derived from CLS pooling, MEAN pooling, or MAX pooling (see Reimers and Gurevych, <a href = 'https://arxiv.org/abs/1908.10084'>2019</a>). <b>For the main 78 configurations presented in this study, only the default pooling, MEAN pooling, was compared and reported</b>. How alternative pooling strategies effect results can be explored in the app.
            <br>
            The first strategy using SBERT embeddings combined these embeddings with Random Forest classification. The second approach leveraged cosine-similarities with a dictionary of examples for each mechanism. The last strategy combined the first and second by averaging the most similar embeddings with the target embedding, and then training and evlauating a random forest on the resulting average embeddings.
            <br>
            Data used to create embeddings for all SBERT based classifiers was either not pre-processed, pre-processed by removing stopwords and lowercasing; normalized by stemming and lemmatizing, or normalized and additionally removing the top 200 shared words. Deepseek R1 was only prompted with unprocessed data. 
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
            tags$img(src = "data_preselection_strategy.svg", style = "width: 100%;  background-color: white;"),
            p(HTML("
            Several ways of including and excluding participants from the analysis based on their subjective perception were compared against each other: <br>
            No data preselection (<i>Full Data</i>) indicates that no participants were excluded from analysis. Data preselection strategies referring to <i>Ground Truth different from...</i> indicate that only participants who reported to perceive a measure outside of the 90%-percentile of the baseline group perception (or all other groups respectively) <i>and</i> who were assigned to the condition they perceived to report were included. <i>Self Report different from... </i> conditions are based on the same selection strategy, but the ground-truth condition assigned was not considered. <br>
            <b> Only comparisons of the ground truth and self-reports against the baseline were reported in the main findings of this study</b>. Comparisons against <i>all other</i> mechanisms (including baseline) were analyzed in the supplementary specifications.
            <br><br>
            The various data preselection strategies relate to varying occurrences in which TAP as the basis of diagnosing psychological mechanisms using NLP might be used: Including full datasets, regardless of subjective perception of participants, most closely resembles experimental setups. Including only TAP from participants reporting to perceive a condition aligns more closely with ecological momentary assessment, as these assessments are generally recorded in instances when participants report to perceive a phenomenon of interest.
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
            tags$img(src = "comparison_type_long.svg", style = "width: 100%;  background-color: white;"),
            p(HTML("
            The three comparison types evaluated are binary comparisons (<i>One vs. One</i>), one-vs-rest-comparisons (<i>One vs. All</i>), and multi-class-comparisons (<i>All vs. All</i>). For the research questions proposed in this study, binary classification and one-vs-rest comparisons are most relevant. Comparing the presence of a mechanism against a baseline (binary) most closely relates to setups in labs, wheras one-vs-rest classification resembles real-world-scenarios, where the mechanism of interest ('one') rarely occurs in isolation, but alongside many other influences ('other'). <b>Therefore, only the two comparison types <i>One vs. One</i> and <i>One vs. Rest</i> were included in the main 78 configurations</b>.
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
            The four mechanisms evaluated were <i>Baseline Condition</i> manipulating choice attributes; <i>Social Norms</i>, <i>Knowledge</i>, and <i>Need</i>. These mechanisms and their presumed influence on decision-making were not identified in this study, but in previous work (see Lob et al., <a href='https://osf.io/preprints/psyarxiv/a73y4_v1'>2025</a>). These endeavors also resulted in a <i>mech-explorer</i> much more elaborate than the web-app at hand, which can be found as part of their online <a href='https://explorer.cbdr-lab.net/'>taxonomy of state-specific psychological mechanisms</a>. For more detailed descriptions of the mechanisms and their suggested influence on decision-making under risk and uncertainty, it is strongly recommended to consult their interactive and explorative taxonomy.
            "), style = "font-size: 16px;")
          )
        ), 

        tags$div(
          class = "panel-heading",
          style = "margin-bottom: 2px;",
          tags$h4(
            class = "panel-title",
            tags$a(
              "Open Science and Open Data Practices",
              "data-toggle" = "collapse",
              "href" = "#collapseOS",
              "aria-expanded" = "false"
            )
          )
        ),
        
        tags$div(
          id = "collapseOS",
          class = "panel-collapse collapse",  # start collapsed; add "in" to show by default
          tags$div(
            class = "panel-body",
            p(HTML("
            This study is commited to transparently report and provide the data collected, the analysis conducted, and the results obtained.
            <br><br>
            All participants of this experiment were given the option to either consent to only participate in the study presented in this app, or also provide their data for subsequent analyses. Out of 320 participants, 290 participants agreed to allow their data to be used in further research. This data can be obtained upon request.
            <br>
            <br>
            The experimental setup for this study relied on free and open-source software being programmed in PHP, with an SQL database for data management. The code for the experimental setup is available upon request.
            <br>
            <br>
            The analysis pipeline was based solely on open-weights model, and written in Python and R, ensuring replicability of the results presented in this study. The full code of the analysis pipeline is available upon request.
            <br>
            <br>
            Please use the contact in the footer of this web-app for further information and inquiries.
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
            h-Scores are a standardized measure for the typically used measure of <i>entropy</i> (see Tornetta, <a href='http://arxiv.org/abs/2103.15157'>2021</a>), and can assume values between 0 and 1, wherein a value of 0 means that a classifier cannot distinguish between alternatives, whereas a h-score of 1 indicates that a classifier distinguishes between alternatives with 100% confidence. It is important to note that high confidence in classification does not imply high performance of the classifier, although the results in this study display a moderate correlation between h-scores and AUC-ROC"), style = "font-size: 16px;")
          )
        ), 

        tags$div(
          class = "panel-heading",
          style = "margin-bottom: 2px;",
          tags$h4(
            class = "panel-title",
            tags$a(
              "Accuracy",
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
            p(HTML("
            While Accuracy is not a primary measure reported in this study due to its risk of being over-optimistic in unbiased datasets, it is a frequently used measure in machine learning, and therefore a filter option in this explorer. Accuracy is the percentage of correctly identified instances by all instances, or in other words the sum of true negatives and true positives divided by the sum of true negatives, true positives, false negatives and false positives of a class.
            "), style = "font-size: 16px;")
          )
        )
      ),
      div(class = "text-center", 
      actionButton("explore_results_2", "Explore Results",class="btn btn-primary btn-lg"),
      )
    )

  ),

  tabPanel("Explore",
    div(class = "container mt-5 mb-5",
      h2("Explore"),
      fluidRow(
        column(12,
        div(
          class = "alert alert-info",
          p(HTML("Select the specifications you are interested in from the configurations in the menu below. The defaults selected represent the main findings reported in this study. <br><br>Once the specification curve is generated, you can download the plot in .svg format as well as the raw measures in .csv format."
        ),style="font-size: 20px;")
        )
      )),
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
                  selected = c("Full Data","GT different baseline","SR different baseline")
                )
          ), 
          column(2, 
              checkboxGroupInput(
                  "filter_classifier",
                  "Select Model:",
                  choices = filter_classifier_options,
                  selected = c("Deepseek R1","Mean Top-5 + Random Forest","Random Forest","Top-5 Similarity")
                ),
                checkboxGroupInput(
                  "filter_spec_ds",
                  "Select Specification for Deepseek R1 based Model(s):",
                  choices = filter_specifications_ds_options,
                  selected = c("Definitions","Examples","Both")
                ),
                checkboxGroupInput(
                  "filter_spec_other",
                  "Select Specification for SBERT based Model(s):",
                  choices = filter_specifications_other_options,
                  selected = "MEAN"
                )
          ), 
          column(2,
              checkboxGroupInput(
                  "filter_preprocessing",
                  "Select Pre-Processing Strategy:",
                  choices = filter_preprocessing_options,
                  selected = c("None","Normalized","Pre-Processed","Top200")
                )
          ),
          column(2, 
              checkboxGroupInput(
                  "filter_type",
                  "Select Filter Type:",
                  choices = filter_type_options,
                  selected = c("One vs. One","One vs. All")
                )                
          ), 
          column(2,
            checkboxGroupInput(
              "filter_mechanism",
              "Select Mechanism analyzed:",
              choices = filter_mechanism_options,
              selected = c("CA","EN","GM","SI")
            )
          )
      ),
      fluidRow(
        column(12,
          div(class = "text-center",
          p("Chose from the selection of specifications in the menu above and press submit to generate a specification curve. The defaults selected represent the main configurations analyzed in this study."),
          p(HTML("<i><b>Important:</b> It may take a couple of seconds for the plot to generate. Please be patient, this shiny app is working hard.</i>")),
          actionButton("submit", "Generate Plot",class="btn btn-success btn-lg")
          )
        )
      ),
      fluidRow(
          column(12,
          h2("Specification Curve"),
          div(
              class = "text-right",
              downloadButton("download_csv", "Download Measures", disabled = TRUE, class = "btn btn-sm btn-primary"),
              downloadButton("download_plot_svg","Download Graph",disabled = TRUE, class = "btn btn-sm btn-primary")
            ),
            plotOutput("filtered_plot",height="600px"),
            textOutput("test")
          )
      )
    )
  )
))
