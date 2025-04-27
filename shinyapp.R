library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)

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

ui <- fluidPage(
  titlePanel("Exploratory visualization of Specification Curve"),
  
  sidebarLayout(
    sidebarPanel(
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
      plotOutput("filtered_plot"),
      textOutput("test"),
      downloadButton("download_csv", "Download CSV")
    )
  )
)

server <- function(input, output, session) {
  
    observe({
        selected <- input$filter_type
        ## check if selected currently contains ALL 
        
        if (!is.null(selected)) {
          if ("All" %in% selected && length(selected) > 1) {
            # If "All" selected together with others -> keep only "All"
            updateCheckboxGroupInput(
              session,
              "filter_type",
              selected = "All"
            )
          }
        }
      })
      
  
  filtered_data <- eventReactive(input$submit, {
    req(input$primary_measure)
    req(input$secondary_measure)
    req(input$filter_type)
    req(input$filter_dataverse)
    req(input$filter_classifier)
    req(input$filter_spec_ds)
    req(input$filter_spec_other)
    req(input$filter_preprocessing)
    req(input$filter_mechanism)
    
    ## Filter according to specifications
    filtered <- data
  
    ## Then filter step-by-step
    if (!"All" %in% input$filter_type) {
      filtered <- filtered %>% filter(type %in% input$filter_type)
    }
  
    if (!"All" %in% input$filter_dataverse) {
      filtered <- filtered %>% filter(dataverse %in% input$filter_dataverse)
    }
  
    if (!"All" %in% input$filter_classifier) {
      filtered <- filtered %>% filter(classifier %in% input$filter_classifier)
    }
  
    if (!"All" %in% input$filter_spec_ds) {
      filtered <- filtered %>% filter(specification %in% input$filter_spec_ds)
    }
  
    if (!"All" %in% input$filter_spec_other) {
      filtered <- filtered %>% filter(specification %in% input$filter_spec_other)
    }
  
    if (!"All" %in% input$filter_preprocessing) {
      filtered <- filtered %>% filter(preprocessing %in% input$filter_preprocessing)
    }
  
    if (!"All" %in% input$filter_mechanism) {
      filtered <- filtered %>% filter(tested_class %in% input$filter_mechanism)
    }

    if(input$primary_measure == "AUC-ROC") {
      filtered <- filtered %>% arrange(desc(aucroc))
    } else if(input$primary_measure == "h-Score") {
      filtered <- filtered %>% arrange(desc(ent))
    } else if(input$primary_measure == "Accuracy") {
      filtered <- filtered %>% arrange(desc(accuracy))
    }

    filtered <- filtered %>% mutate(id = row_number())

    measure <- helper_measures %>% filter(name == input$primary_measure) %>% pull(intname)
    sd_measure <- helper_measures %>% filter(name == input$primary_measure) %>% pull(sdname)
    pub_measure <- input$primary_measure

    ## Secondary measure 
    if(input$secondary_measure != "None") {
      smeasure <- helper_measures %>% filter(name == input$secondary_measure) %>% pull(intname)
      sd_smeasure <- helper_measures %>% filter(name == input$secondary_measure) %>% pull(sdname)
      pub_smeasure <- input$secondary_measure
    } else {
      smeasure <- NA
      sd_smeasure <- NA
      pub_smeasure <- NA
    }

    list(
      df = filtered,
      pub_measure = pub_measure,
      measure = measure,
      sd_measure = sd_measure,
      pub_smeasure = pub_smeasure,
      smeasure = smeasure,
      sd_smeasure = sd_smeasure
    )
  })
  
  output$filtered_plot <- renderPlot({
    filtered_data <- filtered_data()
    df <- filtered_data$df
    pub_measure <- filtered_data$pub_measure
    pub_smeasure <- filtered_data$pub_smeasure
    measure <- filtered_data$measure
    sd_measure <- filtered_data$sd_measure
    smeasure <- filtered_data$smeasure
    sd_smeasure <- filtered_data$sd_smeasure
    n_id <- nrow(df)

    p_primary_measure <- ggplot(df, aes(x = id, y = !!sym(measure))) + 
      geom_errorbar(aes(ymin = !!sym(measure) - !!sym(sd_measure), ymax = !!sym(measure) + !!sym(sd_measure), color = "#E7E7E7"), 
                    width = 0.2, alpha = 0.3) +
      geom_point(aes(color = "#0028A5"), size = 1) +
      scale_color_identity() +
      scale_y_continuous(limits = c(0,1)) +
      scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
      geom_hline(yintercept = mean(df[[measure]]), linetype = "dashed", color = "#A27200") +
      annotate("text", x = n_id, y = mean(df[[measure]]) + 0.03, 
               label = paste0("mean: ", round(mean(df[[measure]]), 3)), 
               color = "#A27200", hjust = 1, size = 3) +
      labs(x = NULL,y=pub_measure) + # remove x and y axis 
      ggtitle(paste0("Distribution of ",pub_measure, " across selected configurations")) +
      theme_minimal() +
      theme(plot.margin = margin(0, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
            panel.grid = element_blank(), strip.text = element_blank(),panel.grid.major.y = element_line(color = "#A3A3A3"),axis.title.y = element_text(angle=0,hjust=0,vjust=0.5)) 
    
    if(!is.na(smeasure)){
      p_secondary_measure <- ggplot(df, aes(x = id, y = !!sym(smeasure))) + 
        geom_errorbar(aes(ymin = !!sym(smeasure) - !!sym(sd_smeasure), ymax = !!sym(smeasure) + !!sym(sd_smeasure), color = "#E7E7E7"), 
                      width = 0.2, alpha = 0.3) +
        geom_point(aes(color = "#0028A5"), size = 1) +
        scale_color_identity() +
        scale_y_continuous(limits = c(0,1)) +
        scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
        geom_hline(yintercept = mean(df[[smeasure]]), linetype = "dashed", color = "#A27200") +
        annotate("text", x = n_id, y = mean(df[[smeasure]]) + 0.03, 
                 label = paste0("mean: ", round(mean(df[[smeasure]]), 3)), 
                 color = "#A27200", hjust = 1, size = 3) +
        labs(x = NULL,y=pub_smeasure) + # remove x and y axis 
        ggtitle(paste0("Distribution of ",pub_smeasure, " across selected configurations")) +
        theme_minimal() +
        theme(plot.margin = margin(0, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
              panel.grid = element_blank(), strip.text = element_blank(),panel.grid.major.y = element_line(color = "#A3A3A3"),axis.title.y = element_text(angle=0,hjust=0,vjust=0.5))   
    }

    p_dataverse <- ggplot(df, aes(x = id)) +
        scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
        geom_text(aes(y=dataverse,label= dataverse_full), color="#001E7C",alpha = 0.7) +
        geom_text(aes(y=dataverse,label= dataverse_gtca), color="#001E7C",alpha = 0.7) +
        geom_text(aes(y=dataverse,label= dataverse_gtall),color="#001E7C",alpha = 0.7) +
        geom_text(aes(y=dataverse,label= dataverse_srca), color="#001E7C",alpha = 0.7) +
        geom_text(aes(y=dataverse,label= dataverse_srall),color="#001E7C",alpha = 0.7) +
        labs(x = NULL) +
        ggtitle("Data Preselection Strategy") +
        theme_minimal() +
        theme(plot.title.position = "plot",plot.title = element_text(hjust = 0,size = 12),plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
              panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#001E7C",angle=0,hjust=0,vjust=0.5))
    
    p_classifier <- ggplot(df, aes(x = id)) +
      scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
      geom_text(aes(y=classifier,label= classifier_rf),    color="#147082", alpha = 0.7) + 
      geom_text(aes(y=classifier,label= classifier_rf_rag),color="#147082", alpha = 0.7) + 
      geom_text(aes(y=classifier,label= classifier_ds),    color="#147082", alpha = 0.7) + 
      geom_text(aes(y=classifier,label= classifier_sm),    color="#147082", alpha = 0.7) +
      labs(x = NULL) +
      ggtitle("Model Configuration") + 
      theme_minimal() +
      theme(plot.title.position = "plot",plot.title = element_text(hjust = 0,size = 12),plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
            panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#147082",angle=0,hjust=0,vjust=0.5))
      
    if(sum(df$classifier == "Deepseek R1") > 0) {
        specA <- ggplot(df[df$classifier == "Deepseek R1",], aes(x = id)) +
          scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
          geom_text(aes(y=specification,label= specification_cls), color="#1EA7C4",alpha = 0.7) +
          geom_text(aes(y=specification,label= specification_mean),color="#1EA7C4",alpha = 0.7) + 
          geom_text(aes(y=specification,label= specification_MAX), color="#1EA7C4",alpha = 0.7) + 
          geom_text(aes(y=specification,label= specification_def), color="#1EA7C4",alpha = 0.7) + 
          geom_text(aes(y=specification,label= specification_ex),  color="#1EA7C4",alpha = 0.7) + 
          geom_text(aes(y=specification,label= specification_both),color="#1EA7C4",alpha = 0.7) + 
          labs(x = NULL,y="Specification \nfor Prompts") + # remove x and y axis 
          theme_minimal() +
          theme(plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
                panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#1EA7C4",angle=0,hjust=0,vjust=0.5))
      }
    
    if(sum(df$classifier != "Deepseek R1") > 0) {
      specB <- ggplot(df[df$classifier != "Deepseek R1",], aes(x = id)) +
        scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
        geom_text(aes(y=specification,label= specification_cls), color="#1EA7C4",alpha = 0.7) +
        geom_text(aes(y=specification,label= specification_mean),color="#1EA7C4",alpha = 0.7) + 
        geom_text(aes(y=specification,label= specification_MAX), color="#1EA7C4",alpha = 0.7) + 
        geom_text(aes(y=specification,label= specification_def), color="#1EA7C4",alpha = 0.7) + 
        geom_text(aes(y=specification,label= specification_ex),  color="#1EA7C4",alpha = 0.7) + 
        geom_text(aes(y=specification,label= specification_both),color="#1EA7C4",alpha = 0.7) + 
        labs(x = NULL,y="Specification \nfor Embeddings") + # remove x and y axis 
        theme_minimal() +
        theme(plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
              panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#1EA7C4",angle=0,hjust=0,vjust=0.5))
    }
    if(exists("specA") && exists("specB")) {
      specs <- specA / specB
      n_specs <- 2
    } else if(exists("specA")) {
      specs <- specA
      n_specs <- 1
    } else if(exists("specB")) {
      specs <- specB
      n_specs <- 1
    }

    p_preprocessing <- ggplot(df, aes(x = id)) +
      scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
      geom_text(aes(y=preprocessing,label= preprocessing_none),   color="#C8E485", alpha = 0.7) +
      geom_text(aes(y=preprocessing,label= preprocessing_pre),    color="#C8E485", alpha = 0.7) +
      geom_text(aes(y=preprocessing,label= preprocessing_norm),   color="#C8E485", alpha = 0.7) +
      geom_text(aes(y=preprocessing,label= preprocessing_top200), color="#C8E485", alpha = 0.7) +
      labs(x = NULL) +
      ggtitle("Preprocessing Steps") + 
      theme_minimal() +
      theme(plot.title.position = "plot",plot.title = element_text(hjust = 0, size = 12),plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
            panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#C8E485",angle=0,hjust=0,vjust=0.5))
    
    if(sum(df$type == "One vs. One") > 0 ) {
      onevone <- ggplot(df[df$type == "One vs. One",], aes(x = id)) +
            scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
            geom_text(aes(y=One.vs..One,label= onevone_casi),color="#FFDE8F") +
            geom_text(aes(y=One.vs..One,label= onevone_caen),color="#FFDE8F") +
            geom_text(aes(y=One.vs..One,label= onevone_cagm),color="#FFDE8F") +
            geom_text(aes(y=One.vs..One,label= onevone_ensi),color="#FFDE8F") +
            geom_text(aes(y=One.vs..One,label= onevone_engm),color="#FFDE8F") +
            geom_text(aes(y=One.vs..One,label= onevone_sigm),color="#FFDE8F") +
            labs(x = NULL) + # remove x and y axis 
            ggtitle("Comparison Type") + 
            theme_minimal() +
            theme(plot.title.position = "plot",plot.title = element_text(hjust = 0, size = 12),plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
                  panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#FFDE8F",angle=0,hjust=0,vjust=0.5))
    }

    if(sum(df$type == "One vs. All") > 0 && sum(df$type == "One vs. One") == 0) {
      onevall <- ggplot(df[df$type == "One vs. All",], aes(x = id)) +
        scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
        geom_text(aes(y=One.vs..All,label= onevall_ca),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=One.vs..All,label= onevall_en),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=One.vs..All,label= onevall_si),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=One.vs..All,label= onevall_gm),color="#FFDE8F", alpha = 0.7) +
        labs(x = NULL) + # remove x and y axis 
        ggtitle("Comparison Type") + 
        theme_minimal() +
        theme(plot.title.position = "plot",plot.title = element_text(hjust = 0, size = 12),plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
              panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#FFDE8F",angle=0,hjust=0,vjust=0.5))

    } else if(sum(df$type == "One vs. All") > 0) {
      onevall <- ggplot(df[df$type == "One vs. All",], aes(x = id)) +
        scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
        geom_text(aes(y=One.vs..All,label= onevall_ca),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=One.vs..All,label= onevall_en),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=One.vs..All,label= onevall_si),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=One.vs..All,label= onevall_gm),color="#FFDE8F", alpha = 0.7) +
        labs(x = NULL) + # remove x and y axis 
        theme_minimal() +
        theme(plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
              panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#FFDE8F",angle=0,hjust=0,vjust=0.5))

    }  
    
    if(sum(df$type == "All vs. All") > 0 && sum(df$type == "One vs. One") == 0 && sum(df$type == "One vs. All") == 0) {
      allvall <- ggplot(df[df$type == "All vs. All",], aes(x = id)) +
        scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
        geom_text(aes(y=All.vs..All,label= allvall_ca),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=All.vs..All,label= allvall_en),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=All.vs..All,label= allvall_si),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=All.vs..All,label= allvall_gm),color="#FFDE8F", alpha = 0.7) +
        labs(x = NULL) + # remove x and y axis 
        ggtitle("Comparison Type") + 
        theme_minimal() +
        theme(plot.title.position = "plot",plot.title = element_text(hjust = 0, size = 12),plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
              panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#FFDE8F",angle=0,hjust=0,vjust=0.5))  
    } else if(sum(df$type == "All vs. All") > 0) {
      allvall <- ggplot(df[df$type == "All vs. All",], aes(x = id)) +
        scale_x_continuous(limits = c(1, n_id),expand=c(0.01,0.01)) +
        geom_text(aes(y=All.vs..All,label= allvall_ca),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=All.vs..All,label= allvall_en),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=All.vs..All,label= allvall_si),color="#FFDE8F", alpha = 0.7) +
        geom_text(aes(y=All.vs..All,label= allvall_gm),color="#FFDE8F", alpha = 0.7) +
        labs(x = NULL) + # remove x and y axis 
        theme_minimal() +
        theme(plot.margin = margin(1, 0, 0, 0, "pt"),axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
              panel.grid = element_blank(), strip.text = element_blank(),axis.title.y=element_text(color="#FFDE8F",angle=0,hjust=0,vjust=0.5))  
    }
    
    if(exists("onevone") && exists("onevall") && exists("allvall")) {
      p_comparisons <- onevone / onevall / allvall
      n_comp <- 3
    } else if(exists("onevone") && exists("onevall")) {
      p_comparisons <- onevone / onevall 
      n_comp <- 2
    } else if(exists("onevone") && exists("allvall")) {
      p_comparisons <- onevone / allvall
      n_comp <- 2
    } else if(exists("onevall") && exists("allvall")) {
      p_comparisons <- onevall / allvall
      n_comp <- 2
    } else if(exists("onevone")) {
      p_comparisons <- onevone
      n_comp <- 1
    } else if(exists("onevall")) {
      p_comparisons <- onevall
      n_comp <- 1
    } else if(exists("allvall")) {
      p_comparisons <- allvall
      n_comp <- 1
    }

    if(!is.na(smeasure)) {
      heights <- append(c(2, 1.5, 1, 1), c(n_specs, 1, n_comp))
      plot <- p_primary_measure / p_secondary_measure / p_dataverse / p_classifier / specs / p_preprocessing / p_comparisons + plot_layout(heights = heights)
    }
    else {
      heights <- append(c(2, 1, 1), c(n_specs, 1, n_comp))
      plot <- p_primary_measure / p_dataverse / p_classifier / specs / p_preprocessing / p_comparisons + plot_layout(heights = heights)
    }
    
    plot
  })
  
  output$test <- renderText({
    df <- filtered_data()
    df <- df$df
    paste("Number of specifications visualized:", nrow(df))
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the filtered data within the content function
      filtered_data <- filtered_data()  # Assuming this is a reactive function returning your data
      df <- filtered_data$df
      df <- df %>% select(c(X:sd_ent))
      
      # Save the data to a CSV file
      write.csv(df, file, row.names = FALSE)
    }
  )
}



shinyApp(ui, server)


