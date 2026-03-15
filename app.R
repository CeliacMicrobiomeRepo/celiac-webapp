# This is the R Shiny web application used to view the Celiac Microbiome Repository


# Define color constants
COLORS <- list(
  PRIMARY = "#d9ad6c",       # Main theme color
  SECONDARY = "#7eb4de",     # Secondary color (used for non-celiac)
  BACKGROUND = "#cccccc",    # Background color for maps
  TEXT = "white"             # Text color
)

# Plotting constants
PLOT_BASE_SIZE <- 22         # Base font size for plots
ANNOTATE_SIZE <- 11          # Size for ggplot2 annotate text

# Load necessary libraries for the application
library(markdown)        # For rendering Markdown content
library(shiny)           # For building the Shiny web application
library(DT)              # For rendering interactive data tables
library(ggplot2)         # For creating plots and visualizations
library(rworldmap)       # For mapping geographical data
library(rworldxtra)      # Additional world map data
library(dplyr)           # For data manipulation
library(sf)              # For handling spatial data
library(rnaturalearth)   # For natural earth map data
library(rnaturalearthdata) # For additional natural earth data
library(countrycode)     # For converting country names to codes
library(grid)            # For grid graphics
library(gridExtra)       # For arranging multiple grid graphics
library(reshape2)        # For reshaping data
library(jsonlite)        # For reading version info
library(scales)          # For formatting axis labels
library(plotly)          # For interactive plots with hover

# Load version and metadata from JSON
VERSION_INFO <- list(
  number = "",
  lit_search_date = ""
)

if (file.exists("repo_data/latest_version.json")) {
  try({
    json_data <- jsonlite::fromJSON("repo_data/latest_version.json")
    VERSION_INFO$number <- sprintf("%.1f", as.numeric(json_data$latest_version_number))
    VERSION_INFO$lit_search_date <- json_data$latest_lit_search_date
  }, silent = TRUE)
}

# Define default columns to display for datasets and samples
# ALL: Dataset_ID	Bioproject_ID	Record_Link	Publication_Title	Publication_Link	Month_Of_Publication	DOI	Used_In_Previous_Meta_Analysis	Lit_Search_Source	Data_Source	Sequencing_Type	Sequencing_Technology	Prospective_Study	Sample_Sites	Amplicon_Region	Forward_Primer	Reverse_Primer	DNA_Extraction_Kit	Read_Pairing	Trimming_Of_Reads_After_Acquisition	Bowtie2_Alignment_Sensitivity	Host_Genome_Index	MetaPhlAn_Database	Fw_Read_Trim_Position	Rv_Read_Trim_Position	ASV_Table_Length_Filter	Notes_From_Processing	Age_Range	Num_Samples	Num_Individuals	Num_Celiac_Samples	Num_GFD_Samples	Num_Prospective_Celiac_Samples	Longitudinal_Study	Country	Samples_With_Significant_Factors	Prospective_Studies	Shotgun_Studies	Study_Design_Description	Multiple_Publications
DEFAULT_DATA_SET_COLUMNS <- c(
  "Dataset_ID", "Sequencing_Type", "Amplicon_Region", "Sample_Sites",
  "Country", "Num_Samples", "Num_Celiac_Samples", 
  "Num_Prospective_Celiac_Samples", "Study_Design_Description"
)
# ALL: SRA_Run_ID	Sample_ID	Dataset_ID	SRA_Project_ID	Month_of_Publication	Publication_DOI	Sequencing_Type	Amplicon_Region	Num_Reads_Nonchim	Percent_Host_Reads_Removed	Total_Pairs_Pre_Host_Removal	Seq_Tech	DNA_Ext_Kit	Paired_Reads	Sample_Site	Diagnosed_Celiac	Gluten_Free_Diet	Will_Develop_Celiac	Group	Short_term_Gluten_Challenge	NCGS	Other_Autoimmune	Hookworm	Possible_Celiac	Any_Significant_Factor	Country	Age	Sex
DEFAULT_SAMPLE_COLUMNS <- c(
  "Sample_ID", "Dataset_ID", "Sequencing_Type", "Amplicon_Region", 
  "Seq_Tech", "Sample_Site", "Group", "Country"
  
)



# UI definition for the Shiny application
ui <- fluidPage(
  
  # Import the Ubuntu font from Google Fonts
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Ubuntu:wght@400;700&display=swap", 
      rel = "stylesheet"
    ),
    # Include custom JavaScript for additional functionality
    tags$script(src = "code.js"),
    # Include external CSS file for styling
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Link to the favicon
    tags$link(rel = "icon", type = "image/png", href = "favicon.png")
  ),
  
  # Create the main navigation bar for the app's interface
  navbarPage(
    title = sprintf("Celiac Microbiome Repo (v%s)", VERSION_INFO$number),  # Title of the web app
    windowTitle = "Celiac Repository",  # Title that appears in the browser's UI
    position = "fixed-top",              # Set the navbar to fixed-top

    # Define each tab in the navigation bar
    tabPanel("About",
             # Div containing all contents, styled by help-tab-content
             tags$div(
               class = c("about-tab-content"),
               style = "max-width: 750px; margin: 0 auto;",
               # Render Markdown content for the About tab with dynamic version/date
               HTML(markdown::markdownToHTML(
                 text = {
                   md_content <- paste(readLines("www/tab_about.md", warn = FALSE), collapse = "\n")
                   md_content <- gsub("{{VERSION}}", VERSION_INFO$number, md_content, fixed = TRUE)
                   md_content <- gsub("{{DATE}}", VERSION_INFO$lit_search_date, md_content, fixed = TRUE)
                   md_content
                 },
                 fragment.only = TRUE
               ))
             )
    ), 

    # tabPanel("Publication",
    #          # Div containing all contents, styled by help-tab-content
    #          tags$div(
    #            class = c("about-tab-content"),
    #            style = "max-width: 750px; margin: 0 auto;",
    #            # Include Markdown content for the Publication tab
    #            includeMarkdown("www/tab_publication.md")
    #          )
    # ), 
    
    tabPanel("Datasets",
             sidebarLayout(
               sidebarPanel(
                 # Filter options for dataset columns
                 h4("Filter Columns"),
                 checkboxGroupInput(
                   "selected_columns", 
                   "Select Columns to Display:", 
                   choices = NULL,  # Choices will be updated based on the dataset
                   selected = DEFAULT_DATA_SET_COLUMNS  # Default selected columns
                 ),
                 actionButton("show_all_columns", "Show all columns", class = "btn-xs"),
                 width = 3,
                 class = "custom-sidebar"  # Custom styling for the sidebar
               ),
               mainPanel(
                 # Display the table for included datasets
                 h3("Datasets"),
                 downloadButton("download_datasets_table", "Download Filtered Table"),
                 DT::dataTableOutput("datasets_table"),  # Output for datasets table
                 width = 9,
                 class = "custom-main"  # Custom styling for the main panel
               )
             )
    ),
    
    tabPanel("Samples",
             sidebarLayout(
               sidebarPanel(
                 # Dynamic summary of filtered data
                 h4("Summary of Samples"),
                 uiOutput("filtered_samples_summary"),  # Output for filtered summary
                 br(),
                 
                # Filter options for samples
                h4("Filter Samples"),
                uiOutput("sample_site_filter"),   # UI for sample site filter
                uiOutput("group_filter"),         # UI for group filter
                uiOutput("dataset_filter"),       # UI for dataset filter
                uiOutput("seq_type_filter"),      # UI for sequencing type filter
                uiOutput("amplicon_region_filter"),  # UI for amplicon region filter
                uiOutput("seq_tech_filter"),      # UI for sequencing technology filter
                uiOutput("any_sig_factor_filter"),  # UI for any significant factor filter
                 br(),
                 
                 # Filter options for sample columns
                 h4("Filter Columns"),
                 checkboxGroupInput(
                   "selected_sample_columns", 
                   "Select Columns to Display:", 
                   choices = NULL,  # Choices will be updated based on the samples data
                   selected = DEFAULT_SAMPLE_COLUMNS  # Default selected columns
                 ),
                 actionButton("show_all_sample_columns", "Show all columns", class = "btn-xs"),
                 width = 3,
                 class = "custom-sidebar"  # Custom styling for the sidebar
               ),
               mainPanel(
                 h3("Samples"),
                 downloadButton("download_samples_table", "Download Filtered Table"),
                 DT::dataTableOutput("samples_table"),  # Output for samples table
                 br(),
                 hr(),

                 # Samples Tab Plots After Sampling Filtering
                 uiOutput("samples_plot_grid"),
                 width = 9,
                 class = "custom-main"  # Custom styling for the main panel
               )
             )
    ),
    
    tabPanel("Plots",
             fluidRow(
               column(
                 width = 12,
                 class = "custom-main",
                 h3("Plots"),
                 uiOutput("plot_grid")
               )
             )
    ),

    tabPanel("Metadata",
             tags$div(
               class = c("about-tab-content"),
               style = "max-width: 750px; margin: 0 auto;",
               HTML(markdown::markdownToHTML(
                 text = {
                   md_content <- paste(readLines("www/tab_metadata.md", warn = FALSE), collapse = "\n")
                   md_content <- gsub("{{VERSION}}", VERSION_INFO$number, md_content, fixed = TRUE)
                   md_content
                 },
                 fragment.only = TRUE
               ))
             )
    )
  )
)

# Server logic for the Shiny application
server <- function(input, output, session) {
  
  # Helpers
  read_tsv_robust <- function(path) {
    if (requireNamespace("readr", quietly = TRUE)) {
      readr::read_tsv(path, show_col_types = FALSE, progress = FALSE, guess_max = 100000)
    } else {
      read.delim(
        path,
        header = TRUE,
        sep = "\t",
        quote = "",
        fill = TRUE,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        comment.char = ""
      )
    }
  }
  
  to_logical <- function(x) {
    if (is.logical(x)) return(x)
    x_chr <- tolower(as.character(x))
    y <- ifelse(
      x_chr %in% c("true", "t", "1", "yes", "y"), TRUE,
      ifelse(x_chr %in% c("false", "f", "0", "no", "n"), FALSE, NA)
    )
    as.logical(y)
  }
  
  # Parse "Mon-YY" to a sortable key (YYYYMM) for stable ordering in DT tables.
  month_sort_key <- function(x) {
    x_chr <- trimws(as.character(x))
    x_chr[x_chr == ""] <- NA
    month_map <- c(
      jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6,
      jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12
    )
    pieces <- strsplit(x_chr, "-", fixed = TRUE)
    month_part <- vapply(pieces, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
    year_part <- vapply(pieces, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
    month_num <- month_map[tolower(substr(month_part, 1, 3))]
    year_num <- suppressWarnings(as.integer(year_part))
    year_num <- ifelse(!is.na(year_num) & year_num < 100, 2000 + year_num, year_num)
    sort_key <- year_num * 100 + month_num
    sort_key[is.na(month_num) | is.na(year_num)] <- NA
    sort_key
  }
  
  included_datasets <- NULL  # Initialize variable for datasets
  included_samples <- NULL    # Initialize variable for samples
  world_map_sf <- NULL        # Cache for Natural Earth world map
  
  # Load the datasets file if it exists
  if (file.exists("repo_data/included_datasets.tsv")) {
    included_datasets <- read_tsv_robust("repo_data/included_datasets.tsv")
    
    
    # Update the column choices for datasets dynamically
    updateCheckboxGroupInput(session, "selected_columns",
                             choices = colnames(included_datasets),
                             selected = DEFAULT_DATA_SET_COLUMNS)
  }
  
  # Load the samples file if it exists
  if (file.exists("repo_data/all_samples.tsv")) {
    included_samples <- read_tsv_robust("repo_data/all_samples.tsv")
    
    # Best-effort coercion of logical-like columns
    if ("Diagnosed_Celiac" %in% names(included_samples)) {
      included_samples$Diagnosed_Celiac <- to_logical(included_samples$Diagnosed_Celiac)
    }
    if ("Gluten_Free_Diet" %in% names(included_samples)) {
      included_samples$Gluten_Free_Diet <- to_logical(included_samples$Gluten_Free_Diet)
    }
    if ("Any_Significant_Factor" %in% names(included_samples)) {
      included_samples$Any_Significant_Factor <- to_logical(included_samples$Any_Significant_Factor)
    }
    
    # Update the column choices for samples dynamically
    updateCheckboxGroupInput(session, "selected_sample_columns",
                             choices = colnames(included_samples),
                             selected = DEFAULT_SAMPLE_COLUMNS)
    
    # Render the Sequencing Type filter UI
    output$seq_type_filter <- renderUI({
      selectInput("seq_type_filter_input",
                  "Sequencing Type:",
                  choices = c("Shotgun" = "SG", "16S" = "16S"),
                  selected = NULL,
                  multiple = TRUE)
    })

    # Render the Sample Site filter UI
    output$sample_site_filter <- renderUI({
      sites <- unique(included_samples$Sample_Site)
      # Create named vector for choices: labels are capitalized, values remain original
      site_choices <- setNames(sites, stringr::str_to_title(sites))
      
      selectInput("sample_site_filter_input",
                  "Sample Site:",
                  choices = site_choices,  # Unique capitalized sample sites
                  selected = NULL,
                  multiple = TRUE)  # Allow multiple selections
    })
    
    # Render the Group filter UI
    output$group_filter <- renderUI({
      # Define preferred order and labels for groups
      group_choices <- c("Healthy Control (HC)" = "HC",
                         "Active Celiac Disease (ACD)" = "ACD",
                         "Treated Celiac Disease (TCD)" = "TCD",
                         "Treated Healthy Control (HC_GFD)" = "HC_GFD",
                         "Prospective Celiac Disease (PCD)" = "PCD",
                         "Prospective Healthy Control (PHC)" = "PHC")
      
      # Filter to only include groups present in the data
      available_groups <- unique(included_samples$Group)
      group_choices <- group_choices[group_choices %in% available_groups]
      
      selectInput("group_filter_input",
                  "Analysis Group:",
                  choices = group_choices,
                  selected = NULL,
                  multiple = TRUE)  # Allow multiple selections
    })
    
    # Render the Dataset filter UI
    output$dataset_filter <- renderUI({
      selectInput("dataset_filter_input",
                  "Dataset:",
                  choices = unique(included_samples$Dataset_ID),  # Unique dataset IDs
                  selected = NULL,
                  multiple = TRUE)  # Allow multiple selections
    })

    # Render the Amplicon Region filter UI
    output$amplicon_region_filter <- renderUI({
      if (!"Amplicon_Region" %in% names(included_samples)) return(NULL)
      regions <- unique(included_samples$Amplicon_Region)
      regions <- regions[!is.na(regions) & regions != ""]

      selectInput("amplicon_region_filter_input",
                  "Amplicon Region:",
                  choices = regions,
                  selected = NULL,
                  multiple = TRUE)
    })

    # Render the Sequencing Technology filter UI
    output$seq_tech_filter <- renderUI({
      if (!"Seq_Tech" %in% names(included_samples)) return(NULL)
      techs <- unique(included_samples$Seq_Tech)
      techs <- techs[!is.na(techs) & techs != ""]

      selectInput("seq_tech_filter_input",
                  "Sequencing Technology:",
                  choices = techs,
                  selected = NULL,
                  multiple = TRUE)
    })

    # Render the Any Significant Factor filter UI
    output$any_sig_factor_filter <- renderUI({
      if (!"Any_Significant_Factor" %in% names(included_samples)) return(NULL)
      sig_values <- unique(included_samples$Any_Significant_Factor)
      sig_values <- sig_values[!is.na(sig_values)]

      if (length(sig_values) == 0) return(NULL)
      choices <- c("Yes" = TRUE, "No" = FALSE)
      choices <- choices[choices %in% sig_values]

      selectInput("any_sig_factor_filter_input",
                  "Any Significant Factor:",
                  choices = choices,
                  selected = NULL,
                  multiple = TRUE)
    })
  }

  # Observers for "Show all columns" buttons
  observeEvent(input$show_all_columns, {
    if (!is.null(included_datasets)) {
      updateCheckboxGroupInput(session, "selected_columns",
                               selected = colnames(included_datasets))
    }
  })
  
  observeEvent(input$show_all_sample_columns, {
    if (!is.null(included_samples)) {
      updateCheckboxGroupInput(session, "selected_sample_columns",
                               selected = colnames(included_samples))
    }
  })

  # Cache world map once per session for Plot 3
  world_map_sf <- tryCatch({
    ne_countries(scale = "medium", returnclass = "sf")
  }, error = function(e) NULL)
  
  # Shared reactives for commonly reused filtered datasets
  filtered_samples <- reactive({
    if (is.null(included_samples)) return(NULL)
    data <- included_samples
    if (!is.null(input$seq_type_filter_input) && length(input$seq_type_filter_input) > 0) {
      data <- data[data$Sequencing_Type %in% input$seq_type_filter_input, ]  # Filter by sequencing type
    }
    if (!is.null(input$sample_site_filter_input) && length(input$sample_site_filter_input) > 0) {
      data <- data[data$Sample_Site %in% input$sample_site_filter_input, ]  # Filter by sample site
    }
    if (!is.null(input$group_filter_input) && length(input$group_filter_input) > 0) {
      data <- data[data$Group %in% input$group_filter_input, ]  # Filter by group
    }
    if (!is.null(input$dataset_filter_input) && length(input$dataset_filter_input) > 0) {
      data <- data[data$Dataset_ID %in% input$dataset_filter_input, ]  # Filter by dataset
    }
    if (!is.null(input$amplicon_region_filter_input) && length(input$amplicon_region_filter_input) > 0) {
      data <- data[data$Amplicon_Region %in% input$amplicon_region_filter_input, ]  # Filter by amplicon region
    }
    if (!is.null(input$seq_tech_filter_input) && length(input$seq_tech_filter_input) > 0) {
      data <- data[data$Seq_Tech %in% input$seq_tech_filter_input, ]  # Filter by sequencing technology
    }
    if (!is.null(input$any_sig_factor_filter_input) && length(input$any_sig_factor_filter_input) > 0) {
      data <- data[data$Any_Significant_Factor %in% input$any_sig_factor_filter_input, ]  # Filter by significant factor
    }
    data
  })

  celiac_samples <- reactive({
    if (is.null(included_samples)) return(NULL)
    included_samples[included_samples$Diagnosed_Celiac == TRUE, ]  # Subset to diagnosed celiac
  })

  samples_16s <- reactive({
    if (is.null(included_samples)) return(NULL)
    included_samples[grepl("16S", included_samples$Sequencing_Type, ignore.case = TRUE), ]  # Subset 16S samples
  })

  # Render the datasets table based on selected columns
  output$datasets_table <- DT::renderDataTable({
    if (!is.null(included_datasets) && !is.null(input$selected_columns)) {
      displayed_data <- included_datasets[, input$selected_columns, drop = FALSE]  # Select columns to display
      
      # Make Publication_Link clickable if present
      if ("Publication_Link" %in% colnames(displayed_data)) {
        displayed_data$Publication_Link <- ifelse(
          !is.na(displayed_data$Publication_Link) & displayed_data$Publication_Link != "",
          paste0("<a href='", displayed_data$Publication_Link, "' target='_blank'>", displayed_data$Publication_Link, "</a>"),
          displayed_data$Publication_Link
        )
      }

      # Make Record_Link clickable if present
      if ("Record_Link" %in% colnames(displayed_data)) {
        displayed_data$Record_Link <- ifelse(
          !is.na(displayed_data$Record_Link) & displayed_data$Record_Link != "",
          paste0("<a href='", displayed_data$Record_Link, "' target='_blank'>", displayed_data$Record_Link, "</a>"),
          displayed_data$Record_Link
        )
      }
      column_defs <- list(list(
        targets = "_all",
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if (type === 'display') {",
          "    if (data === null || data === undefined || data === '') {",
          "      return 'NA';",
          "    }",
          "  }",
          "  return data;",
          "}"
        )
      ))
      month_col_name <- "Month_Of_Publication"
      month_col_index <- which(colnames(displayed_data) == month_col_name)
      if (length(month_col_index) == 1) {
        sort_col_name <- paste0(month_col_name, "_SortKey")
        displayed_data[[sort_col_name]] <- month_sort_key(displayed_data[[month_col_name]])
        sort_col_index <- which(colnames(displayed_data) == sort_col_name)
        column_defs <- append(column_defs, list(
          list(targets = sort_col_index - 1, visible = FALSE, searchable = FALSE),
          list(targets = month_col_index - 1, orderData = sort_col_index - 1)
        ))
      }

      DT::datatable(displayed_data,
                    options = list(
                      pageLength = 50,  # Number of rows per page
                      autoWidth = FALSE,
                      scrollX = TRUE,   # Enable horizontal scrolling
                      scrollY = "75vh", # Set vertical scroll height
                      dom = 'tip',      # Display table information and pagination
                      buttons = c('csv', 'excel', 'pdf'),  # Export options
                       deferRender = TRUE,  # Faster initial draw
                       processing = TRUE,    # Show processing indicator
                       columnDefs = column_defs
                    ),
                    class = "display compact nowrap hover",  # Table styling
                    rownames = FALSE,  # Do not display row names
                    escape = FALSE,     # Allow HTML content
                    selection = 'none'  # Disable row selection
      )
    }
  })
  
  # Render the samples table based on selected columns
  output$samples_table <- DT::renderDataTable({
    if (!is.null(included_samples) && !is.null(input$selected_sample_columns)) {
      # Use shared reactive for filtered data
      filtered_data <- filtered_samples()
      
      # Then select the columns to display
      displayed_sample_data <- filtered_data[, input$selected_sample_columns, drop = FALSE]
      
      column_defs <- list(list(
        targets = "_all",
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if (type === 'display') {",
          "    if (data === null || data === undefined || data === '') {",
          "      return 'NA';",
          "    }",
          "  }",
          "  return data;",
          "}"
        )
      ))
      month_col_name <- "Month_of_Publication"
      month_col_index <- which(colnames(displayed_sample_data) == month_col_name)
      if (length(month_col_index) == 1) {
        sort_col_name <- paste0(month_col_name, "_SortKey")
        displayed_sample_data[[sort_col_name]] <- month_sort_key(displayed_sample_data[[month_col_name]])
        sort_col_index <- which(colnames(displayed_sample_data) == sort_col_name)
        column_defs <- append(column_defs, list(
          list(targets = sort_col_index - 1, visible = FALSE, searchable = FALSE),
          list(targets = month_col_index - 1, orderData = sort_col_index - 1)
        ))
      }

      DT::datatable(displayed_sample_data,
                    options = list(
                      pageLength = 150,  # Number of rows per page
                      autoWidth = FALSE,
                      scrollX = TRUE,    # Enable horizontal scrolling
                      scrollY = "75vh",  # Set vertical scroll height
                      dom = 'tip',       # Display table information and pagination
                      buttons = c('csv', 'excel', 'pdf'),  # Export options
                       deferRender = TRUE,  # Faster initial draw
                       processing = TRUE,    # Show processing indicator
                       columnDefs = column_defs
                    ),
                    class = "display compact nowrap hover",  # Table styling
                    rownames = FALSE,  # Do not display row names
                    escape = FALSE,     # Allow HTML content
                    selection = 'none'  # Disable row selection
      )
    }
  })

  # Download handlers for filtered tables
  output$download_datasets_table <- downloadHandler(
    filename = function() {
      "datasets_filtered.csv"
    },
    content = function(file) {
      if (!is.null(included_datasets) && !is.null(input$selected_columns)) {
        data <- included_datasets[, input$selected_columns, drop = FALSE]
        write.csv(data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(), file, row.names = FALSE)
      }
    }
  )
  
  output$download_samples_table <- downloadHandler(
    filename = function() {
      "samples_filtered.csv"
    },
    content = function(file) {
      if (!is.null(included_samples) && !is.null(input$selected_sample_columns)) {
        data <- filtered_samples()[, input$selected_sample_columns, drop = FALSE]
        write.csv(data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(), file, row.names = FALSE)
      }
    }
  )
  
  # Render summary for filtered samples
  output$filtered_samples_summary <- renderUI({
    if (!is.null(included_samples)) {
      # Use shared reactive for filtered data
      filtered_data <- filtered_samples()
      
      # Check the number of samples
      num_samples <- nrow(filtered_data)
      
      if (num_samples == 0) {
        HTML("<p>There are currently no samples that pass these filters.</p>")  # No samples found
      } else {
        num_datasets <- length(unique(filtered_data$Dataset_ID))  # Count unique datasets
        
        # Calculate counts for specific groups
        acd_count <- sum(filtered_data$Group == "ACD", na.rm = TRUE)
        tcd_count <- sum(filtered_data$Group == "TCD", na.rm = TRUE)
        pcd_count <- sum(filtered_data$Group == "PCD", na.rm = TRUE)
        
        # Format numbers with commas
        num_samples_comma <- format(num_samples, big.mark = ",")
        
        datasets_txt <- if (num_datasets == 1) "dataset." else "datasets."
        samples_txt <- if (num_samples == 1) "sample from" else "samples from"
        
        # Build the summary text
        summary_text <- paste(
          "<p>Displayed are", num_samples_comma, samples_txt, num_datasets, datasets_txt,
          paste0("There are ", format(acd_count, big.mark = ","), " ACD samples, ", 
                 format(tcd_count, big.mark = ","), " TCD samples and ", 
                 format(pcd_count, big.mark = ","), " PCD samples.")
        )
        
        HTML(summary_text)  # Render the summary text as HTML
      }
    }
  })
  
  # PLOTS ---------
  
  # List of plot titles for the dynamic plot grid
  plot_titles <- c(
    "Number of Publicly Available Gut Microbiome Samples of Celiac Patients Over The Years",
    "Number of Celiac Samples by Body Site",
    "All Samples by Geography",
    "Amplicon Region of All 16S rRNA Samples",
    "Number of Samples in Each Group",
    "Sample Group Distributions for Prospective Studies"
  )
  
  # Number of columns per row in the plot grid
  columns_per_row <- 2
  
  # Render the dynamic plot grid
  output$plot_grid <- renderUI({
    num_plots <- length(plot_titles)  # Total number of plots
    
    # Generate the UI for each plot with a title and button aligned horizontally
    plot_output_list <- lapply(1:num_plots, function(i) {
      plotname <- paste0("plot", i)  # Unique name for the plot output
      downloadname <- paste0("download_data_plot", i)  # Unique name for the download button
      
      # Use a column with flexbox styling for the title and button
      column(
        width = 12 / columns_per_row,
        style = "margin-bottom: 30px; padding-left: 30px; padding-right: 30px;",  # Add space between rows
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          h4(plot_titles[i], style = "margin: 0;"),  # Title aligned left
          downloadButton(downloadname, "Download Data")  # Button aligned right
        ),
        plotOutput(plotname)  # Plot output below the title and button
      )
    })
    
    # Arrange the plots into rows with consistent spacing
    rows <- lapply(seq(1, num_plots, by = columns_per_row), function(i) {
      fluidRow(
        style = "margin-bottom: 20px;",  # Add vertical spacing between rows
        plot_output_list[i:min(i + columns_per_row - 1, num_plots)]
      )
    })
    
    do.call(tagList, rows)  # Combine all rows into a single UI element
  })
  
  # Plot 1: Accumulating line plot over the years
  output$plot1 <- renderPlot({
    if (!is.null(included_samples)) {
      # Filter to diagnosed celiac samples
      plot1_data <- celiac_samples()
      
      # Extract 'Month_of_Publication' and remove NAs
      month_pub <- na.omit(plot1_data$Month_of_Publication)
      
      # Extract year suffix and construct full year
      year_suffix <- substr(month_pub, 5, 6)
      year_full <- as.numeric(paste0("20", year_suffix))
      
      shiny::validate(shiny::need(length(year_full) > 0, "No celiac samples with publication dates to plot."))
      
      # Create data frame and count samples per year
      plot1_df <- data.frame(Year = year_full)
      counts_per_year <- as.data.frame(table(plot1_df$Year))  # Count samples per year
      colnames(counts_per_year) <- c("Year", "Count")
      counts_per_year$Year <- as.numeric(as.character(counts_per_year$Year))
      
      # Ensure all years are included
      all_years <- seq(min(counts_per_year$Year), max(counts_per_year$Year))
      counts_per_year <- merge(
        data.frame(Year = all_years), counts_per_year, 
        by = "Year", all.x = TRUE
      )
      counts_per_year$Count[is.na(counts_per_year$Count)] <- 0  # Replace NAs with 0
      
      # Compute cumulative sum
      counts_per_year$CumulativeCount <- cumsum(counts_per_year$Count)
      
      # Plot
      {
        p <- ggplot(counts_per_year, aes(x = Year, y = CumulativeCount)) +
          labs(x = "Year of Publication", y = "Cumulative Number of Samples") +
          theme_minimal()
        if (nrow(counts_per_year) >= 2) {
          p + geom_line(color = COLORS$PRIMARY, size = 1) + geom_point(color = COLORS$PRIMARY, size = 2)
        } else {
          p + geom_point(color = COLORS$PRIMARY, size = 2)
        }
      }
    }
  })
  
  # Download handler for Plot 1
  output$download_data_plot1 <- downloadHandler(
    filename = function() {
      "plot1_data.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Generate the data for Plot 1
      plot1_data <- celiac_samples()
      month_pub <- na.omit(plot1_data$Month_of_Publication)
      year_suffix <- substr(month_pub, 5, 6)
      year_full <- as.numeric(paste0("20", year_suffix))
      counts_per_year <- as.data.frame(table(year_full))  # Count samples per year
      write.csv(counts_per_year, file, row.names = FALSE)  # Write to CSV
    }
  )
  
  # Plot 2: Bar plot of sample sites
  output$plot2 <- renderPlot({
    if (!is.null(included_samples)) {
      # Filter to diagnosed celiac samples
      plot2_data <- celiac_samples()
      
      # Extract 'Sample_Site' and remove NAs
      sample_site <- na.omit(plot2_data$Sample_Site)
      sample_site <- stringr::str_to_title(sample_site)
      
      # Count occurrences robustly and sort
      sample_site_counts <- data.frame(SampleSite = sample_site) %>%
        dplyr::count(SampleSite, name = "Count", sort = TRUE)
      
      shiny::validate(shiny::need(nrow(sample_site_counts) > 0, "No sample-site data to display."))
      
      # Plot
      ggplot(sample_site_counts, aes(x = reorder(SampleSite, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = COLORS$PRIMARY) +  # Bar color
        geom_text(aes(label = Count), vjust = -0.5, size = 3) +  # Add counts at the top of bars
        labs(
          x = "Sample Site",
          y = "Number of Samples"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    }
  })
  
  # Download handler for Plot 2
  output$download_data_plot2 <- downloadHandler(
    filename = function() {
      "plot2_data.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Generate the data for Plot 2
      plot2_data <- celiac_samples()
      sample_site <- na.omit(plot2_data$Sample_Site)
      sample_site <- stringr::str_to_title(sample_site)
      sample_site_counts <- data.frame(SampleSite = sample_site) %>%
        dplyr::count(SampleSite, name = "Count", sort = TRUE)
      write.csv(sample_site_counts, file, row.names = FALSE)  # Write to CSV
    }
  )
  
  # Plot 3: World map with circles proportional to the number of samples per country
  output$plot3 <- renderPlot({
    if (!is.null(included_samples)) {
      # Extract 'Country' and remove NAs
      country_data <- na.omit(included_samples$Country)
      
      # Standardize country names if necessary
      country_data <- as.character(country_data)
      
      # Count occurrences
      country_counts <- as.data.frame(table(country_data))  # Count samples per country
      colnames(country_counts) <- c("Country", "Count")
      
      # Get world map data (cached)
      world_map <- world_map_sf
      shiny::validate(shiny::need(!is.null(world_map), "Map data unavailable."))
      
      # Merge country counts with map data
      country_counts$iso_a3 <- countrycode(
        country_counts$Country,
        "country.name",
        "iso3c",
        custom_match = c(
          "England" = "GBR",
          "Scotland" = "GBR",
          "Wales" = "GBR",
          "Northern Ireland" = "GBR"
        )
      )  # Convert country names to ISO codes with custom matches
      country_counts <- country_counts[!is.na(country_counts$iso_a3), ]  # Remove NAs
      map_data <- left_join(world_map, country_counts, by = c("iso_a3"))  # Join map data with counts
      map_data$Count[is.na(map_data$Count)] <- 0  # Replace NAs with 0
      
      # Plot
      {
        # Compute centroids in a projected CRS for more reliable points
        centroids <- sf::st_centroid(sf::st_transform(map_data, 3857))
        centroids <- sf::st_transform(centroids, 4326)
        coords <- sf::st_coordinates(centroids)
        points_df <- data.frame(lon = coords[, 1], lat = coords[, 2], Count = map_data$Count)
        points_df <- points_df[points_df$Count > 0, ]
        
        ggplot(map_data) +
          geom_sf(aes(geometry = geometry), fill = COLORS$BACKGROUND, color = COLORS$TEXT) +  # Base map
          geom_point(data = points_df, aes(x = lon, y = lat, size = Count), color = COLORS$PRIMARY) +
          scale_size_continuous(range = c(2, 10), guide = FALSE) +  # Size scale for points
          labs(x = "", y = "") +
          theme_minimal() +
          theme(axis.text = element_blank(), axis.ticks = element_blank())  # Remove axis text and ticks
      }
    }
  })
  
  # Download handler for Plot 3
  output$download_data_plot3 <- downloadHandler(
    filename = function() {
      "plot3_data.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Generate the data for Plot 3
      country_data <- na.omit(included_samples$Country)
      country_counts <- as.data.frame(table(country_data))  # Count samples per country
      write.csv(country_counts, file, row.names = FALSE)  # Write to CSV
    }
  )
  
  # Plot 4: Bar plot of amplicon regions of all 16S rRNA samples
  output$plot4 <- renderPlot({
    if (!is.null(included_samples)) {
      # Filter samples with "16S" in Sequencing_Type
      plot4_data <- samples_16s()
      
      # Extract 'Amplicon_Region' and remove NAs
      amplicon_region <- na.omit(plot4_data$Amplicon_Region)
      
      # Count occurrences
      amplicon_counts <- as.data.frame(table(amplicon_region))  # Count samples per amplicon region
      colnames(amplicon_counts) <- c("AmpliconRegion", "Count")
      
      # Plot
      ggplot(amplicon_counts, aes(x = AmpliconRegion, y = Count)) +
        geom_bar(stat = "identity", fill = COLORS$PRIMARY) +  # Bar color
        geom_text(aes(label = Count), vjust = -0.5, size = 3) +  # Add counts at the top of bars
        labs(
          x = "Amplicon Region",
          y = "Number of Samples"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    }
  })
  
  # Download handler for Plot 4
  output$download_data_plot4 <- downloadHandler(
    filename = function() {
      "plot4_data.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Generate the data for Plot 4
      plot4_data <- samples_16s()
      amplicon_region <- na.omit(plot4_data$Amplicon_Region)  # Extract amplicon regions
      amplicon_counts <- as.data.frame(table(amplicon_region))  # Count samples per amplicon region
      colnames(amplicon_counts) <- c("AmpliconRegion", "Count")
      write.csv(amplicon_counts, file, row.names = FALSE)  # Write to CSV
    }
  )
  
  # Plot 5: Table of Number of Samples in Each Group
  output$plot5 <- renderPlot({
    if (!is.null(included_samples)) {
      # Exclude samples where 'Gluten_Free_Diet' or 'Diagnosed_Celiac' are not TRUE or FALSE
      plot5_data <- included_samples[
        included_samples$Gluten_Free_Diet %in% c(TRUE, FALSE) &
          included_samples$Diagnosed_Celiac %in% c(TRUE, FALSE), 
      ]
      
      shiny::validate(shiny::need(nrow(plot5_data) > 0, "No data available for this table with current filters."))
      
      # Create contingency table
      table_data <- table(
        GlutenFreeDiet = ifelse(plot5_data$Gluten_Free_Diet, "Gluten-free", "Non-gluten-free"),
        DiagnosedCeliac = ifelse(plot5_data$Diagnosed_Celiac, "Celiac", "Healthy")
      )
      
      # Convert table to data frame for plotting
      df_table <- as.data.frame.matrix(table_data)  # Convert to data frame
      # Ensure both columns exist even if one level is absent
      if (!"Celiac" %in% colnames(df_table)) df_table$Celiac <- 0L
      if (!"Healthy" %in% colnames(df_table)) df_table$Healthy <- 0L
      df_table$Group <- rownames(df_table)  # Add group names as a column
      df_table <- df_table[, c("Group", "Celiac", "Healthy")]  # Reorder columns
      
      # Plot the table as a grob
      table_grob <- tableGrob(df_table, rows = NULL, theme = ttheme_default(core = list(fg_params = list(cex = 1.2))))  # Create table graphic
      grid::grid.draw(table_grob)  # Draw the table graphic
    }
  })
  
  # Download handler for Plot 5
  output$download_data_plot5 <- downloadHandler(
    filename = function() {
      "plot5_data.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Generate the data for Plot 5
      plot5_data <- included_samples[
        included_samples$Gluten_Free_Diet %in% c(TRUE, FALSE) &
          included_samples$Diagnosed_Celiac %in% c(TRUE, FALSE), 
      ]
      table_data <- table(
        GlutenFreeDiet = ifelse(plot5_data$Gluten_Free_Diet, "Gluten-free", "Non-gluten-free"),
        DiagnosedCeliac = ifelse(plot5_data$Diagnosed_Celiac, "Celiac", "Healthy")
      )
      df_out <- as.data.frame.matrix(table_data)
      if (!"Celiac" %in% colnames(df_out)) df_out$Celiac <- 0L
      if (!"Healthy" %in% colnames(df_out)) df_out$Healthy <- 0L
      write.csv(df_out, file, row.names = TRUE)  # Write to CSV
    }
  )
  
  # Plot 6: Sample Group Distributions for Prospective Studies
  output$plot6 <- renderPlot({
    if (!is.null(included_samples)) {
      # Filter samples where 'Group' is 'PHC' or 'PCD'
      plot6_data <- included_samples[included_samples$Group %in% c("PHC", "PCD"), ]
      
      # Prepare data for plotting
      plot6_counts <- plot6_data %>%
        group_by(Dataset_ID, Group) %>%
        summarise(Count = n(), .groups = "drop") %>%
        ungroup()  # Count samples per dataset and group
      
      # Map 'Group' to readable labels and colors
      plot6_counts$SampleGroup <- ifelse(
        plot6_counts$Group == "PCD", "Celiac", "Non-Celiac"
      )
      
      # Plot
      ggplot(plot6_counts, aes(x = Dataset_ID, y = Count, fill = SampleGroup)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  # Bar plot with dodged positions
        geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +  # Add counts on bars
        labs(
          x = "Dataset ID",
          y = "Number of Samples",
          fill = "Sample Group"
        ) +
        scale_fill_manual(values = c("Celiac" = COLORS$PRIMARY, "Non-Celiac" = COLORS$SECONDARY)) +  # Custom colors for groups
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    }
  })
  
  # Download handler for Plot 6
  output$download_data_plot6 <- downloadHandler(
    filename = function() {
      "plot6_data.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Generate the data for Plot 6
      plot6_data <- included_samples[included_samples$Group %in% c("PHC", "PCD"), ]
      plot6_counts <- plot6_data %>%
        group_by(Dataset_ID, Group) %>%
        summarise(Count = n(), .groups = "drop") %>%
        ungroup()  # Count samples per dataset and group
      write.csv(plot6_counts, file, row.names = FALSE)  # Write to CSV
    }
  )

  # Samples Tab Plots After Sampling Filtering ---------

  # List of plot titles for the samples tab (after filtering)
  samples_plot_titles <- c(
    "All Dataset Sample Sizes (After Filters)",
    "Number of Samples by Amplicon Region (After Filters)",
    "Number of Datasets by Amplicon Region (After Filters)",
    "Median Reads after DADA2 by Dataset (After Filters)",
    "Number of Datasets by Sequencing Technology (After Filters)",
    "Number of Datasets by Body Site (After Filters)",
    "Number of Samples by Body Site (After Filters)",
    "Analysis Group Distribution (After Filters)",
    "Samples with Significant Factors by Dataset (After Filters)"
  )

  # Render the dynamic plot grid for the samples tab
  output$samples_plot_grid <- renderUI({
    num_plots <- length(samples_plot_titles)

    # Generate the UI for each plot using bootstrap classes for responsiveness
    plot_output_list <- lapply(1:num_plots, function(i) {
      plotname <- paste0("samples_filter_plot", i)

      # Use plotlyOutput for plot 9 (Significant Factors) for hover functionality
      plot_ui <- if (i == 9) {
        plotlyOutput(plotname, height = "900px")
      } else {
        plotOutput(plotname, height = "900px")
      }

      # Use column with responsive classes to change column count based on window width
      tags$div(
        class = "col-xs-12",
        style = "margin-bottom: 30px; padding: 15px;",
        tags$div(
          style = "background: #f9f9f9; border: 1px solid #ddd; padding: 15px; border-radius: 4px; height: 100%;",
          h4(samples_plot_titles[i], style = "margin-top: 0;"),
          plot_ui
        )
      )
    })

    # Use a single fluidRow to allow bootstrap to wrap columns naturally
    fluidRow(plot_output_list)
  })

  # Helper function for empty plot message
  empty_plot_message <- function(msg = "No data to display after filtering") {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = msg, size = ANNOTATE_SIZE, color = "grey50") +
      theme_void(base_size = PLOT_BASE_SIZE) +
      theme(panel.background = element_rect(fill = "#f0f0f0", color = NA))
  }

  # Plot 1: Dataset sample sizes bar plot
  output$samples_filter_plot1 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    dataset_counts <- data %>%
      group_by(Dataset_ID) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))
    
    if (nrow(dataset_counts) == 0) {
      return(empty_plot_message())
    }
    
    ggplot(dataset_counts, aes(x = reorder(Dataset_ID, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = COLORS$PRIMARY) +
      geom_text(aes(label = Count), vjust = -0.5, size = ANNOTATE_SIZE * 0.5) +
      labs(x = "Dataset ID", y = "Number of Samples") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })

  # Plot 2: Samples by Amplicon Region (Shotgun first)
  output$samples_filter_plot2 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Create a combined column for sequencing type / amplicon region
    data <- data %>%
      mutate(Seq_Category = ifelse(Sequencing_Type == "SG", "Shotgun", 
                                   ifelse(is.na(Amplicon_Region) | Amplicon_Region == "", "Unknown", Amplicon_Region)))
    
    seq_counts <- data %>%
      group_by(Seq_Category) %>%
      summarise(Count = n(), .groups = "drop")
    
    if (nrow(seq_counts) == 0) {
      return(empty_plot_message())
    }
    
    # Order with Shotgun first, then alphabetically
    seq_counts$Seq_Category <- factor(seq_counts$Seq_Category, 
                                       levels = c("Shotgun", sort(setdiff(unique(seq_counts$Seq_Category), "Shotgun"))))
    
    ggplot(seq_counts, aes(x = Seq_Category, y = Count)) +
      geom_bar(stat = "identity", fill = COLORS$PRIMARY) +
      geom_text(aes(label = Count), vjust = -0.5, size = ANNOTATE_SIZE * 0.5) +
      labs(x = "Amplicon Region / Sequencing Type", y = "Number of Samples") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Plot 3: Datasets by Amplicon Region (Shotgun first)
  output$samples_filter_plot3 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Create a combined column for sequencing type / amplicon region
    data <- data %>%
      mutate(Seq_Category = ifelse(Sequencing_Type == "SG", "Shotgun", 
                                   ifelse(is.na(Amplicon_Region) | Amplicon_Region == "", "Unknown", Amplicon_Region)))
    
    # Count unique datasets per category
    dataset_seq_counts <- data %>%
      distinct(Dataset_ID, Seq_Category) %>%
      group_by(Seq_Category) %>%
      summarise(Count = n(), .groups = "drop")
    
    if (nrow(dataset_seq_counts) == 0) {
      return(empty_plot_message())
    }
    
    # Order with Shotgun first, then alphabetically
    dataset_seq_counts$Seq_Category <- factor(dataset_seq_counts$Seq_Category, 
                                               levels = c("Shotgun", sort(setdiff(unique(dataset_seq_counts$Seq_Category), "Shotgun"))))
    
    ggplot(dataset_seq_counts, aes(x = Seq_Category, y = Count)) +
      geom_bar(stat = "identity", fill = COLORS$PRIMARY) +
      geom_text(aes(label = Count), vjust = -0.5, size = ANNOTATE_SIZE * 0.5) +
      labs(x = "Amplicon Region / Sequencing Type", y = "Number of Datasets") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Plot 4: Median Reads after DADA2 by Dataset (excluding Shotgun)
  output$samples_filter_plot4 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Exclude shotgun samples
    data <- data %>%
      filter(Sequencing_Type != "SG")
    
    if (nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Calculate median reads per dataset
    median_reads <- data %>%
      filter(!is.na(Num_Reads_Nonchim)) %>%
      group_by(Dataset_ID) %>%
      summarise(MedianReads = median(Num_Reads_Nonchim, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(MedianReads))
    
    if (nrow(median_reads) == 0) {
      return(empty_plot_message())
    }
    
    ggplot(median_reads, aes(x = reorder(Dataset_ID, -MedianReads), y = MedianReads)) +
      geom_bar(stat = "identity", fill = COLORS$PRIMARY) +
      labs(x = "Dataset ID", y = "Median Reads after DADA2") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_y_continuous(labels = scales::comma)
  })

  # Plot 5: Number of Datasets by Sequencing Technology
  output$samples_filter_plot5 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Count unique datasets per sequencing technology
    tech_counts <- data %>%
      filter(!is.na(Seq_Tech) & Seq_Tech != "") %>%
      distinct(Dataset_ID, Seq_Tech) %>%
      group_by(Seq_Tech) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))
    
    if (nrow(tech_counts) == 0) {
      return(empty_plot_message())
    }
    
    ggplot(tech_counts, aes(x = reorder(Seq_Tech, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = COLORS$PRIMARY) +
      geom_text(aes(label = Count), vjust = -0.5, size = ANNOTATE_SIZE * 0.5) +
      labs(x = "Sequencing Technology", y = "Number of Datasets") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Plot 6: Number of Datasets by Body Site
  output$samples_filter_plot6 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Count unique datasets per sample site
    site_dataset_counts <- data %>%
      filter(!is.na(Sample_Site) & Sample_Site != "") %>%
      distinct(Dataset_ID, Sample_Site) %>%
      group_by(Sample_Site) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Sample_Site = stringr::str_to_title(Sample_Site)) %>%
      arrange(desc(Count))
    
    if (nrow(site_dataset_counts) == 0) {
      return(empty_plot_message())
    }
    
    ggplot(site_dataset_counts, aes(x = reorder(Sample_Site, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = COLORS$PRIMARY) +
      geom_text(aes(label = Count), vjust = -0.5, size = ANNOTATE_SIZE * 0.5) +
      labs(x = "Body Site", y = "Number of Datasets") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Plot 7: Number of Samples by Body Site
  output$samples_filter_plot7 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Count samples per sample site
    site_counts <- data %>%
      filter(!is.na(Sample_Site) & Sample_Site != "") %>%
      group_by(Sample_Site) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Sample_Site = stringr::str_to_title(Sample_Site)) %>%
      arrange(desc(Count))
    
    if (nrow(site_counts) == 0) {
      return(empty_plot_message())
    }
    
    ggplot(site_counts, aes(x = reorder(Sample_Site, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = COLORS$PRIMARY) +
      geom_text(aes(label = Count), vjust = -0.5, size = ANNOTATE_SIZE * 0.5) +
      labs(x = "Body Site", y = "Number of Samples") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Plot 8: Analysis Group Pie Chart
  output$samples_filter_plot8 <- renderPlot({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      return(empty_plot_message())
    }
    
    # Define group labels
    group_labels <- c(
      "ACD" = "ACD: Active Celiac Disease (not on GFD)",
      "TCD" = "TCD: Treated Celiac Disease (on GFD)",
      "HC" = "HC: Healthy Control",
      "HC_GFD" = "HC_GFD: Healthy Control on a Gluten-Free Diet",
      "PCD" = "PCD: Prospective Celiac Disease",
      "PHC" = "PHC: Prospective Healthy Control"
    )
    
    # Count samples per group
    group_counts <- data %>%
      filter(!is.na(Group) & Group != "") %>%
      group_by(Group) %>%
      summarise(Count = n(), .groups = "drop")
    
    if (nrow(group_counts) == 0) {
      return(empty_plot_message())
    }
    
    # Add full labels
    group_counts$Label <- group_labels[group_counts$Group]
    group_counts$Label[is.na(group_counts$Label)] <- group_counts$Group[is.na(group_counts$Label)]
    
    # Calculate percentages and positions for labels
    group_counts <- group_counts %>%
      mutate(
        Percentage = Count / sum(Count) * 100,
        Position = cumsum(Count) - Count / 2
      )
    
    # Define colors for each group
    group_colors <- c(
      "ACD" = "#d9ad6c",
      "TCD" = "#7eb4de",
      "HC" = "#98c379",
      "HC_GFD" = "#c678dd",
      "PCD" = "#e06c75",
      "PHC" = "#56b6c2"
    )
    
    ggplot(group_counts, aes(x = "", y = Count, fill = Label)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 10) +
      scale_fill_manual(values = setNames(group_colors[group_counts$Group], group_counts$Label)) +
      labs(fill = "Analysis Group") +
      theme_void(base_size = PLOT_BASE_SIZE) +
      theme(legend.position = "right")
  })

  # Plot 9: Significant Factors Stacked Bar Plot with hover text
  output$samples_filter_plot9 <- renderPlotly({
    data <- filtered_samples()
    if (is.null(data) || nrow(data) == 0) {
      # Return empty plotly plot with message
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data to display after filtering", size = ANNOTATE_SIZE, color = "grey50") +
        theme_void(base_size = PLOT_BASE_SIZE) +
        theme(panel.background = element_rect(fill = "#f0f0f0", color = NA))
      return(ggplotly(p) %>% 
               layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
               config(displayModeBar = FALSE))
    }
    
    # Get columns for significant factors details
    factor_cols <- c("Short_term_Gluten_Challenge", "NCGS", "Other_Autoimmune", "Hookworm", "Possible_Celiac")
    factor_labels <- c("Short-term Gluten Challenge", "NCGS", "Other Autoimmune", "Hookworm", "Possible Celiac")
    
    # Create hover text for samples with significant factors
    # Summarize which factors are present per dataset for samples with Any_Significant_Factor = TRUE
    sig_data <- data %>%
      filter(Any_Significant_Factor == TRUE)
    
    no_sig_data <- data %>%
      filter(Any_Significant_Factor == FALSE | is.na(Any_Significant_Factor))
    
    # Count samples with significant factors per dataset and create hover text
    if (nrow(sig_data) > 0) {
      sig_summary <- sig_data %>%
        group_by(Dataset_ID) %>%
        summarise(
          Count = n(),
          Short_term_Gluten_Challenge_n = sum(Short_term_Gluten_Challenge == TRUE, na.rm = TRUE),
          NCGS_n = sum(NCGS == TRUE, na.rm = TRUE),
          Other_Autoimmune_n = sum(Other_Autoimmune == TRUE, na.rm = TRUE),
          Hookworm_n = sum(Hookworm == TRUE, na.rm = TRUE),
          Possible_Celiac_n = sum(Possible_Celiac == TRUE, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          Hover_Text = paste0(
            "Significant factors:<br>",
            ifelse(Short_term_Gluten_Challenge_n > 0, paste0("- Short-term Gluten Challenge: ", Short_term_Gluten_Challenge_n, "<br>"), ""),
            ifelse(NCGS_n > 0, paste0("- NCGS: ", NCGS_n, "<br>"), ""),
            ifelse(Other_Autoimmune_n > 0, paste0("- Other Autoimmune: ", Other_Autoimmune_n, "<br>"), ""),
            ifelse(Hookworm_n > 0, paste0("- Hookworm: ", Hookworm_n, "<br>"), ""),
            ifelse(Possible_Celiac_n > 0, paste0("- Possible Celiac: ", Possible_Celiac_n, "<br>"), "")
          ),
          Factor_Label = "Significant factors"
        ) %>%
        select(Dataset_ID, Count, Hover_Text, Factor_Label)
    } else {
      sig_summary <- data.frame(Dataset_ID = character(), Count = numeric(), Hover_Text = character(), Factor_Label = character())
    }
    
    # Count samples without significant factors per dataset
    if (nrow(no_sig_data) > 0) {
      no_sig_summary <- no_sig_data %>%
        group_by(Dataset_ID) %>%
        summarise(Count = n(), .groups = "drop") %>%
        mutate(
          Hover_Text = "No significant factors",
          Factor_Label = "No significant factors"
        )
    } else {
      no_sig_summary <- data.frame(Dataset_ID = character(), Count = numeric(), Hover_Text = character(), Factor_Label = character())
    }
    
    # Combine the data
    sig_counts <- bind_rows(no_sig_summary, sig_summary)
    
    if (nrow(sig_counts) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data to display after filtering", size = ANNOTATE_SIZE, color = "grey50") +
        theme_void(base_size = PLOT_BASE_SIZE) +
        theme(panel.background = element_rect(fill = "#f0f0f0", color = NA))
      return(ggplotly(p) %>% 
               layout(xaxis = list(showticklabels = FALSE), yaxis = list(showticklabels = FALSE)) %>%
               config(displayModeBar = FALSE))
    }
    
    # Order datasets by total sample count
    dataset_order <- sig_counts %>%
      group_by(Dataset_ID) %>%
      summarise(Total = sum(Count), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      pull(Dataset_ID)
    
    sig_counts$Dataset_ID <- factor(sig_counts$Dataset_ID, levels = dataset_order)
    sig_counts$Factor_Label <- factor(sig_counts$Factor_Label, levels = c("Significant factors", "No significant factors"))
    
    p <- ggplot(sig_counts, aes(x = Dataset_ID, y = Count, fill = Factor_Label, text = Hover_Text)) +
      geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
      scale_fill_manual(values = c("No significant factors" = COLORS$SECONDARY, "Significant factors" = "#e06c75")) +
      labs(x = "Dataset ID", y = "Number of Samples", fill = "") +
      theme_minimal(base_size = PLOT_BASE_SIZE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = "top")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.05)) %>%
      config(displayModeBar = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
