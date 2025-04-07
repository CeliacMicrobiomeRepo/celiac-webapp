# This is the R Shiny web application used to view the Celiac Microbiome Repository


# Define color constants
COLORS <- list(
  PRIMARY = "#d9ad6c",      # Main theme color
  SECONDARY = "#7eb4de",    # Secondary color (used for non-celiac)
  BACKGROUND = "#cccccc",    # Background color for maps
  TEXT = "white"            # Text color
)

# Load necessary libraries for the application
library(markdown)        # For rendering Markdown content
library(shiny)          # For building the Shiny web application
library(DT)             # For rendering interactive data tables
library(ggplot2)        # For creating plots and visualizations
library(rworldmap)      # For mapping geographical data
library(rworldxtra)     # Additional world map data
library(dplyr)          # For data manipulation
library(sf)             # For handling spatial data
library(ggplot2)        # For creating plots (duplicate, can be removed)
library(rnaturalearth)  # For natural earth map data
library(rnaturalearthdata) # For additional natural earth data
library(countrycode)    # For converting country names to codes
library(grid)           # For grid graphics
library(gridExtra)      # For arranging multiple grid graphics
library(reshape2)       # For reshaping data

# Define default columns to display for datasets and samples
# ALL: Dataset_ID	Bioproject_ID	Record_Link	Publication_Title	Publication_Link	Month_Of_Publication	DOI	Used_In_Previous_Meta_Analysis	Lit_Search_Source	Data_Source	Sequencing_Type	Sequencing_Technology	Prospective_Study	Sample_Sites	Amplicon_Region	V1	V2	V3	V4	V5	V6	Forward_Primer	Reverse_Primer	DNA_Extraction_Kit	Read_Pairing	Trimming_Of_Reads_After_Acquisition	Bowtie2_Alignment_Sensitivity	Host_Genome_Index	MetaPhlAn_Database	Fw_Read_Trim_Position	Rv_Read_Trim_Position	ASV_Table_Length_Filter	Notes_From_Processing	Age_Range	Num_Samples_Processed_And_With_Metadata	Num_Individuals_Processed_And_With_Metadata	Num_Celiac_Samples_Processed_And_With_Metadata	Num_GFD_Samples_Processed_And_With_Metadata	Num_Prospective_Celiac_Samples_Processed_And_With_Metadata	Longitudinal_Study	Country	Samples_With_Significant_Factors	Prospective_Studies	Shotgun_Studies	Study_Design_Description
DEFAULT_DATA_SET_COLUMNS <- c(
  "Dataset_ID", "Sequencing_Type", "Amplicon_Region", "Sample_Sites",
  "Country", "Publication_Title", "Num_Samples", "Num_Individuals",
  "Num_Celiac_Samples", "Num_GFD_Samples", "Num_Prospective_Celiac_Samples"
)
# ALL: Sample_ID	Dataset_ID	SRA_Run_ID	SRA_Project_ID	Month_of_Publication	Publication_DOI	Sequencing_Type	Amplicon_Region	Seq_Depth_Filtered	Seq_Tech	DNA_Ext_Kit	Paired_Reads	Sample_Site	Diagnosed_Celiac	Gluten_Free_Diet	Group	Will_Develop_Celiac	Group_Prospective_Study	Short_term_Gluten_Challenge	NCGS	Other_Autoimmune	Hookworm	Possible_Celiac	Any_Significant_Factor	Country	Age	Sex
DEFAULT_SAMPLE_COLUMNS <- c(
  "Sample_ID", "Dataset_ID", "Sample_Site", "Group", "Country",
  "Sequencing_Type", "Amplicon_Region"
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
    title = "Celiac Microbiome Repo (v1.0)",  # Title of the web app
    windowTitle = "Celiac Repository",  # Title that appears in the browser's UI
    position = "fixed-top",              # Set the navbar to fixed-top

    # Define each tab in the navigation bar
    tabPanel("About",
             # Div containing all contents, styled by help-tab-content
             tags$div(
               class = c("about-tab-content"),
               style = "max-width: 750px; margin: 0 auto;",
               # Include Markdown content for the About tab
               includeMarkdown("www/tab_about.md")
             )
    ), 

    tabPanel("Publication",
             # Div containing all contents, styled by help-tab-content
             tags$div(
               class = c("about-tab-content"),
               style = "max-width: 750px; margin: 0 auto;",
               # Include Markdown content for the Publication tab
               includeMarkdown("www/tab_publication.md")
             )
    ), 
    
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
                 width = 3,
                 class = "custom-sidebar"  # Custom styling for the sidebar
               ),
               mainPanel(
                 # Display the table for included datasets
                 h3("Datasets"),
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
                 h4("Filtered Summary"),
                 uiOutput("filtered_samples_summary"),  # Output for filtered summary
                 br(),
                 
                 # Filter options for samples
                 h4("Filter Samples"),
                 uiOutput("sample_site_filter"),  # UI for sample site filter
                 uiOutput("group_filter"),         # UI for group filter
                 uiOutput("dataset_filter"),       # UI for dataset filter
                 br(),
                 
                 # Filter options for sample columns
                 h4("Filter Columns"),
                 checkboxGroupInput(
                   "selected_sample_columns", 
                   "Select Columns to Display:", 
                   choices = NULL,  # Choices will be updated based on the samples data
                   selected = DEFAULT_SAMPLE_COLUMNS  # Default selected columns
                 ),
                 width = 3,
                 class = "custom-sidebar"  # Custom styling for the sidebar
               ),
               mainPanel(
                 h3("Samples"),
                 DT::dataTableOutput("samples_table"),  # Output for samples table
                 width = 9,
                 class = "custom-main"  # Custom styling for the main panel
               )
             )
    ),
    
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 # Options for plotting
                 h4("Plot Options"),
                 width = 3,
                 class = "custom-sidebar"  # Custom styling for the sidebar
               ),
               mainPanel(
                 # Display plots
                 h3("Plots"),
                 uiOutput("plot_grid"),  # Output for dynamic grid of plots
                 width = 9,
                 class = "custom-main"  # Custom styling for the main panel
               )
             )
    )
  )
)

# Server logic for the Shiny application
server <- function(input, output, session) {
  
  included_datasets <- NULL  # Initialize variable for datasets
  included_samples <- NULL    # Initialize variable for samples
  
  # Load the datasets file if it exists
  if (file.exists("repo_data/included_datasets.tsv")) {
    included_datasets <- read.table("repo_data/included_datasets.tsv", header = TRUE, sep = "\t")
    
    # Update the column choices for datasets dynamically
    updateCheckboxGroupInput(session, "selected_columns",
                             choices = colnames(included_datasets),
                             selected = DEFAULT_DATA_SET_COLUMNS)
  }
  
  # Load the samples file if it exists
  if (file.exists("repo_data/all_samples.tsv")) {
    included_samples <- read.table("repo_data/all_samples.tsv", header = TRUE, sep = "\t")
    
    # Update the column choices for samples dynamically
    updateCheckboxGroupInput(session, "selected_sample_columns",
                             choices = colnames(included_samples),
                             selected = DEFAULT_SAMPLE_COLUMNS)
    
    # Render the Sample Site filter UI
    output$sample_site_filter <- renderUI({
      selectInput("sample_site_filter_input",
                  "Sample Site:",
                  choices = unique(included_samples$Sample_Site),  # Unique sample sites
                  selected = NULL,
                  multiple = TRUE)  # Allow multiple selections
    })
    
    # Render the Group filter UI
    output$group_filter <- renderUI({
      selectInput("group_filter_input",
                  "Group:",
                  choices = unique(included_samples$Group),  # Unique groups
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
  }
  
  # Render the datasets table based on selected columns
  output$datasets_table <- DT::renderDataTable({
    if (!is.null(included_datasets) && !is.null(input$selected_columns)) {
      displayed_data <- included_datasets[, input$selected_columns, drop = FALSE]  # Select columns to display
      
      DT::datatable(displayed_data,
                    options = list(
                      pageLength = 50,  # Number of rows per page
                      autoWidth = FALSE,
                      scrollX = TRUE,   # Enable horizontal scrolling
                      scrollY = "75vh", # Set vertical scroll height
                      dom = 'tip',      # Display table information and pagination
                      buttons = c('csv', 'excel', 'pdf')  # Export options
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
      # Start with the full data
      filtered_data <- included_samples
      
      # Apply filters based on user selections
      if (!is.null(input$sample_site_filter_input) && length(input$sample_site_filter_input) > 0) {
        filtered_data <- filtered_data[filtered_data$Sample_Site %in% input$sample_site_filter_input, ]  # Filter by sample site
      }
      if (!is.null(input$group_filter_input) && length(input$group_filter_input) > 0) {
        filtered_data <- filtered_data[filtered_data$Group %in% input$group_filter_input, ]  # Filter by group
      }
      if (!is.null(input$dataset_filter_input) && length(input$dataset_filter_input) > 0) {
        filtered_data <- filtered_data[filtered_data$Dataset_ID %in% input$dataset_filter_input, ]  # Filter by dataset
      }
      
      # Then select the columns to display
      displayed_sample_data <- filtered_data[, input$selected_sample_columns, drop = FALSE]
      
      DT::datatable(displayed_sample_data,
                    options = list(
                      pageLength = 150,  # Number of rows per page
                      autoWidth = FALSE,
                      scrollX = TRUE,    # Enable horizontal scrolling
                      scrollY = "75vh",  # Set vertical scroll height
                      dom = 'tip',       # Display table information and pagination
                      buttons = c('csv', 'excel', 'pdf')  # Export options
                    ),
                    class = "display compact nowrap hover",  # Table styling
                    rownames = FALSE,  # Do not display row names
                    escape = FALSE,     # Allow HTML content
                    selection = 'none'  # Disable row selection
      )
    }
  })
  
  # Render summary for filtered samples
  output$filtered_samples_summary <- renderUI({
    if (!is.null(included_samples)) {
      # Start with the full data
      filtered_data <- included_samples
      
      # Apply filters based on user selections
      if (!is.null(input$sample_site_filter_input) && length(input$sample_site_filter_input) > 0) {
        filtered_data <- filtered_data[filtered_data$Sample_Site %in% input$sample_site_filter_input, ]  # Filter by sample site
      }
      if (!is.null(input$group_filter_input) && length(input$group_filter_input) > 0) {
        filtered_data <- filtered_data[filtered_data$Group %in% input$group_filter_input, ]  # Filter by group
      }
      if (!is.null(input$dataset_filter_input) && length(input$dataset_filter_input) > 0) {
        filtered_data <- filtered_data[filtered_data$Dataset_ID %in% input$dataset_filter_input, ]  # Filter by dataset
      }
      
      # Check the number of samples
      num_samples <- nrow(filtered_data)
      
      if (num_samples == 0) {
        HTML("<p>There are currently no samples that pass these filters.</p>")  # No samples found
      } else {
        num_datasets <- length(unique(filtered_data$Dataset_ID))  # Count unique datasets
        
        # Calculate the percentage of diagnosed celiac samples
        celiac_values <- filtered_data$Diagnosed_Celiac
        if (!is.null(celiac_values)) {
          celiac_values <- celiac_values[celiac_values %in% c(TRUE, FALSE)]  # Keep only boolean values
          if (length(celiac_values) > 0) {
            celiac_percentage <- round(mean(celiac_values) * 100, 1)  # Calculate percentage
          } else {
            celiac_percentage <- NA
          }
        } else {
          celiac_percentage <- NA
        }
        
        # Format numbers with commas
        num_samples_comma <- format(num_samples, big.mark = ",")
        
        datasets_txt <- if (num_datasets == 1) "dataset." else "datasets."
        samples_txt <- if (num_samples == 1) "sample from" else "samples from"
        
        # Build the summary text
        summary_text <- paste(
          "<p>Displayed are", num_samples_comma, samples_txt, num_datasets, datasets_txt
        )
        if (!is.na(celiac_percentage)) {
          summary_text <- paste0(summary_text, " ", celiac_percentage, "% of samples are diagnosed with celiac disease.")
        } else {
          summary_text <- paste(summary_text, "The percentage of samples diagnosed with celiac disease could not be calculated due to invalid values.")
        }
        
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
      plot1_data <- included_samples[included_samples$Diagnosed_Celiac == TRUE, ]
      
      # Extract 'Month_of_Publication' and remove NAs
      month_pub <- na.omit(plot1_data$Month_of_Publication)
      
      # Extract year suffix and construct full year
      year_suffix <- substr(month_pub, 5, 6)
      year_full <- as.numeric(paste0("20", year_suffix))
      
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
      ggplot(counts_per_year, aes(x = Year, y = CumulativeCount)) +
        geom_line(color = COLORS$PRIMARY, size = 1) +  # Line color and size
        geom_point(color = COLORS$PRIMARY, size = 2) +  # Point color and size
        labs(
          x = "Year of Publication",
          y = "Cumulative Number of Samples"
        ) +
        theme_minimal()  # Minimal theme for the plot
    }
  })
  
  # Download handler for Plot 1
  output$download_data_plot1 <- downloadHandler(
    filename = function() {
      "plot1_data.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Generate the data for Plot 1
      plot1_data <- included_samples[included_samples$Diagnosed_Celiac == TRUE, ]
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
      plot2_data <- included_samples[included_samples$Diagnosed_Celiac == TRUE, ]
      
      # Extract 'Sample_Site' and remove NAs
      sample_site <- na.omit(plot2_data$Sample_Site)
      
      # Capitalize first letter of each sample site
      sample_site <- stringr::str_to_title(sample_site)
      
      # Count occurrences and sort
      sample_site_counts <- as.data.frame(table(sample_site))  # Count samples per site
      colnames(sample_site_counts) <- c("SampleSite", "Count")
      sample_site_counts <- sample_site_counts[order(-sample_site_counts$Count), ]  # Sort by count
      
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
      plot2_data <- included_samples[included_samples$Diagnosed_Celiac == TRUE, ]
      sample_site <- na.omit(plot2_data$Sample_Site)
      sample_site <- stringr::str_to_title(sample_site)  # Capitalize first letter
      sample_site_counts <- as.data.frame(table(sample_site))  # Count samples per site
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
      
      # Get world map data
      world_map <- ne_countries(scale = "medium", returnclass = "sf")
      
      # Merge country counts with map data
      country_counts$iso_a3 <- countrycode(country_counts$Country, "country.name", "iso3c")  # Convert country names to ISO codes
      country_counts <- country_counts[!is.na(country_counts$iso_a3), ]  # Remove NAs
      map_data <- left_join(world_map, country_counts, by = c("iso_a3"))  # Join map data with counts
      map_data$Count[is.na(map_data$Count)] <- 0  # Replace NAs with 0
      
      # Plot
      ggplot(map_data) +
        geom_sf(aes(geometry = geometry), fill = COLORS$BACKGROUND, color = COLORS$TEXT) +  # Base map
        geom_point(data = map_data[map_data$Count > 0, ], aes(size = Count, geometry = geometry),
                   color = COLORS$PRIMARY, stat = "sf_coordinates") +  # Points for sample counts
        scale_size_continuous(range = c(2, 10), guide = FALSE) +  # Size scale for points
        labs(x = "", y = "") +
        theme_minimal() +
        theme(axis.text = element_blank(), axis.ticks = element_blank())  # Remove axis text and ticks
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
      plot4_data <- included_samples[grepl("16S", included_samples$Sequencing_Type, ignore.case = TRUE), ]
      
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
      plot4_data <- included_samples[grepl("16S", included_samples$Sequencing_Type, ignore.case = TRUE), ]
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
      
      # Create contingency table
      table_data <- table(
        GlutenFreeDiet = ifelse(plot5_data$Gluten_Free_Diet, "Gluten-free", "Non-gluten-free"),
        DiagnosedCeliac = ifelse(plot5_data$Diagnosed_Celiac, "Celiac", "Healthy")
      )
      
      # Convert table to data frame for plotting
      df_table <- as.data.frame.matrix(table_data)  # Convert to data frame
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
      write.csv(as.data.frame.matrix(table_data), file, row.names = TRUE)  # Write to CSV
    }
  )
  
  # Plot 6: Sample Group Distributions for Prospective Studies
  output$plot6 <- renderPlot({
    if (!is.null(included_samples)) {
      # Filter samples where 'Group_Prospective_Study' is 'non-CD' or 'CD'
      plot6_data <- included_samples[included_samples$Group_Prospective_Study %in% c("non-CD", "CD"), ]
      
      # Prepare data for plotting
      plot6_counts <- plot6_data %>%
        group_by(Dataset_ID, Group_Prospective_Study) %>%
        summarise(Count = n()) %>%
        ungroup()  # Count samples per dataset and group
      
      # Map 'Group_Prospective_Study' to readable labels and colors
      plot6_counts$SampleGroup <- ifelse(
        plot6_counts$Group_Prospective_Study == "CD", "Celiac", "Non-Celiac"
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
      plot6_data <- included_samples[included_samples$Group_Prospective_Study %in% c("non-CD", "CD"), ]
      plot6_counts <- plot6_data %>%
        group_by(Dataset_ID, Group_Prospective_Study) %>%
        summarise(Count = n()) %>%
        ungroup()  # Count samples per dataset and group
      write.csv(plot6_counts, file, row.names = FALSE)  # Write to CSV
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


