# Load required packages
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(reshape2)
library(zip)

# Load historical data (replace with actual data as needed)
historical_data <- data.frame(
  Year = rep(c("2021-2022", "2022-2023"), each = 4),
  Category = rep(c("Below 100K", "100K_500K", "500K_1M", "Above 1M"), times = 2),
  NumSuccessfulProjects = c(16, 6, 1, 1, 21, 3, 2, 2),
  SuccessRatePercent = c(47, 33, 25, 100, 39, 14, 40, 25),
  TotalAmountSecured = c(328778, 1384068, 666647, 1231940, 386236, 598292, 1498418, 3587773),
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  titlePanel("Financial Scenario Modeling for Grant Projects"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Input Parameters"),
      numericInput("cost_per_hour", "Cost per Hour (£):", value = 50, min = 0),
      tabsetPanel(
        tabPanel("Below £100K",
                 numericInput("num_success_below100k", "Number of Successful Projects:", value = 21, min = 0),
                 sliderInput("success_rate_below100k", "Success Rate (%):", min = 0, max = 100, value = 39),
                 numericInput("total_amount_below100k", "Total Amount Secured (£):", value = 386236, min = 0),
                 numericInput("hours_per_project_below100k", "Hours per Project:", value = 40, min = 0)
        ),
        tabPanel("£100K–£500K",
                 numericInput("num_success_100k_500k", "Number of Successful Projects:", value = 3, min = 0),
                 sliderInput("success_rate_100k_500k", "Success Rate (%):", min = 0, max = 100, value = 14),
                 numericInput("total_amount_100k_500k", "Total Amount Secured (£):", value = 598292, min = 0),
                 numericInput("hours_per_project_100k_500k", "Hours per Project:", value = 44, min = 0)
        ),
        tabPanel("£500K–£1M",
                 numericInput("num_success_500k_1m", "Number of Successful Projects:", value = 2, min = 0),
                 sliderInput("success_rate_500k_1m", "Success Rate (%):", min = 0, max = 100, value = 40),
                 numericInput("total_amount_500k_1m", "Total Amount Secured (£):", value = 1498418, min = 0),
                 numericInput("hours_per_project_500k_1m", "Hours per Project:", value = 48, min = 0)
        ),
        tabPanel("Above £1M",
                 numericInput("num_success_above1m", "Number of Successful Projects:", value = 2, min = 0),
                 sliderInput("success_rate_above1m", "Success Rate (%):", min = 0, max = 100, value = 25),
                 numericInput("total_amount_above1m", "Total Amount Secured (£):", value = 3587773, min = 0),
                 numericInput("hours_per_project_above1m", "Hours per Project:", value = 52, min = 0)
        )
      ),
      actionButton("run_analysis", "Run Analysis"),
      hr(),
      h3("Export Options"),
      downloadButton("download_data", "Download Results Table"),
      downloadButton("download_plot", "Download Plots")
    ),
    
    mainPanel(
      h3("Analysis Results"),
      DTOutput("results_table"),
      br(),
      h4("Visualizations"),
      tabsetPanel(
        tabPanel("Amount per Hour",
                 plotlyOutput("amount_per_hour_plot")
        ),
        tabPanel("ROI Percentage",
                 plotlyOutput("roi_plot")
        ),
        tabPanel("Cost-Benefit Ratio",
                 plotlyOutput("cost_benefit_plot")
        ),
        tabPanel("Historical Comparison",
                 selectInput("select_year", "Select Year for Comparison:", choices = unique(historical_data$Year)),
                 plotlyOutput("historical_plot")
        )
      ),
      br(),
      h4("Key Financial Metrics"),
      fluidRow(
        column(4, strong("Total Amount Secured:"), textOutput("total_amount_secured")),
        column(4, strong("Total Costs:"), textOutput("total_costs")),
        column(4, strong("Total Hours Invested:"), textOutput("total_hours_invested"))
      ),
      fluidRow(
        column(4, strong("Overall Amount Secured per Hour:"), textOutput("overall_amount_per_hour")),
        column(4, strong("Overall ROI (%):"), textOutput("overall_roi")),
        column(4, strong("Overall Cost-Benefit Ratio:"), textOutput("overall_cost_benefit"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive expression to store the results
  results <- reactive({
    req(input$num_success_below100k, input$success_rate_below100k)
    
    categories <- c("Below 100K", "100K_500K", "500K_1M", "Above 1M")
    
    num_success <- c(input$num_success_below100k,
                     input$num_success_100k_500k,
                     input$num_success_500k_1m,
                     input$num_success_above1m)
    
    success_rate <- c(input$success_rate_below100k,
                      input$success_rate_100k_500k,
                      input$success_rate_500k_1m,
                      input$success_rate_above1m) / 100  # Convert to decimal
    
    total_amount_secured <- c(input$total_amount_below100k,
                              input$total_amount_100k_500k,
                              input$total_amount_500k_1m,
                              input$total_amount_above1m)
    
    hours_per_project <- c(input$hours_per_project_below100k,
                           input$hours_per_project_100k_500k,
                           input$hours_per_project_500k_1m,
                           input$hours_per_project_above1m)
    
    # Handle cases where success rate is zero to avoid division by zero
    num_applications <- ifelse(success_rate > 0, num_success / success_rate, 0)
    
    # Calculate total hours invested
    total_hours_invested <- num_applications * hours_per_project
    
    # Calculate total costs
    cost_per_hour <- input$cost_per_hour
    total_costs <- total_hours_invested * cost_per_hour
    
    # Calculate amount secured per hour, handle zero total hours invested
    amount_secured_per_hour <- ifelse(total_hours_invested > 0, total_amount_secured / total_hours_invested, 0)
    
    # Calculate ROI percentages, handle zero total costs
    roi_percentage <- ifelse(total_costs > 0, ((total_amount_secured - total_costs) / total_costs) * 100, 0)
    
    # Calculate cost-benefit ratios, handle zero total costs
    cost_benefit_ratio <- ifelse(total_costs > 0, total_amount_secured / total_costs, 0)
    
    # Create data frame with R-friendly column names
    data.frame(
      Category = categories,
      NumSuccessfulProjects = num_success,
      SuccessRatePercent = success_rate * 100,
      NumApplications = round(num_applications, 2),
      HoursPerProject = hours_per_project,
      TotalHoursInvested = round(total_hours_invested, 2),
      TotalCosts = round(total_costs, 2),
      TotalAmountSecured = total_amount_secured,
      AmountSecuredPerHour = round(amount_secured_per_hour, 2),
      ROIPercent = round(roi_percentage, 2),
      CostBenefitRatio = round(cost_benefit_ratio, 2),
      stringsAsFactors = FALSE
    )
  })
  
  # Render the results table
  output$results_table <- renderDT({
    datatable(results(), options = list(pageLength = 5))
  })
  
  # Render the amount per hour plot using Plotly
  output$amount_per_hour_plot <- renderPlotly({
    df <- results()
    req(nrow(df) > 0)  # Ensure df has data
    p <- ggplot(df, aes(x = Category, y = AmountSecuredPerHour, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Amount Secured per Hour by Grant Category",
           x = "Grant Category",
           y = "Amount Secured per Hour (£/hour)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Render the ROI percentage plot using Plotly
  output$roi_plot <- renderPlotly({
    df <- results()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = Category, y = ROIPercent, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "ROI Percentage by Grant Category",
           x = "Grant Category",
           y = "ROI (%)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Render the cost-benefit ratio plot using Plotly
  output$cost_benefit_plot <- renderPlotly({
    df <- results()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = Category, y = CostBenefitRatio, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Cost-Benefit Ratio by Grant Category",
           x = "Grant Category",
           y = "Cost-Benefit Ratio") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Render the historical comparison plot
  output$historical_plot <- renderPlotly({
    df_current <- results()
    req(nrow(df_current) > 0)
    df_historical <- subset(historical_data, Year == input$select_year)
    
    # Merge current and historical data
    df_merged <- merge(df_current[, c("Category", "TotalAmountSecured")],
                       df_historical[, c("Category", "TotalAmountSecured")],
                       by = "Category", suffixes = c("_Current", "_Historical"))
    
    # Reshape data to long format
    df_long <- reshape2::melt(df_merged, id.vars = "Category",
                              measure.vars = c("TotalAmountSecured_Current", "TotalAmountSecured_Historical"),
                              variable.name = "Scenario", value.name = "TotalAmountSecured")
    
    # Modify Scenario names
    df_long$Scenario <- gsub("TotalAmountSecured_", "", df_long$Scenario)
    df_long$Scenario[df_long$Scenario == "Current"] <- "Current Scenario"
    df_long$Scenario[df_long$Scenario == "Historical"] <- input$select_year
    
    p <- ggplot(df_long, aes(x = Category, y = TotalAmountSecured, fill = Scenario)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Total Amount Secured: Current vs Historical",
           x = "Grant Category",
           y = "Total Amount Secured (£)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Calculate and render total amount secured
  output$total_amount_secured <- renderText({
    df <- results()
    total_amount <- sum(df$TotalAmountSecured)
    paste0("£", format(round(total_amount, 2), big.mark = ","))
  })
  
  # Calculate and render total costs
  output$total_costs <- renderText({
    df <- results()
    total_costs <- sum(df$TotalCosts)
    paste0("£", format(round(total_costs, 2), big.mark = ","))
  })
  
  # Calculate and render total hours invested
  output$total_hours_invested <- renderText({
    df <- results()
    total_hours <- sum(df$TotalHoursInvested)
    format(round(total_hours, 2), big.mark = ",")
  })
  
  # Calculate and render overall amount secured per hour
  output$overall_amount_per_hour <- renderText({
    df <- results()
    total_amount <- sum(df$TotalAmountSecured)
    total_hours <- sum(df$TotalHoursInvested)
    overall_amount_per_hour <- ifelse(total_hours > 0, total_amount / total_hours, 0)
    paste0("£", format(round(overall_amount_per_hour, 2), big.mark = ","))
  })
  
  # Calculate and render overall ROI percentage
  output$overall_roi <- renderText({
    df <- results()
    total_amount <- sum(df$TotalAmountSecured)
    total_costs <- sum(df$TotalCosts)
    overall_roi <- ifelse(total_costs > 0, ((total_amount - total_costs) / total_costs) * 100, 0)
    paste0(round(overall_roi, 2), "%")
  })
  
  # Calculate and render overall cost-benefit ratio
  output$overall_cost_benefit <- renderText({
    df <- results()
    total_amount <- sum(df$TotalAmountSecured)
    total_costs <- sum(df$TotalCosts)
    overall_cbr <- ifelse(total_costs > 0, total_amount / total_costs, 0)
    round(overall_cbr, 2)
  })
  
  # Download results table
  output$download_data <- downloadHandler(
    filename = function() {
      paste("results_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
  
  # Download plots as a zip file
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plots_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      tmpdir <- tempdir()
      
      # Save plots
      plots <- list(
        amount_per_hour = ggplot(results(), aes(x = Category, y = AmountSecuredPerHour, fill = Category)) +
          geom_bar(stat = "identity") +
          labs(title = "Amount Secured per Hour by Grant Category",
               x = "Grant Category",
               y = "Amount Secured per Hour (£/hour)") +
          theme_minimal(),
        roi_percentage = ggplot(results(), aes(x = Category, y = ROIPercent, fill = Category)) +
          geom_bar(stat = "identity") +
          labs(title = "ROI Percentage by Grant Category",
               x = "Grant Category",
               y = "ROI (%)") +
          theme_minimal(),
        cost_benefit_ratio = ggplot(results(), aes(x = Category, y = CostBenefitRatio, fill = Category)) +
          geom_bar(stat = "identity") +
          labs(title = "Cost-Benefit Ratio by Grant Category",
               x = "Grant Category",
               y = "Cost-Benefit Ratio") +
          theme_minimal()
      )
      
      # Create filenames for the plots
      plot_files <- file.path(tmpdir, paste0(names(plots), ".png"))
      
      # Save each plot as a PNG file
      for (i in seq_along(plots)) {
        ggsave(filename = plot_files[i], plot = plots[[i]], width = 8, height = 6)
      }
      
      # Zip the files
      zip::zipr(zipfile = file, files = plot_files)
    },
    contentType = "application/zip"
  )
}

# Run the application
shinyApp(ui = ui, server = server)
