# "Understanding Subclusters" App
#  examine subcluster metrics for a single Porter Cluster
# It was prepared for NoCO REDI's 2019 Cluster Exploration Workshop
# by A.C. Repella. Extensively commented to support novice R-shiny users.

# Code for the App (this file only) is released as CC-BY-SA 4.0.
# Data files are not included in this license and any content that
# crosswalks or otherwise aligns data with other sources (such as
# Emsi, U.S. Cluster Mapping, or the other cluster systems bundled here)
# can be considered the property of the organization that produced it.



# LOAD LIBRARIES
# if you don't have these installed, you'll need to do so:
# by typing: install.packages("library name") at the console
library(shiny)
library(tidyverse)
library(magrittr) # %<>% "forward assignment" pipe
library(knitr) # "nice looking" tables with kable()
library(kableExtra) # kable styling
library(plotly) # interactive plots
library(DT) # sortable data tables

# All data is prepped and assembled in a different script.
load("understand_subclusters_app_data_YYYY-MM-DD.Rdata")



PLOTLY_WIDTH = "99%"
PLOTLY_HEIGHT = "1400px"

# font styling for chart axis text
axis_font_style <-  list( family = "Arial Narrow",
                          size = 12,
                          color = "#7f7f7f"
                          )


subclusters_list= list() # just needs to be initialized. values added later.

# Define UI for application, all content "lives" in a page
# css can be found in the www file. It was downloaded from bootswatch, and modified for our use
ui <- fluidPage(
  theme = "bootstrap_customized.css",
  
  # Application title shown at top
  titlePanel("Explore Components (Subclusters) of Each Cluster"),
  
  # Sidebar with cluster section options
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # this passcode is a good faith effort to shield raw numbers
      # from people who shouldn't see it per various EULAs
      # but it doesn't actually protect any sensitive data.
      # our EULA doesn't allow for interactive data for analysis
      # purposes to be shared with external parties. so, we needed to
      # make data access easy for partners during the workshop process,
      # but not widely accessible.
      
      # UNLOCK CODE IS IN THE DATA PREP SCRIPT
      #   (prep_data_package_for_subcluster_app.R)
      # 3 lines of code below to have input area for unlock code
      # 2 lines of code in server() lock content access
      # (need to be un/commented as well)
      # UNCOMMENT THE NEXT 3 LINES to Have Unlock Code input on sidebar
      # passwordInput("pwIn",
      #              "Enter the Unlock Code to see content",
      #              width = '100%'),
      
      # passwordInput("pwIn",
      #              "Enter the Unlock Code to see all content",
      #              width = '100%'),
      
      # radio buttons to pick which cluster list to use
      radioButtons("cluster_list_type_choice",
                   "Pick Cluster Options from:",
                   c(
                     "Priority (23)" = "priority",
                     "All Traded (41)" = "traded",
                     "All Clusters" = "all"
                   ),
                   selected = NULL, inline = FALSE
      ),
      
      # then select a cluster from a nice dropdown
      selectizeInput("selected_cluster", "Pick a Cluster",
                     choices = choice_list,
                     selected = choice_list[1]
      ),
      checkboxGroupInput("regions", "Regions to Display",
                         choices = region_list,
                         selected = region_list[1]
      )
    ),
    
    # Main Area -- this is where all the values get displayed
    mainPanel(
      # the screen is divided into visual "tabs", tabsetPanel defines them
      tabsetPanel(
        tabPanel(
          "Industries",
          HTML(industries_text),
          htmlOutput("NAICS_list")
        ),
        tabPanel(
          "Subclusters",
          # slide to select year to display, or "play" thru animation.
          sliderInput("chart_year",
                      "Chart/Table Data Year",
                      min = 2006, max = 2018, sep = "",
                      value = 2018, ticks = FALSE,
                      animate = animationOptions(
                        interval = 1000,
                        loop = FALSE,
                        playButton = NULL,
                        pauseButton = NULL
                      )
          ), checkboxGroupInput("show_subcluster",
                                label = "Select Subclusters",
                                choices = subclusters_list,
                                selected = subclusters_list,
                                inline = FALSE
          ),
          tabsetPanel(
            # display data for each subcluster in a table
            tabPanel(
              "Subclusters - table",
              htmlOutput("subcluster_table_title"),
              DTOutput("subcluster_table")
            ),
            # display data for each subcluster in charts
            tabPanel(
              "Subclusters - charts",
              HTML(subclusters_text),
              
              
              uiOutput("ui_subclusters")
            )
          )
        ),
        # this is a list of companies that represent that cluster in our area.
        # our list (not included) was assembled from multiple sources.
        tabPanel(
          "Companies",
          HTML(companies_text),
          htmlOutput("company_list")
        )
      )
    )
  )
)


# This function defines server logic for each output item
# input, output are required, 'session' is needed to support observers
server <- function(input, output, session) {
  
  
  # this "observer" watches for changes to the cluster type list
  # it updates the choices available in the dropdown box
  observe({
    x <- input$cluster_list_type_choice
    if (x == "priority") {
      choice_list <- priority.clusters.list
    } else if (x == "traded") {
      choice_list <- traded.clusters.list
    } else {
      choice_list <- all.clusters.list
    }
    updateSelectInput(session, "selected_cluster",
                      choices = choice_list,
                      selected = choice_list[1]
    )
  })
  
  
  observe({
    z <- input$selected_cluster
    
    subclusters_list <- unique(all_subclusters$SubclusterName[
      which(all_subclusters$Cluster == z)
      ])
    
    nsubs <- length(subclusters_list)
    # ifelse(length(subclusters_list)>5, 5, length(subclusters_list))
    
    updateCheckboxGroupInput(session, "show_subcluster",
                             label = "Select Subclusters",
                             choices = subclusters_list,
                             selected = subclusters_list[1:nsubs],
                             inline = FALSE
    )
  })
  
  
  # this displays the list of companies as a nicely formatted HTML table
  output$company_list <- reactive({
    #validate(
    # need(input$pwIn== unlock_code, "Please enter the unlock code"
    # ))
    
    make_company_table(input$selected_cluster)
  })
  
  # this displays the industry list as a nicely formatted HTML table
  output$NAICS_list <- reactive({
    #validate(
    #  need(input$pwIn == unlock_code, "Please enter the unlock code")
    #)
    
    make_NAICS_table(input$selected_cluster)
  })
  
  
  output$ui_subclusters <- renderUI({
    #validate(
    #  need(input$pwIn == unlock_code, "Please enter the unlock code")
    #)
    
    plotlyOutput("subclusters",
                 width = PLOTLY_WIDTH, height = PLOTLY_HEIGHT
    )
  })
  
  output$subclusters <- renderPlotly({
    #validate(
    #  need(input$pwIn == unlock_code, "Please enter the unlock code")
    #)
    
    manage_bar_plot(
      input$regions, input$chart_year, input$selected_cluster,
      input$show_subcluster
    )
  })
  
  
  output$subcluster_table_title <- renderText(paste0(
    "<br /><br /><h2>", input$selected_cluster,
    " (Subclusters), ", input$chart_year, "</h2>"
  ))
  output$subcluster_table <- renderDT({
    
    #validate(
    # need(input$pwIn == unlock_code, "Please enter the unlock code"))
    make_subcluster_table(
      input$selected_cluster, input$show_subcluster,
      input$regions, input$chart_year
    )
  })
}



# FUNCTIONS BELOW THIS POINT SUPPORT SERVER FUNCTION

# formats dataframe of companies by Cluster/Sub into a nice table
# uses global data companies
make_company_table <- function(selected_cluster) {
  x <- companies[
    which(companies$Cluster == selected_cluster),
    c("SubclusterName", "Nice.Name")
    ]
  
  if (dim(x)[1] == 0) {
    return(NULL)
  }
  
  colnames(x) <- c("Subcluster", "Example Businesses")
  return(kable(x) %>%
           kable_styling(font_size = 13, position = "float_left", "condensed") %>%
           collapse_rows(columns = c(1, 2), valign = "top"))
}


# formats crosswalk (xwalk) dataframe into a nice html table
# for display.
# uses global dataframe xwalk
make_NAICS_table <- function(selected_cluster) {
  x <- xwalk[
    which(xwalk$Cluster %in% selected_cluster),
    c("Cluster", "SubclusterName", "NAICS", "Description")
    ]
  
  if (dim(x)[1] == 0) {
    return(NULL)
  }
  
  colnames(x) <- c("Cluster", "Subcluster", "NAICS", "Description")
  x <- x[order(x$Subcluster, x$NAICS), ]
  
  return(kable(x) %>%
           kable_styling(font_size = 13, position = "float_left", "condensed") %>%
           collapse_rows(columns = c(1, 2), valign = "top"))
}



# a function to take all subcluster data and put it into a nice table
make_subcluster_table <- function(selected_cluster, show_subcluster,
                                  regions, data_year) {
  x <- all_subclusters %>% 
    filter(all_subclusters$Cluster %in% selected_cluster &
             all_subclusters$SubclusterName %in% show_subcluster,
             year == data_year &
             region %in% regions)
  
  if (dim(x)[1] == 0) {
    return(NULL)
  }
  
  x$year <- NULL
  x <- x[, c("Cluster", "SubclusterName", "LQ", "jobs", "jobs.change", 
             "jobs.pct.change", "average_earnings", "Competitive.Effect", 
             "region")]
  
  # This matches our intervals -- 5 year and 2 year as specified. this needs to match
  # whatever intervals exist in the data.
  colnames(x) <- c("Cluster", "Subcluster", "LQ", "Jobs", "5 year jobs change",
                   "5 year job growth (%)", "av. earnings", 
                   "2 year Competitive Effect. Job Growth", "region")
  x$`5 year job growth (%)` <- x$`5 year job growth (%)` / 100
  return(datatable(x) %>%
           formatCurrency(c("av. earnings"), "$", digits = 0) %>%
           formatPercentage(c("5 year job growth (%)"), 1) %>%
           formatRound(c("Jobs"), digits = 0))
}


# function to take some data and make a nice column style barplot
# expected data format: data.frame(x = x values,
#                                  y = y values,
#                                 region= region names)
create_column_plotly <- function(title_text = "", df_in, showlegend = FALSE,
                                 y_title = "y", x_title = "x") {
  
  # transform data to wide format (col 1 = x value, each subs col a y series)
  df <- spread(df_in, key = c(region), value = c(y))
  
  # calculate generic y variable names, but save the region names for display
  cols <- dim(df)[2]
  
  ylabels <- colnames(df)[2:cols]
  new_ynames <- paste0("y", c(1:(cols - 1)))
  colnames(df)[2:cols] <- try(new_ynames, silent = TRUE)
  # setnames(df, old=ylabels, new=c(new_ynames), skip_absent=TRUE)
  
  # make a PLOTLY BAR PLOT
  p <- plot_ly(df,
               x = ~x, y = ~y1,
               hoverinfo = "text",
               text = ~ paste0(title_text, "<br >", x, ", ", ylabels[1], ": ", y1),
               type = "bar", name = ylabels[1],
               legendgroup = "region1", showlegend = showlegend
  )
  
  if (cols > 2) {
    # FYI %<>% is a forward assignment ("take this var, feed it in to f(),
    #  and also reassign result here")
    p %<>% add_trace(
      y = ~y2, name = ylabels[2], hoverinfo = "text",
      legendgroup = "region2",
      showlegend = showlegend,
      text = ~ paste0(
        title_text, "<br >",
        x, ", ", ylabels[2], ": ", y2
      )
    )
  }
  
  if (cols > 3) {
    p %<>% add_trace(
      y = ~y3, name = ylabels[3], hoverinfo = "text",
      legendgroup = "region3",
      showlegend = showlegend,
      text = ~ paste0(
        title_text, "<br >",
        x, ", ", ylabels[3], ": ", y3
      )
    )
  }
  p %<>% layout(
    yaxis = list(title = y_title, titlefont = axis_font_style),
    xaxis = list(titlefont = axis_font_style, tickfont = axis_font_style),
    barmode = "group",
    legend = list(x = 1.01, y = 0.9),
    colorway = THREE_COLORS[c(1:(cols - 1))],
    margin = list(b = 65),
    annotations = list(list(
      x = 0.01, y = 1.02,
      text = paste0("<b>", title_text, "</b>"),
      showarrow = F, xref = "paper", yref = "paper"
    ))
  )
  return(p)
}




# function to format data for a barplot
format_bar_chart_data <- function(regions, plot_year, selected_cluster, 
                                  show_subcluster, selected_variable) {
  df <- all_subclusters %>% filter(
    Cluster == selected_cluster,
    SubclusterName %in% show_subcluster,
    region %in% regions
  )
  colnames(df)[colnames(df) == selected_variable] <- "value"
  
  
  df %<>% filter(year == plot_year) %>%
    select(SubclusterName, region, value)
  
  colnames(df) <- c("x", "region", "y")
  return(data.frame(df))
}


# function to manage subplot process
manage_bar_plot <- function(regions, chart_year, selected_cluster,
                            show_subcluster) {
  
  if ( length(regions) == 0 | length(show_subcluster) == 0) {
    # when nothing is selected... show this instead
    axis_opts <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range=c(0,1)
      )
    x <- plot_ly(data.frame(x = 0, y = .9, 
                            z = "select both regions and subclusters to see a chart"),
                 x = ~x, y =~y, mode='text', type='scatter', text = ~z,
                 textposition = 'middle right') %>%
         layout(xaxis=axis_opts, 
                yaxis=axis_opts)
    return(x)
  }

  
  # these are the columnames in the data
  variable_columns <- c("LQ", "Competitive.Effect", "jobs", 
                        "average_earnings", "jobs.pct.change")
  
  # create plotly-ready dataframes for each chart
  
  df1 <- format_bar_chart_data(
    regions, chart_year, selected_cluster,
    show_subcluster, variable_columns[1]
  )
  
  df2 <- format_bar_chart_data(
    regions, chart_year, selected_cluster,
    show_subcluster, variable_columns[2]
  )
  
  df3 <- format_bar_chart_data(
    regions, chart_year, selected_cluster,
    show_subcluster, variable_columns[3]
  )
  
  df4 <- format_bar_chart_data(
    regions, chart_year, selected_cluster,
    show_subcluster, variable_columns[5]
  )
  
  df5 <- format_bar_chart_data(
    regions, chart_year, selected_cluster,
    show_subcluster, variable_columns[4]
  )
  
  
  # then, send each to plotly
  p1 <- suppressWarnings(create_column_plotly(
    title_text = paste0("Location Quotient ", chart_year),
    df1, showlegend = TRUE,
    y_title = "Location Quotient", x_title = ""
  ))
  
  p2 <- suppressWarnings(create_column_plotly(
    title_text = paste0("Competitive Effect as of", chart_year),
    df2, showlegend = FALSE,
    y_title = "job growth (#) due to comp. eff. (multi-year moving average)", x_title = ""
  ))
  
  
  p3 <- suppressWarnings(create_column_plotly(
    title_text = paste0("Total Jobs ", chart_year),
    df3, showlegend = FALSE,
    y_title = "#", x_title = ""
  ))
  
  p5 <- suppressWarnings(create_column_plotly(
    title_text = paste0("Average Earnings Per Job ", chart_year),
    df4, showlegend = FALSE,
    y_title = "$", x_title = ""
  ))
  
  p4 <- suppressWarnings(create_column_plotly(
    title_text = paste0("5-Year Job Growth (ending ", chart_year, ")"),
    df5, showlegend = FALSE,
    y_title = "5-year % change", x_title = ""
  ))
  
  # bundle plots together for combined display as a plotly subplot
  subplot(list(p1, p2, p3, p4, p5),
          shareX = FALSE, shareY = FALSE,
          titleY = TRUE, nrows = 5, margin = .05,
          heights = rep(.18, 5)
  )
}




# Run the application (the most important line...)
shinyApp(ui = ui, server = server)
