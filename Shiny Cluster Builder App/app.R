# This shiny app displays points on an interactive map by cluster definition
# It was prepared for NoCO REDI's 2019 Cluster Exploration Workshop
# by A.C. Repella. Extensively commented to support novice R-shiny users.
# Code for the App (this file only) is released as CC-BY-SA 4.0.
# Data files and any U.S. Cluster Mapping
# or Emsi-Harvard Cluster based terminology is not included under that license.

# this is extensively commented specifically to help Economic Developers
# use R and Shiny for data visualization


## LIBRARIES
require(shiny)
require(dplyr)
require(magrittr)
require(readxl) # reads excel files
require(kableExtra) # makes nice looking presentation tables (static)
require(DT) # makes nice data tables (filter/search)
require(plotly) # visualization with Plotly (bubble chart)

## LOAD DATA AND DEFINE GLOBALS

# It can be assumed that every function assumes data is
# structured as provided/instructed in the prep file
# this prep file is called "assemble_build_cluster_app_data.R"
# it will produce a file that uses this naming convention
load("build_a_cluster_data_YYYY-MM-DD.Rdata")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "bootstrap.css",

  # Application title
  titlePanel("Build an Industry Cluster"),

  # Sidebar contains "input" sections that users can change
  sidebarLayout(
    sidebarPanel(
      # this passcode is a good faith effort to shield raw numbers
      # from people who shouldn't see it per various EULAs
      # but it doesn't actually protect any sensitive data.
      # our EULA doesn't allow for interactive data for analysis
      # purposes to be shared with external parties. so, we needed to
      # make data access easy for partners during the workshop,
      # but not widely accessible.

      # UNLOCK CODE IS IN THE DATA PREP SCRIPT (prep_data_file_for_cluster_map_app.R)
      # 3 lines of code below to have input area for unlock code
      # 2 lines of code in server() lock content access (need to be un/commented as well)
      # UNCOMMENT THE NEXT 3 LINES to Have Unlock Code input on sidebar
      # passwordInput("pwIn",
      #              "Enter the Unlock Code to see content",
      #              width = '100%'),


      textInput("CustomName", "Name your cluster (optional)", "Custom Cluster"),
      radioButtons("cluster_list_type_choice", "Pick Subclusters Options from:",
        c(
          "Priority Clusters (23)" = "priority",
          "All Traded Clusters (41)" = "traded",
          "All Clusters" = "all"
        ),
        selected = NULL, inline = FALSE,
        width = NULL
      ),

      selectizeInput(
        "select_subs", "Choose Your Subclusters",
        choices = choice_list,
        multiple = TRUE
      ),
      width = 3
    ),


    # MAIN PANEL displays content across various tabs
    mainPanel(

      ## my extensive use of HTML line break <br /> for content spacing
      ## is definitely not a best practice.
      HTML("Build a cluster by combining subclusters.<br/><br/>"),
      htmlOutput("selected_values"),

      tabsetPanel(
        # FIRST TAB - nice table of Clusters using kableExtra
        tabPanel("Cluster Definition", htmlOutput("NAICS_list")),
        # SECOND TAB - summary ("Industry Table") in both of our focus regions
        tabPanel(
          "Summary Tables",
          HTML(paste0("<h2> Subcluster Components, <b>", region1, "</b> </h2>")),
          DTOutput("summary_table1"),
          HTML(paste0("<h2> Subcluster Components, <b>", region2, "</b> </h2>")),
          DTOutput("summary_table2")
        ),
        # THIRD TAB - a list of companies that are representative of the cluster
        tabPanel(
          "Companies",
          HTML("<h2> Representative Companies Included in Cluster</h2>"),
          htmlOutput("representative_cos_list"),
          HTML(paste0("<div>", rep_cos_text, "</div>"))
        ),
        # FOURTH TAB - the infamous Economic Development Bubble Chart
        #              implemented as an animated time series
        tabPanel(
          "Chart",
          # the chart needs a year.
          sliderInput("chart_year", "Chart Data Year",
            value = 2018, min = 2006, max = 2018, sep = "",
            ticks = FALSE,
            animate = animationOptions(
              interval = 1000,
              loop = FALSE,
              playButton = NULL,
              pauseButton = NULL
            )
          ),
          # this outputs the chart
          plotlyOutput("bubblechart"),
          # some disclaimer/detail text is prepped in the "Data Prep" script
          HTML(paste0("<div>", bubble_chart_text, "</div>"))
        ),
        # FIFTH TAB - We have some other clusters defined in our state
        #             this tab shows overlap between those systems and the custom definition
        tabPanel(
          "Other Cluster Systems",
          HTML('<div style="Clear:both;padding-top:20px;">
                        </div><H2>OEDIT Clusters</h2>
                        <p> These are the <a href="http://choosecolorado.com"
                        target = "_blank">
                        14 Colorado Key Industries (OEDIT)</a></p>'),
          htmlOutput("OEDIT_clusters"),
          HTML('<div style="Clear:both;padding-top:20px;"></div>
                        <H2>Metro Denver Clusters</h2> <p> These
                        <a href="http://www.metrodenver.org/industries/overview/"
                        target = "_blank">
                        clusters</a> include "partial"
                        NAICS that are split between clusters,
                        indicated as "(P)"</p>'),
          htmlOutput("MDEDC_clusters"),
          HTML('<div style="Clear:both;padding-top:20px;"></div>
                        <H2>Fort Collins Clusters</h2>
                        <p> These
                        <a href="https://www.fcgov.com/business/target-industries"
                        target = "_blank">
                        clusters </a> were defined by Fort Collins
                        in 2006.</p>'),
          htmlOutput("FC_clusters"),
          HTML('<div style="Clear:both;padding-top:20px;"></div><h2>
                         Longmont Clusters </h2><p> Advance Longmont defined
                        <a href="http://www.longmont.org/Longmont/media/Longmont/PDF%20Files/Advance-Longmont-Final.pdf"
                        target = "_blank">these clusters</a> in 2013.</p>'),
          htmlOutput("AL_clusters")
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {

  # this code changes the list of clusters that 'feeds' dropdown multi-select.
  # because there are just A LOT of subclusters.
  # unfortunately you have to pick one and stick with it in this implementation

  observe({
    x <- input$cluster_list_type_choice
    if (x == "priority") {
      choice_list <- priority.subclusters.list
    } else if (x == "traded") {
      choice_list <- traded.subclusters.list
    } else {
      choice_list <- all.subclusters.list
    }
    updateSelectInput(
      session, "select_subs",
      choices = choice_list # ,
      #  selected = choice_list[1]
    )
  })

  # This just prints a list of currently included subclusters
  output$selected_values <- renderPrint({
    # generate bins based on input$bins from ui.R
    cat(format(paste0(
      "Your custom cluster includes these subclusters: ",
      paste(input$select_subs, collapse = ", "),
      "<br/><br/><br/>"
    )))
  })


  # this is SECOND TAB table 1
  output$summary_table1 <- renderDT({
    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code")
    # )

    make_summary_table(input$select_subs, region1)
  })

  # this is SECOND TAB table 2
  output$summary_table2 <- renderDT({
    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code" )
    # )


    make_summary_table(input$select_subs, region2)
  })


  # this generates the bubble chart
  output$bubblechart <- ({
    renderPlotly({
      # UNLOCK CODE - un/comment lines to force/remove unlock code protection
      # validate(
      #  need(input$pwIn== unlock_code, "Please enter the unlock code" )
      # )

      update_bubble_chart(
        subclusters, input$select_subs,
        input$chart_year, input$CustomName
      )
    })
  })

  output$NAICS_list <- reactive({
    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code" )
    # )

    make_NAICS_table(input$select_subs)
  })

  # this prints the company table
  output$representative_cos_list <- reactive({
    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code"
    # ))

    make_company_table(input$select_subs)
  })

  ## THIS IS ALL THE TABLES for the 'OTHER CLUSTERS SYSTEMS' section.
  ## Each table = 1 call.

  output$AL_clusters <- reactive({
    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code"
    #  ))

    kable(create_cluster_def_data_table(
      input$select_subs,
      AL_clusters_df
    )) %>%
      kable_styling(font_size = 13, position = "float_left", "condensed") %>%
      collapse_rows(columns = 1, valign = "top")
  })

  output$MDEDC_clusters <- reactive({
    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code"
    #  ))

    kable(
      create_cluster_def_data_table(
        input$select_subs,
        MDEDC_clusters_df
      )
    ) %>%
      kable_styling(font_size = 13, position = "float_left", "condensed") %>%
      collapse_rows(columns = 1, valign = "top")
  })

  output$FC_clusters <- reactive({
    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code"
    #  ))

    kable(
      create_cluster_def_data_table(
        input$select_subs,
        FC_clusters_df
      )
    ) %>%
      kable_styling(font_size = 13, position = "float_left", "condensed") %>%
      collapse_rows(columns = 1, valign = "top")
  })

  output$OEDIT_clusters <- reactive({

    # UNLOCK CODE - un/comment lines to force/remove unlock code protection
    # validate(
    #  need(input$pwIn== unlock_code, "Please enter the unlock code" )
    # ))
    kable(create_cluster_def_data_table(
      input$select_subs,
      OEDIT_clusters_df
    )) %>%
      kable_styling(font_size = 13, position = "float_left", "condensed") %>%
      collapse_rows(columns = 1, valign = "top")
  })
}



## FUNCTION DEFS

## formats local clusters (see Other Clusters XLSX file) for display in nice kable table
## calculates the total number of NAICS in the local clusters
## that overlap the custom subcluster definition

create_cluster_def_data_table <- function(selected_subs, cluster_system_df) {
  x1 <- cluster_system_df %>% filter(Porter_Subcluster %in% selected_subs)
  if (dim(x1)[1] == 0) {
    return(data.frame("details" = "no overlap between systems"))
  }


  local_cluster_names <- as.character(unique(x1$Cluster_Name))

  nice_name <- x1$Cluster_System[1]

  x.part <- cluster_system_df %>%
    filter(Cluster_Name %in% local_cluster_names) %>%
    group_by(Cluster_Name) %>%
    summarize(total_NAICS = n())

  cols_for_table <- c("Porter_Cluster", "Porter_Subcluster", "NAICS")
  colnames_for_table <- c("Cluster", "Subcluster", "NAICS")

  if (!(all(is.na(x1$PartialNAICS)))) {
    cols_for_table <- c(cols_for_table, "PartialNAICS")
    colnames_for_table <- c(colnames_for_table, "")
  }
  cols_for_table <- c(cols_for_table, "Description", "Cluster_Name")
  colnames_for_table <- c(
    colnames_for_table,
    "Industry Description", paste0(nice_name, " Cluster")
  )

  if (!(all(is.na(x1$Subcluster) | all(x1$Subcluster == "")))) {
    cols_for_table <- c(cols_for_table, "Subcluster")
    colnames_for_table <- c(colnames_for_table, paste0(nice_name, " Subcluster"))
  }

  cluster_desc <- x1[, cols_for_table]
  cluster_desc %<>% left_join(x.part, by = "Cluster_Name")
  colnames(cluster_desc) <- c(
    colnames_for_table,
    paste0("# NAICS in ", nice_name, "  Cluster")
  )

  return(cluster_desc)
}


calculate_FC_cluster_defs <- function(selected_subs) {
  x1 <- which(FC.clusters$SubclusterName %in% selected_subs)
  if (length(x1) == 0) {
    return(data.frame("Cluster" = "no overlap between systems"))
  }

  cluster.names <- as.character(unique(FC.clusters$FC.Cluster[x1]))

  x.part <- group_by(
    FC.clusters[which(FC.clusters$FC.Cluster %in% cluster.names), ],
    FC.Cluster
  ) %>%
    summarize(total_NAICS = n())
  cluster.desc <- FC.clusters[x1, c(
    "Cluster", "SubclusterName", "NAICS",
    "Partial", "Description.x", "FC.Cluster"
  )]
  cluster.desc <- left_join(cluster.desc, x.part, by = "FC.Cluster")
  colnames(cluster.desc) <- c(
    "Cluster", "Subcluster", "NAICS", "", "NAICS Description",
    "FC Cluster", "Total NAICS in FC definition"
  )
  return(cluster.desc)
}



## returns which selected subclusters have overlaps with Adv. Longmont
## AL.clusters is global.
calculate_AL_cluster_defs <- function(selected_subs) {
  x1 <- which(AL.clusters$SubclusterName %in% selected_subs)
  if (length(x1) == 0) {
    return(data.frame("Cluster" = "no overlap between systems"))
  }

  cluster.names <- as.character(unique(AL.clusters$Cluster.Name[x1]))

  x.part <- group_by(AL.clusters[which(AL.clusters$Cluster.Name %in%
    cluster.names), ], Cluster.Name) %>%
    summarize(total_NAICS = n())
  cluster.desc <- AL.clusters[x1, c(
    "Cluster", "SubclusterName", "NAICS",
    "Description.x", "Cluster.Name"
  )]
  cluster.desc <- left_join(cluster.desc, x.part, by = "Cluster.Name")
  colnames(cluster.desc) <- c(
    "Cluster", "Subcluster", "NAICS",
    "NAICS Description", "Adv. Longmont Cluster (AL)",
    "Total NAICS in AL definition"
  )
  return(cluster.desc)
}


## returns which selected subclusters have overlaps with Metro Denver EDC
## MDEDC.clusters is global.
calculate_MDEDC_cluster_defs <- function(selected_subs) {
  x1 <- which(MDEDC.clusters$SubclusterName %in% selected_subs)

  if (length(x1) == 0) {
    return(data.frame("Cluster" = "no overlap between systems"))
  }

  cluster.names <- as.character(unique(MDEDC.clusters$CustomCluster[x1]))

  x.part <- group_by(MDEDC.clusters[which(MDEDC.clusters$CustomCluster %in%
    cluster.names), ], CustomCluster) %>%
    summarize(total_NAICS = n())
  cluster.desc <- MDEDC.clusters[x1, c(
    "Cluster", "SubclusterName", "NAICS",
    "Partial.Prop", "Description.x",
    "CustomCluster"
  )]

  cluster.desc <- left_join(cluster.desc, x.part, by = "CustomCluster")
  colnames(cluster.desc) <- c(
    "Cluster", "Subcluster", "NAICS", "Partial",
    "NAICS Description", "MDEDC Cluster",
    "Total NAICS in MDEDC definition"
  )
  return(cluster.desc)
}


## returns which selected subclusters have overlaps with Metro Denver EDC
## MDEDC.clusters is global.
calculate_OEDIT_cluster_defs <- function(selected_subs) {
  x1 <- which(OEDIT.clusters$SubclusterName %in% selected_subs)
  if (length(x1) == 0) {
    return(data.frame("Cluster" = "no overlap between systems"))
  }

  cluster.names <- as.character(unique(OEDIT.clusters$KEY.INDUSTRY[x1]))

  x.part <- group_by(OEDIT.clusters[which(OEDIT.clusters$KEY.INDUSTRY %in%
    cluster.names), ], KEY.INDUSTRY) %>%
    summarize(total_NAICS = n())
  cluster.desc <- OEDIT.clusters[x1, c(
    "Cluster", "SubclusterName", "NAICS",
    "Description",
    "KEY.INDUSTRY", "COMPONET", "ADVANCED.INDUSTRIES"
  )]

  cluster.desc <- left_join(cluster.desc, x.part, by = "KEY.INDUSTRY")
  colnames(cluster.desc) <- c(
    "Cluster", "Subcluster", "NAICS",
    "NAICS Description", "OEDIT Cluster",
    "OEDIT Cluster Component", "Adv. Industry?",
    "Total NAICS in OEDIT definition"
  )
  return(cluster.desc)
}




# format a nice table. accesses global subclusters df
# recalc's variables to have overall "custom cluster" values
make_summary_table <- function(selected_subs, region) {
  theyear <- 2018
  x <- subclusters[
    which(subclusters$SubclusterName %in% selected_subs &
      subclusters$year == theyear &
      subclusters$region == region),
    c(
      "Cluster", "SubclusterName", "jobs", "jobs.change",
      "jobs.pct.change", "LQ", "region", "national.jobs",
      "average_earnings", "Sum_Earnings", "earnings.jobs"
    )
  ]

  if (dim(x)[1] == 0) {
    return(NULL)
  }

  table_sum <- group_by(x, region) %>% summarise(
    jobs = sum(jobs),
    national.jobs = sum(national.jobs),
    jobs.change = sum(jobs.change),
    average_earnings = sum(Sum_Earnings) / sum(earnings.jobs)
  )

  table_sum$LQ <- (table_sum$jobs / as.numeric(totaljobs[
    which(totaljobs$year == theyear), region
  ])) /
    (table_sum$national.jobs / as.numeric(totaljobs[which(
      totaljobs$year == theyear
    ), "total.us.jobs"]))

  table_sum$jobs.pct.change <- table_sum$jobs.change / table_sum$jobs
  table_sum$Cluster <- " "
  table_sum$SubclusterName <- "Combined Total"
  x$Sum_Earnings <- NULL
  x$earnings.jobs <- NULL

  x <- rbind(x, table_sum)

  x$jobs <- round(x$jobs, 0)
  x$jobs.change <- round(x$jobs.change, 0)
  x$jobs.pct.change <- round(x$jobs.pct.change, 3)
  x$LQ <- round(x$LQ, 2)
  x$average_earnings <- round(x$average_earnings, 2)


  # get rid of excess columns
  x$national.jobs <- NULL
  x$region <- NULL

  colnames(x) <- c(
    "Cluster", "Subcluster", "2018 Jobs", "5 year job growth (#)", "5 year job growth (%)",
    "LQ", "av. earnings"
  )

  return(datatable(x, extensions = "Buttons", options = list(
    pageLength = 10, autoWidth = TRUE, dom = "Bfrtip", buttons = c("copy")
  ), class = "display") %>%
    formatCurrency(c("av. earnings"), "$", digits = 0) %>%
    formatPercentage(c("5 year job growth (%)"), 1))
}


# nice formatting of overview table of cluster by NAICS
# xwalk is global
make_NAICS_table <- function(selected_subs) {
  x <- xwalk[
    which(xwalk$SubclusterName %in% selected_subs),
    c("Cluster", "SubclusterName", "NAICS", "Description")
  ]

  if (dim(x)[1] == 0) {
    return(NULL)
  }

  colnames(x) <- c("Cluster", "Subcluster", "NAICS", "Description")

  x <- x[order(x$Cluster, x$Subcluster, x$NAICS), ]

  nice.table <- kable(x) %>%
    kable_styling(font_size = 13, position = "float_left", "condensed") %>%
    collapse_rows(columns = c(1, 2), valign = "top")

  return(nice.table)
}

# nice formatting of representative company list for selected subclusters.
# companies are global
make_company_table <- function(selected_subs) {
  x <- companies[
    which(companies$SubclusterName %in% selected_subs),
    c("SubclusterName", "Nice.Name")
  ]

  if (dim(x)[1] == 0) {
    return(NULL)
  }

  colnames(x) <- c("Subcluster", "Business Establishment")


  nice.table <- kable(x) %>%
    kable_styling(font_size = 13, position = "float_left", "condensed") %>%
    collapse_rows(columns = c(1, 2), valign = "top")

  return(nice.table)
}

# prep data for presentation in bubble chart.
# subclusters is global
make_bubble_df <- function(subclusters, selected_subs) {
  x <- subclusters[
    which(subclusters$SubclusterName %in% selected_subs),
    c(
      "Cluster", "SubclusterName", "jobs", "national.jobs",
      "jobs.change", "LQ", "year", "region"
    )
  ]

  df <- group_by(x, year, region) %>%
    summarize(
      jobs = sum(jobs),
      national.jobs = sum(national.jobs),
      jobs.change = sum(jobs.change)
    ) %>%
    left_join(totaljobs, by = c("year" = "year"))

  df$percent.change <- df$jobs.change / df$jobs
  df$region.total.jobs <- 0
  df$region.total.jobs <- ifelse(df$region == region1, df[[region1]], df[[region2]])

  df$LQ <- (df$jobs / df$region.total.jobs) / (df$national.jobs / df$total.us.jobs)
  df <- df[, c("percent.change", "LQ", "jobs", "year", "region")]
  colnames(df) <- c("x", "y", "z", "year", "Category")
  return(data.frame(ungroup(df)[, c(1:5)]))
}


# z must be a numeric vector
scale_df_bubble_area <- function(z, zmin = minjobs, zmax = maxjobs, plotmin = 15, plotmax = 200) {
  zscaled <- ((z - zmin) / (zmax - zmin)) *
    (plotmax - plotmin) + plotmin
  return(zscaled)
}



create_bubble_chart_plotly <- function(df, bubble.year, CustomName, zmin, zmax) {
  # define refence lines (plotly "shapes")
  df$x <- round(df$x * 100, 2)
  df$y <- round(df$y, 2)
  df$z <- round(df$z, 0)

  lines <- list(
    list(
      type = "line", line = list(color = "rgb(16, 172, 229)", width = 1.5),
      xref = "x", yref = "y",
      x0 = min(df$x) - 2, x1 = max(df$x) + 2,
      y0 = 1, y1 = 1
    ),

    list(
      type = "line", line = list(color = "rgb(16, 172, 229)", width = 1.5),
      xref = "x", yref = "y",
      x0 = 0, x1 = 0,
      y0 = min(df$y) - .5, y1 = max(df$y) + .5
    )
  )



  df$size <- scale_df_bubble_area(df$z)

  color_choices <- c("#E31737", "#787878")
  df <- df[which(df$year == bubble.year), ]

  p <- NULL

  p <- plot_ly(df,
    x = ~x, y = ~y, name = ~Category, type = "scatter", mode = "markers",
    hoverinfo = "text",
    text = ~ paste(
      Category, "<br /> % change:", x, "<br />LQ: ", y,
      "<br />jobs: ", z
    ),
    marker = list(
      size = ~size, sizemode = "area", opacity = 0.9,
      color = color_choices
    )
  ) %>%


    layout(
      title = paste0(CustomName, ", ", bubble.year),
      xaxis = list(
        title = "jobs, 5-year % change (Relative Growth)",
        showgrid = TRUE, range = c(-50, 50)
      ),
      yaxis = list(title = "LQ (Specialization)", showgrid = TRUE, range = c(-2, 5)),
      shapes = lines
    )

  return(p)
}


# controls the whole bubble chart process, filter data, send it to a chart
update_bubble_chart <- function(subclusters, selected_subs, the_year, CustomName) {
  if (length(selected_subs) == 0) {
    return(NULL)
  }

  x <- make_bubble_df(subclusters, selected_subs)

  create_bubble_chart_plotly(x, the_year, CustomName)
}



# Run the application
shinyApp(ui = ui, server = server)
