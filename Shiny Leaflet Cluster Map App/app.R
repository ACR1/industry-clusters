# This shiny app displays points on an interactive map by cluster definition
# It was prepared for NoCO REDI's 2019 Cluster Exploration Workshop
# by A.C. Repella.
# Code for the App (this file only) is released as CC-BY-SA 4.0.
# Data files and any U.S. Cluster Mapping
# or Emsi-Harvard Cluster based terminology is not included under that license.

# this is extensively commented specifically to help Economic Developers
# use R and Shiny for data visualization :)


library(shiny) # the library that makes this app work
library(leaflet) # Leaflet.js for R


## 1. LOAD ALL YOUR INPUT DATA
# this input data is assembled in "prep_data_file_for_cluster_map_app.R"
# the prep script saves all the data required for the app in this file.
load("cluster_map_data_YYYY-MM-DD.Rdata")


# 2. SET YOUR DEFAULT MAP VIEW (coordinates and zoom) ----------------
# ITEMS IN CAPS BELOW ARE USED AS GLOBALS
# this is the inital lat,lng (center of view) and zoom level on the map.
# Accessed in function get_leaflet_map(core_cluster,linked_clusters)
DEFAULT_ZOOM <- 9
DEFAULT_LAT <- 40.25
DEFAULT_LNG <- -104.9

# 2a. SET THE SCALING FACTOR FOR CIRCLES ON THE MAP ----------------
# this is the scaling factor for circles. see map function for details.
# this is a linear factor, so you'll want to make sure your original scaling
# represents the points well, too.
MAP_SCALE_FACTOR <- 55000000 # arbitrary -- whatever looks good

# if you want your circles to relatively bigger or smaller, change accordingly

# 3. DEFINE THE USER INTERFACE ----------------
# we put in a minimally customized bootswatch CSS File.
# You can use whatever you want.
ui <- fluidPage(
  theme = "bootstrap_customized.css",

  # Application title
  titlePanel("Cluster Center of Gravity - Example Map with Fake Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # this passcode is a good faith effort to shield raw numbers
      # from people who shouldn't see it per EULAs
      # but it doesn't actually protect any sensitive data.
      # our EULA doesn't allow for interactive data for analysis
      # purposes to be shared with external parties. so, we needed to
      # make data access easy for partners during the workshop process,
      # but not widely accessible. no sensitive data was published via the app.

      # UNLOCK CODE IS IN THE DATA PREP SCRIPT
      # (prep_data_file_for_cluster_map_app.R)
      # 3 lines of code below to have input area for unlock code
      # 2 lines of code in server() lock content access
      #   (need to be un/commented as well)
      # UNCOMMENT THE NEXT 3 LINES to Have Unlock Code input on sidebar

      # passwordInput("pwIn",
      #    "Enter the Unlock Code to see all content",
      #             width = '100%'),
      # radio buttons to pick which cluster list to use:
      radioButtons("cluster_list_type_choice",
        "Pick Cluster Options from:",
        c(
          "Priority (23)" = "priority",
          "All Traded" = "traded",
          "All Clusters" = "all"
        ),
        selected = NULL, inline = FALSE
      ),
      # select a cluster from a nice dropdown
      selectizeInput("selected_cluster", "Pick a Cluster",
        choices = choice_list,
        selected = choice_list[1]
      ),
      # traded clusters come with the option to show the closely
      # related clusters, too
      checkboxGroupInput("show_linked_cluster",
        label = "show linked clusters",
        choices = linked.clusters.list,
        selected = "", inline = FALSE,
        width = "100%"
      ),
      HTML(linked_cluster_text)
    ),

    # This panel will hold the Leaflet Map
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Map",
          HTML("<br /><h4>"),
          uiOutput("map_title"),
          HTML("</h4>"),
          # these css tags force the map to take up most of the vertical height
          tags$style(
            type = "text/css",
            "#cluster_map {height: calc(100vh - 150px) !important;}"
          ),
          # this is the map
          leafletOutput("cluster_map"),
          # this is the text at the bottom of the screen
          HTML(paste0("<small> ", exclusion_text, "<br/></br>", "</small>"))
        ),
        tabPanel(
          "More Info",
          HTML(paste0(
            map_text, "<small> ", exclusion_text, "<br/></br>",
            disclaimer_text, "</small>"
          ))
        )
      )
    )
  )
)
# 4. DEFINE SERVER LOGIC ----------------
# ('feed' calculations to generate a map based on user input in UI)
# this function defines server logic for each output item
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

  # this observer watches the cluster in the dropdown box
  # it updates the choices for linked clusters based on what is in that box
  observe({
    y <- input$selected_cluster
    if (y %in% clusterLinks.list) {
      linked_clusters_list <- clusterLinks$`Linked Cluster`[which(
        clusterLinks$`Core Cluster` == y)]
    } else {
      linked_clusters_list <- c("no closely linked traded clusters")
    }
    updateCheckboxGroupInput(session, "show_linked_cluster",
      choices = linked_clusters_list,
      selected = NULL, inline = FALSE
    )
  })

  output$map_title <- renderUI(get_map_title(input$selected_cluster,
                                             input$show_linked_cluster))


  # This code tells the map to go make itself, and then sends it to the UI
  output$cluster_map <- renderLeaflet({
    # UNCOMMENT THE TWO LINES BELOW TO ENABLE UNLOCK CODE
    # validate(need(input$pwIn== unlock_code,
    # "Please enter the unlock code to display map."))

    get_leaflet_map(input$selected_cluster, input$show_linked_cluster)
  })
}




# SUPPORT FUNCTIONS DEFINED BELOW ----------------

get_map_title <- function(core_cluster, linked_clusters) {
  links <- ifelse(length(linked_clusters) > 0, " with Linked Clusters", "")
  titletext <- paste0(core_cluster, links)
  return(titletext)
}

get_leaflet_map <- function(core_cluster, linked_clusters) {

  # subset the cluster points in the core selected
  # + any selected linked clusters into a dataframe
  df <- rbind(
    cluster_points[which(cluster_points$Cluster %in% core_cluster), ],
    cluster_points[which(cluster_points$Cluster %in% linked_clusters), ]
  )


  # employment has been scaled to a display value between .1 and 1.1,
  # area needs to be scaled up to a good display size
  # the scale factor below is arbitrary -- this is what I think looks good.
  df$area <- df$scaled_emp * MAP_SCALE_FACTOR
  # Leaflet wants a radius, not area. so... math.
  df$radius <- sqrt(df$area / pi)

  # this makes a rainbow color palette on the fly for each change to the map
  # this can use any other factor based approach to colors.
  # See Rcolorbrewer or Viridis for more choices.
  factpal <- colorFactor(rainbow(length(unique(df$Cluster))), df$Cluster)


  # this tells Leaflet what to put in the map
  # it also returns this leaflet to the function call location
  leaflet(df) %>%
    addProviderTiles("Stamen.TonerLite") %>%
    setView(lng = DEFAULT_LNG, lat = DEFAULT_LAT, zoom = DEFAULT_ZOOM) %>%
    addCircles(
      lng = ~LON, lat = ~LAT, weight = 1,
      radius = ~radius, popup = ~content, color = ~ factpal(Cluster),
      highlightOptions = highlightOptions(
        color = "gray10", weight = 1.5,
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = factpal, values = ~Cluster
    )
}

# RUN SHINY ----------------
# this is the one line that controls the entire process:
shinyApp(ui = ui, server = server)
