library(shiny)
library(ggvis)

plot_data = readRDS("data/bubble_data.rds")
descriptions = readRDS("data/descriptions.rds")
about = readLines("data/about.txt")

ui = navbarPage("Gender Bubbles",
  tabPanel("App",
    fluidPage(
      fluidRow(
        
        # Show plot selection options
        column(3,
          selectInput('indicator', 'Indicator', unique(plot_data$short_name)),
          selectInput('color', 'Color by', c("Region" = "region", "Income level" = "income")),
          selectInput('size', 'Bubble size', c("By population" = 1,
                                               "Equal" = 0, 
                                               "Compromise" = 0.75),
                                               selected = 0.75),
          
          # Show indicator description
          wellPanel(HTML("<b>About the indicator</b>"), textOutput('description'))
        ),
        
        # Disable settings button
        tags$head(tags$style(HTML('a[class="ggvis-dropdown-toggle"]{display:none;}'))),
        
        # Show plot; add padding column
        column(8, ggvisOutput('plot1')),
        column(1)
      )
    )
  ),
  # Show about the app panel
  tabPanel("About", htmlOutput('about'))
)

server = function(input, output) {
  
  # Reactively filter by selected indicator
  vis = reactive({
    plot_data[plot_data$short_name == input$indicator, ]
  })
  
  # Render indicator description
  output$description = renderText({
    descriptions$indicatorDesc[descriptions$short_name == input$indicator]
  })
  
  # Render about the app text
  output$about = renderUI({
    HTML(about)
  })
  
  # Create tooltip function
  gend_tip <- function(x) {
    isolate({
      if(is.null(x)) return(NULL)
      paste0(vis()$country[vis()$iso3c == x$iso3c], 
             " (", vis()$indicator_year[vis()$iso3c == x$iso3c], ")",
             "<br />Male: ", x$male,
             "<br />Female: ", x$female)
    })
  }
  
  # Define axes properties
  axes_props =
    axis_props(labels = list(fontSize = 16, fill = "#404040"),
               title = list(fontSize = 18, fill = "#404040"),
               axis = list(stroke = "#404040"),
               ticks = list(stroke = "#404040"))
  
  # Define legend properties
  leg_props =
    legend_props(labels = list(fontSize = 14),
                 symbols = list(stroke = NULL, opacity = 0.55, size=120))
  
  # Create ggvis object, sizing bubbles by user inputs
  vis %>%
    ggvis(x = ~male, y = ~female, fill = ~eval(as.name(input$color))) %>%
    layer_paths(x= ~parity_line, y= ~parity_line, strokeDash:=6, stroke := "#404040") %>% 
    layer_points(size := ~population * as.numeric(input$size)/300000 + (1-as.numeric(input$size)) * 120,
                 size.hover := ~population * as.numeric(input$size)/275000 + (1-as.numeric(input$size)) * 100 + 100,
                 fillOpacity := 0.55, fillOpacity.hover := 0.75, key := ~iso3c) %>%
    add_axis("x", title = "Male", title_offset = 45, properties = axes_props) %>%
    add_axis("y", title = "Female", title_offset = 45, properties = axes_props) %>%
    scale_nominal("fill",
                  range = c("#D36135", "#E6AA68", "#7FB069", "#A7BED3", "#1C77C3", "#995D81", "#E85D75")) %>%
    set_options(height=575, width=815, duration = 450) %>% 
    add_tooltip(gend_tip, "hover") %>%
    add_legend("fill", title = "", properties = leg_props) %>% 
    bind_shiny("plot1")
}

shinyApp(ui, server)