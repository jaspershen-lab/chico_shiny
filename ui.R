
# UI
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "CHICO Study"),
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem(
      "Data Visualization",
      tabName = "viz",
      icon = icon("line-chart")
    ),
    menuItem("Authors", tabName = "authors", icon = icon("users"))
  )),
  
  dashboardBody(
    tags$head(tags$style(
      HTML(
        "
                /* Change the background color of the header */
                .skin-blue .main-header .navbar,
                .skin-blue .main-header .logo {
                    background-color: #00458A;  /* NTU Blue */
                    border-bottom-color: #8E0C3A;  /* NTU Red */
                }

                /* Change the hover color of the sidebar menu */
                .skin-blue .sidebar-menu > li:hover > a,
                .skin-blue .sidebar-menu > li.active > a {
                    border-left-color: #8E0C3A;  /* NTU Red */
                }

                /* Other custom styles can be added as needed */
            "
      )
    )),
    tabItems(
      # About tab
      tabItem(tabName = "about", uiOutput("aboutContent")),
      
      # tabItem(tabName = "about", fluidPage(
      #   titlePanel(""), fluidRow(column(
      #     12, includeHTML("markdown/about.html")
      #   ))
      # )),
      
      # Data Visualization tab
      tabItem(tabName = "viz", fluidRow(
        column(
          width = 3,
          box(
            title = "Plot Controls",
            selectInput(
              "bacteria_level",
              "Level:",
              choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
              selected = "Phylum",
              multiple = FALSE
            ),
            selectInput(
              inputId = "bacteria_name",
              label = "Bacteria:",
              choices = phylum_name,
              selected = phylum_name[1],
            ),
            selectInput(
              inputId = "group_type",
              label = "Group type:",
              choices = c("Affect or not", "HPV risk", "HPV persistent"),
              selected = "Affect or not",
              multiple = FALSE
            ),
            selectInput(
              inputId = "group",
              label = "Group:",
              choices = c("Negative", "Positive"),
              selected = c("Negative", "Positive"),
              multiple = TRUE
            ),
            actionButton("generate_plot", "Generate plot", icon = icon("play")),
            # checkboxInput("smooth", "Smooth lines?", TRUE),
            # checkboxInput("smooth_one", "Only one smoothed line?", TRUE),
            # checkboxInput("points", "Show points?", TRUE),
            # numericInput(
            #   "width",
            #   "Download Width (in):",
            #   10,
            #   min = 1,
            #   max = 20
            # ),
            # numericInput(
            #   "height",
            #   "Download Height (in):",
            #   6,
            #   min = 1,
            #   max = 20
            # ),
            # selectInput("filetype", "File Type:", choices = c("PNG", "PDF")),
            # downloadButton("downloadPlot", "Download Plot"),
            width = NULL
          )
        ), column(
          width = 9,
          box(plotlyOutput("box_plot"), width = NULL),
          box(DTOutput("stats_table"), width = NULL)
        )
      )),
      # Authors tab
      tabItem(tabName = "authors", uiOutput("authorContent"))
    ),
    tags$head(tags$style(
      HTML(
        "
        .author-img {
          border-radius: 50%;
          width: 120px;
          height: 150px;
          object-fit: cover;
        }
      "
      )
    )),
    tags$footer(
      div(
        style = "background-color: #ecf0f4; display: flex; align-items: center; justify-content: left; padding: 10px; height: 80px; position: fixed; bottom: 0; width: 100%; z-index: 100; border-top: 1px solid #ccc;",
        tags$img(
          src = "ntu_logo.png",
          height = "80px",
          style = "margin-right: 15px;",
          onerror = "this.onerror=null; this.src='default_logo.png';"
        ),
        # Add vertical line
        div(style = "border-left: 1px solid #ccc; height: 50px; margin-right: 15px;"),
        tags$img(
          src = "SUSig_Red_Stree_Stacked_Left.png",
          height = "80px",
          style = "margin-right: 15px;",
          onerror = "this.onerror=null; this.src='default_logo.png';"
        ),
        # Add vertical line
        div(style = "border-left: 1px solid #ccc; height: 50px; margin-right: 15px;"),
        tags$img(
          src = "CHICO_logo.png",
          height = "60px",
          style = "margin-right: 15px;",
          onerror = "this.onerror=null; this.src='default_logo.png';"
        ),
        # Add vertical line
        div(style = "border-left: 1px solid #ccc; height: 50px; margin-right: 15px;"),
        div(
          # HTML("The Shen Lab at Nanyang Technological University Singapore"),
          # HTML("<br>"),
          tags$a(
            href = "http://www.shen-lab.org",
            target = "_blank",
            tags$i(class = "fa fa-house", style = "color: purple;"),
            " Shen Lab",
            style = "text-align: left; margin-left: 10px;"
          ),
          tags$a(
            href = "https://www.shen-lab.org/#contact",
            target = "_blank",
            tags$i(class = "fa fa-envelope", style = "color: purple;"),
            " Email",
            style = "text-align: left; margin-left: 10px;"
          ),
          tags$a(
            href = "https://github.com/jaspershen-lab",
            target = "_blank",
            tags$i(class = "fa fa-github", style = "color: purple;"),
            " GitHub",
            style = "text-align: left; margin-left: 10px;"
          ),
          style = "text-align: left;"
        )
      )
    )
  )
)
