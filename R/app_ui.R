#' @import shiny
#' @import shinydashboard
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @export
app_ui <- function() {
  # 1. 注册静态资源路径 (对应 inst/www)
  addResourcePath("www", system.file("www", package = "chicoshiny"))
  
  # 2. 加载 UI 需要的下拉菜单数据
  # 注意：这里加载的数据只在当前函数作用域有效，不会污染全局环境
  load(system.file("data/phylum_name.rda", package = "chicoshiny"))
  
  # 3. UI 主体
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "CHICO Study"),
    dashboardSidebar(sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Data Visualization", tabName = "viz", icon = icon("line-chart")),
      menuItem("Authors", tabName = "authors", icon = icon("users"))
    )),
    
    dashboardBody(
      tags$head(tags$style(HTML("
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
        .author-img {
          border-radius: 50%;
          width: 120px;
          height: 150px;
          object-fit: cover;
        }
      "))),
      
      tabItems(
        # About tab
        tabItem(tabName = "about", uiOutput("aboutContent")),
        
        # Data Visualization tab
        tabItem(tabName = "viz", fluidRow(
          column(
            width = 3,
            box(
              title = "Plot Controls",
              selectInput(
                "bacteria_level", "Level:",
                choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                selected = "Phylum", multiple = FALSE
              ),
              selectInput(
                inputId = "bacteria_name",
                label = "Bacteria:",
                choices = phylum_name, # 这里使用了上面 load 进来的数据
                selected = phylum_name[1]
              ),
              selectInput(
                inputId = "group_type",
                label = "Group type:",
                choices = c("Affect or not", "HPV risk", "HPV persistent"),
                selected = "Affect or not", multiple = FALSE
              ),
              selectInput(
                inputId = "group",
                label = "Group:",
                choices = c("Negative", "Positive"),
                selected = c("Negative", "Positive"),
                multiple = TRUE
              ),
              actionButton("generate_plot", "Generate plot", icon = icon("play")),
              width = NULL
            )
          ),
          column(
            width = 9,
            box(plotlyOutput("box_plot"), width = NULL),
            box(DTOutput("stats_table"), width = NULL)
          )
        )),
        
        # Authors tab
        tabItem(tabName = "authors", uiOutput("authorContent"))
      ),
      
      tags$footer(
        div(
          style = "background-color: #ecf0f4; display: flex; align-items: center; justify-content: left; padding: 10px; height: 80px; position: fixed; bottom: 0; width: 100%; z-index: 100; border-top: 1px solid #ccc;",
          
          # ⚠️ 关键修改：src 路径增加了 www/
          tags$img(src = "www/CHICO_logo.png", height = "60px", style = "margin-right: 10px;", onerror = "this.onerror=null; this.src='www/default_logo.png';"),
          div(style = "border-left: 1px solid #ccc; height: 50px; margin-right: 10px;"),
          
          tags$img(src = "www/Chengdu_University_logo.png", height = "50px", style = "margin-right: 10px;", onerror = "this.onerror=null; this.src='www/default_logo.png';"),
          div(style = "border-left: 1px solid #ccc; height: 50px; margin-right: 10px;"),
          
          tags$img(src = "www/Fourth_Military_Medical_University_logo.png", height = "50px", style = "margin-right: 10px;", onerror = "this.onerror=null; this.src='www/default_logo.png';"),
          div(style = "border-left: 1px solid #ccc; height: 50px; margin-right: 10px;"),
          
          tags$img(src = "www/ntu_logo.png", height = "70px", style = "margin-right: 10px;", onerror = "this.onerror=null; this.src='www/default_logo.png';"),
          div(style = "border-left: 1px solid #ccc; height: 50px; margin-right: 10px;"),
          
          div(
            tags$a(href = "http://www.shen-lab.org", target = "_blank", tags$i(class = "fa fa-house", style = "color: purple;"), " Shen Lab", style = "text-align: left; margin-left: 10px;"),
            tags$a(href = "https://www.shen-lab.org/#contact", target = "_blank", tags$i(class = "fa fa-envelope", style = "color: purple;"), " Email", style = "text-align: left; margin-left: 10px;"),
            tags$a(href = "https://github.com/jaspershen-lab", target = "_blank", tags$i(class = "fa fa-github", style = "color: purple;"), " GitHub", style = "text-align: left; margin-left: 10px;"),
            style = "text-align: left;"
          )
        )
      )
    )
  )
}