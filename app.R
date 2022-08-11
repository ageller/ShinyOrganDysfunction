# for tabs: https://shiny.rstudio.com/articles/tabsets.html
library(shiny)
library(shinyjs)
library(shinythemes)

library(dplyr)
library(tidyr)
library(stringr)

library(ggplot2)
library(ggpattern)
library(gginnards)
#library(ggpubr)
#library(ggiraph)
#library(plotly)

library(networkD3)
library(htmlwidgets)

# read in and manipulate the initial data and set various global quantities
source("src/R/init.R")

# general utility functions
source("src/R/utils.R")

# functions for creating the bar charts 
source("src/R/plotting_utils.R")

# modules for the ui
source("src/R/data_selection_panel.R")
source("src/R/update_plot_panel.R")
source("src/R/organ_support_bar/sidebarPanel.R")
source("src/R/organ_dysfunction_bar/sidebarPanel.R")
source("src/R/organ_dysfunction_criteria_bar/sidebarPanel.R")
source("src/R/organ_dysfunction_timeseries_line/sidebarPanel.R")
source("src/R/organ_dysfunction_timeseries_sankey/sidebarPanel.R")
source("src/R/demographics_sankey/sidebarPanel.R")

# modules for the main panel
source("src/R/organ_support_bar/mainPanel.R")
source("src/R/organ_dysfunction_bar/mainPanel.R")
source("src/R/organ_dysfunction_criteria_bar/mainPanel.R")
source("src/R/organ_dysfunction_timeseries_line/mainPanel.R")
source("src/R/organ_dysfunction_timeseries_sankey/mainPanel.R")
source("src/R/demographics_sankey/mainPanel.R")

# modules for the server
source("src/R/shared_server.R")
source("src/R/organ_support_bar/server.R")
source("src/R/organ_dysfunction_bar/server.R")
source("src/R/organ_dysfunction_criteria_bar/server.R")
source("src/R/organ_dysfunction_timeseries_line/server.R")
source("src/R/organ_dysfunction_timeseries_sankey/server.R")
source("src/R/demographics_sankey/server.R")

# everything will be on the same namespace
# Note: if you change this namespace, you need to also change it below in the Shiny.setInputValue command.
namespace <- "Nelson"
ns <- NS(namespace)

# Define UI 
ui <- fluidPage(

	# set the colors using the shinytheme library : https://rstudio.github.io/shinythemes/
	theme = shinytheme("paper"),

	# I need this for the reset button
	useShinyjs(), 

	# change the color for the error messages
	# change the style for inline checkboxes that cover two lines
	# Also get the window dimenstions for the tooltip placement
	# From https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
	# BUT note that Shiny.setInputValue requires the namespace, which I have hardcoded below.  (There does not appear to be a better way.)
	# If that namespace changes above, it must be changed here as well
	tags$head(
		tags$style(HTML("
			.shiny-output-error-validation {
				color: #ff0000;
				font-style: italic;
			}
			.checkbox-inline { 
				margin-left: 0px;
				margin-right: 10px;
			}
			.checkbox-inline+.checkbox-inline {
				margin-left: 0px;
				margin-right: 10px;
			}
			.shiny-notification {
				position: fixed; 
				top: 200px;
				left: 60%;
			}
			.tab-pane.active div{
				padding-left:4px;
			}

		")),
		tags$script('
			var dimensions = [0, 0];
			$(document).on("shiny:connected", function(e) {
				dimensions[0] = window.innerWidth;
				dimensions[1] = window.innerHeight;
				Shiny.setInputValue("Nelson-dimensions", dimensions);
			});
			$(window).resize(function(e) {
				dimensions[0] = window.innerWidth;
				dimensions[1] = window.innerHeight;
				Shiny.setInputValue("Nelson-dimensions", dimensions);
			});
		', ns(namespace)
		)
	),



	# App title 
	headerPanel("Pediatric Organ Dysfunction Explorer"),

	# ui 
	sidebarPanel(
		data_selection_sidebar(namespace),
		conditionalPanel(condition="input.mainPanelTabSelected==1", ns=ns, organ_support_bar_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==2", ns=ns, organ_dysfunction_bar_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==3", ns=ns, organ_dysfunction_criteria_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==4", ns=ns, organ_dysfunction_timeseries_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==5", ns=ns, demographics_sankey_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==6", ns=ns, organ_dysfunction_timeseries_sankey_sidebar(namespace)),
		update_plot_sidebar(namespace),
		conditionalPanel(condition="input.mainPanelTabSelected!=5 & input.mainPanelTabSelected!=6", ns=ns, zoom_info_sidebar(namespace)),

	),

	mainPanel(
		tabsetPanel(
			id = ns("mainPanelTabSelected"),
			tabPanel("1. Support Type (bar)",
				value=1, 
				organ_support_main(namespace),
			),
			tabPanel("2. Dysfunction Type (bar)",
				value=2, 
				organ_dysfunction_main(namespace)
			),
			tabPanel("3. Dysfunction Criteria (bar)",
				value=3, 
				organ_dysfunction_criteria_main(namespace)
			),
			tabPanel("4. Dysfunction Over Time (line)",
				value=4, 
				organ_dysfunction_timeseries_main(namespace)
			),
			tabPanel("5. Demographics (sankey)",
				value=5, 
				demographics_sankey_main(namespace)
			),
			tabPanel("6. Score Over Time (sankey)",
				value=6, 
				organ_dysfunction_timeseries_sankey_main(namespace)
			),

			tabPanel("Credits",
				value=7,
				div(style="font-size:16px; margin-top:50px",
					"Visualization developed by Aaron M. Geller (Northwestern University IT - Research Computing Services).  Data provided by Lazaro Nelson Sanchez-Pinto (Northwestern University - Feinberg School of Medicine).")
			),
		),
		htmlOutput(ns("summary_table"))

	)
)




# Define server logic 
server <- function(input, output, session) {
	shared_server(namespace)
	organ_support_server(namespace)
	organ_dysfunction_server(namespace)
	organ_dysfunction_criteria_server(namespace)
	organ_dysfunction_timeseries_server(namespace)
	organ_dysfunction_timeseries_sankey_server(namespace)
	demographics_sankey_server(namespace)
}



shinyApp(ui, server)
