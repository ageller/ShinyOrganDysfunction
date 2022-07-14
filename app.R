# for tabs: https://shiny.rstudio.com/articles/tabsets.html
library(shiny)
library(shinyjs)

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
source("src/R/organ_bar_chart_sidebarPanel.R")
source("src/R/organ_dysfunction_criteria_bar/sidebarPanel.R")
source("src/R/organ_dysfunction_timeseries_bar/sidebarPanel.R")
source("src/R/organ_dysfunction_timeseries_sankey/sidebarPanel.R")
source("src/R/demographics_sankey/sidebarPanel.R")

# modules for the main panel
source("src/R/organ_support_bar/mainPanel.R")
source("src/R/organ_dysfunction_bar/mainPanel.R")
source("src/R/organ_dysfunction_criteria_bar/mainPanel.R")
source("src/R/organ_dysfunction_timeseries_bar/mainPanel.R")
source("src/R/organ_dysfunction_timeseries_sankey/mainPanel.R")
source("src/R/demographics_sankey/mainPanel.R")

# modules for the server
source("src/R/shared_server.R")
source("src/R/organ_support_bar/server.R")
source("src/R/organ_dysfunction_bar/server.R")
source("src/R/organ_dysfunction_criteria_bar/server.R")
source("src/R/organ_dysfunction_timeseries_bar/server.R")
source("src/R/organ_dysfunction_timeseries_sankey/server.R")
source("src/R/demographics_sankey/server.R")

# everything will be on the same namespace
namespace <- "Nelson"
ns <- NS(namespace)

# Define UI 
ui <- fluidPage(
	# I need this for the reset button
	useShinyjs(), 

	# change the color for the error messages
	# change the style for inline checkboxes that cover two lines
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
		"))
	),



	# App title 
	headerPanel("Pediatric Organ Dysfunction Explorer"),

	# ui for the organ_support_type plot
	# adding the [1] to avoid printing TRUE to the screen (odd)
	sidebarPanel(
		data_selection_sidebar(namespace),
		conditionalPanel(condition="input.mainPanelTabSelected==1 | input.mainPanelTabSelected==2", organ_bar_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==3", organ_dysfunction_criteria_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==4", organ_dysfunction_timeseries_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==5", organ_dysfunction_timeseries_sankey_sidebar(namespace)),
		conditionalPanel(condition="input.mainPanelTabSelected==6", demographics_sankey_sidebar(namespace)),
		update_plot_sidebar(namespace),
	),

	mainPanel(
		tabsetPanel(
			id = "mainPanelTabSelected",
			tabPanel("Support Type (bar)",
				value=1, 
				organ_support_main(namespace),
			),
			tabPanel("Dysfunction Type (bar)",
				value=2, 
				organ_dysfunction_main(namespace)
			),
			tabPanel("Dysfunction Criteria (bar)",
				value=3, 
				organ_dysfunction_criteria_main(namespace)
			),
			tabPanel("Dysfunction Over Time (line)",
				value=4, 
				organ_dysfunction_timeseries_main(namespace)
			),
			tabPanel("Score Over Time (sankey)",
				value=5, 
				organ_dysfunction_timeseries_sankey_main(namespace)
			),
			tabPanel("Deomographics (sankey)",
				value=6, 
				demographics_sankey_main(namespace)
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
