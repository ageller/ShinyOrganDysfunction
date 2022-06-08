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

# read in and manipulate the initial data and set various global quantities
source("src/R/init.R")

# general utility functions
source("src/R/utils.R")

# functions for creating the bar charts 
source("src/R/bar_chart_utils.R")

# modules for the ui
source("src/R/data_selection_panel.R")
source("src/R/update_plot_panel.R")
source("src/R/organ_bar_chart_sidebarPanel.R")
source("src/R/organ_support/mainPanel.R")
source("src/R/organ_failure/mainPanel.R")

# modules for the server
source("src/R/organ_support/server.R")
source("src/R/organ_failure/server.R")

# everything will be on the same namespace
namespace <- "Nelson"
ns <- NS(namespace)

# Define UI 
ui <- fluidPage(
	# I need this for the reset button
	useShinyjs(), 

	# change the color for the error messages
	tags$head(
		tags$style(HTML("
			.shiny-output-error-validation {
				color: #ff0000;
				font-style: italic;
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
		update_plot_sidebar(namespace),
	),

	mainPanel(
		tabsetPanel(
			id = "mainPanelTabSelected",
			tabPanel("Organ Support Type Bar Chart",
				value=1, 
				organ_support_main(namespace),
			),
			tabPanel("Organ Failure Bar Chart",
				value=2, 
				organ_failure_main(namespace)
			),
		),
		htmlOutput(ns("summary_table"))

	)
)




# Define server logic 
server <- function(input, output, session) {
	organ_support_server(namespace)
	organ_failure_server(namespace)

}



shinyApp(ui, server)