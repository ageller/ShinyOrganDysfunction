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

# functions for creating the bar charts 
source("src/R/bar_chart_utils.R")

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
	source("src/R/organ_support_type/sidebarPanel.R", local=TRUE)[1],

	source("src/R/organ_support_type/mainPanel.R", local=TRUE)[1]


)




# Define server logic 
server <- function(input, output) {

	# server for the organ_support_type plot
	source("src/R/organ_support_type/server.R", local=TRUE)
}



shinyApp(ui, server)