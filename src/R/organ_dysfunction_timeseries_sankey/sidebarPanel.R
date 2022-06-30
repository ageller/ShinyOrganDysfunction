# Sidebar panel for inputs 
# for the organ dysfunction timeseries sankey figure
 
organ_dysfunction_timeseries_sankey_sidebar <- function(id){

	ns <- NS(id)

	tagList(

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Select the criteria and organ to use."),
		radioButtons(ns("organ_dysfunction_timeseries_sankey_criteria_radio"), "Criteria",
			c("pSOFA","PELOD"),
			selected = "pSOFA",
			inline = TRUE,
		),
		radioButtons(ns("organ_dysfunction_timeseries_sankey_organs_radio"), "Organs:",
			organs,
			selected = "Neuro",
			inline = TRUE,
		),
		# div(
		# 	style = "min-height:20px;",
		# 	textOutput(ns("organ_dysfunction_timeseries_sankey_error"))
		# ),

	)
}