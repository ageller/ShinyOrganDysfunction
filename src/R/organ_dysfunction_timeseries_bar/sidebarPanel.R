# Sidebar panel for inputs 
# for the organ dysfunction timeseries figure
 
organ_dysfunction_timeseries_sidebar <- function(id){

	ns <- NS(id)

	tagList(

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Select the criteria and organs to include."),
		checkboxGroupInput(ns("organ_dysfunction_timeseries_criteria_checkbox"), "Criteria:",
			criteria,
			selected = criteria,
			inline = TRUE,
		),
		checkboxGroupInput(ns("organ_dysfunction_timeseries_organs_checkbox"), "Organs:",
			organs,
			selected = organs,
			inline = TRUE,
		),
		div(
			style = "min-height:20px;",
			textOutput(ns("organ_dysfunction_timeseries_error"))
		),

	)
}