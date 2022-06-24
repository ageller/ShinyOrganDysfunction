# Sidebar panel for inputs 
# for the organ dysfunction timeseries figure
 
organ_dysfunction_timeseries_sidebar <- function(id){

	ns <- NS(id)

	tagList(

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Select the days to include."),
		checkboxGroupInput(ns("organ_dysfunction_criteria_checkbox"), "",
			criteria,
			selected = criteria,
			inline = TRUE
		),
		div(
			style = "min-height:20px;",
			textOutput(ns("organ_dysfunction_criteria_error"))
		),

	)
}