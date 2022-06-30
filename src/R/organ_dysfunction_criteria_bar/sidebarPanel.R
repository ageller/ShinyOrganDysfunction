# Sidebar panel for inputs 
# for the organ dysfunction criteria figure
 
organ_dysfunction_criteria_sidebar <- function(id){

	ns <- NS(id)

	tagList(

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Select the days to include."),
		checkboxGroupInput(ns("organ_dysfunction_criteria_day_checkbox"), "",
			c(1,2,3,4,5,6,7),
			selected = c(1,2,3,4,5,6,7),
			inline = TRUE
		),
		div(
			style = "min-height:20px;",
			textOutput(ns("organ_dysfunction_criteria_day_error"))
		),

	)
}