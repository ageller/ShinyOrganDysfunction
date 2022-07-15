# Sidebar panel for inputs 
# for the organ dysfunction timeseries sankey figure
 
organ_dysfunction_timeseries_sankey_sidebar <- function(id){

	ns <- NS(id)

	tagList(

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Select the criteria and organ to use."),
		radioButtons(ns("organ_dysfunction_timeseries_sankey_criteria_radio"), "Criteria",
			c("pSOFA","PELOD2"),
			selected = "pSOFA",
			inline = TRUE,
		),

		conditionalPanel(condition="input.organ_dysfunction_timeseries_sankey_criteria_radio=='PELOD2'", ns=ns, 
			PELOD2_organ_dysfunction_timeseries_sankey_sidebar(id)
		),
		conditionalPanel(condition="input.organ_dysfunction_timeseries_sankey_criteria_radio=='pSOFA'", ns=ns, 
			pSOFA_organ_dysfunction_timeseries_sankey_sidebar(id)
		),

		# div(
		# 	style = "min-height:20px;",
		# 	textOutput(ns("organ_dysfunction_timeseries_sankey_error"))
		# ),

	)
}

PELOD2_organ_dysfunction_timeseries_sankey_sidebar <- function(id){
	ns <- NS(id)
	tagList(
		radioButtons(ns("organ_dysfunction_timeseries_sankey_organs_radio_PELOD2"), "Organs:",
			c("Neuro","Resp","CV","Renal","Heme"),
			selected = "Neuro",
			inline = TRUE,
		),
	)
}

pSOFA_organ_dysfunction_timeseries_sankey_sidebar <- function(id){
	ns <- NS(id)
	tagList(
		radioButtons(ns("organ_dysfunction_timeseries_sankey_organs_radio_pSOFA"), "Organs:",
			c("Neuro","Resp","CV","Renal", "Hepatic","Coag"),
			selected = "Neuro",
			inline = TRUE,
		),
	)
}