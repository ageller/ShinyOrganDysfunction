# Sidebar panel for inputs 
# for the organ support type figure 

organ_support_sidebar <- function(id){

	ns <- NS(id)

	tagList(

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Specify how to aggregate the data."),
		selectInput(
			ns("organ_support_agg1"), "First Aggregation Group", 
			c("Age Group" = "Age_Group", 
				"Outcome" = "Outcome",
				"Season Admission" = "Season_Admission",
				"Gender" = "Gender",
				"Malignancy" = "Malignancy",
				"Transplant" = "Transplant",
				"Technology Dependence" = "Technology_Dependence"
			),
			selected = "Age Group"
		),
		selectInput(
			ns("organ_support_agg2"), "Second Aggregation Group (optional)", 
			c("Age Group" = "Age_Group", 
				"Outcome" = "Outcome",
				"Season Admission" = "Season_Admission",
				"Gender" = "Gender",
				"Malignancy" = "Malignancy",
				"Transplant" = "Transplant",
				"Technology Dependence" = "Technology_Dependence",
				"None" = "None"
			),
			selected = "None"
		),

		div(
			style = "min-height:20px;",
			textOutput(ns("organ_support_aggError"))
		),

	)
}