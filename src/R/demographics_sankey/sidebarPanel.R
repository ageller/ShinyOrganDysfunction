# Sidebar panel for inputs 
# for the demographics sankey figure
 
demographics_sankey_sidebar <- function(id){

	ns <- NS(id)

	tagList(

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Select the characteristics to include."),
		checkboxGroupInput(ns("demographics_characteristics_sankey_checkbox"), "Deomgraphics:",
			c("Gender" = "Gender",
			  "Age Group" = "Age_Group", 
			  "Season Admission" = "Season_Admission", 
			  "Malignancy" = "Malignancy", 
			  "Transplant" = "Transplant", 
			  "Technology Dependence" = "Technology_Dependence",
			  "MOD Day 1" = "MOD1", 
			  "MOD Day 3" = "MOD3"
			  ),
			selected = c("Gender", "Age_Group", "Season_Admission", "Malignancy", "Transplant", "Technology_Dependence","MOD1", "MOD3"),
			inline = TRUE,
		),
		# this does not need to have anything selected
		checkboxGroupInput(ns("demographics_organs_sankey_checkbox"), "Organs",
			organs,
			selected = "CV",
			inline = TRUE,
		),
		div(
			style = "min-height:20px;",
			textOutput(ns("demographics_characteristics_sankey_error"))
		),

	)
}