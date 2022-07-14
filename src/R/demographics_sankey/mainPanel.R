# Main panel for displaying outputs  
# for the demographics sankey figure 

demographics_sankey_main <- function(id){

	ns <- NS(id)

	tagList(
		# text describing plot 
		h3("Demographics"),

		div(
			style = "position:relative",
			sankeyNetworkOutput(ns("demographics_sankey_plot"))
		)

	)
}
