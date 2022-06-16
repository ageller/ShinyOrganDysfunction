# server module 
# functions used by multiple other modules

shared_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# reset button
			observeEvent(input$resetInputs, {
				reset("AgeGroupCheckbox")
				reset("GenderCheckbox")
				reset("SeasonCheckbox")
				reset("malignancyRadiobutton")
				reset("transplantRadiobutton")
				reset("technologyDependenceRadiobutton")
				reset("MOD1Radiobutton")
				reset("MOD3Radiobutton")

				for (oo in organs){
					reset(paste0(oo, "Radiobutton"))
				}
			})

			# validate the checkboxes
			output$checkboxError <- renderText({
				validate(
					need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
					need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
					need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
				)
			})
		}
	)
}