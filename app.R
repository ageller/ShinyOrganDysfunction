# for tabs: https://shiny.rstudio.com/articles/tabsets.html

library(dplyr)
library(tidyr)

library(ggplot2)
library(ggpattern)
library(ggpubr)
#library(plotly)

# start with the binary version
df <- read.csv('data/od_viz_binary.csv')

# add a column to have strings for lived vs. died 
df <- df %>%
	mutate(Outcome = case_when(
		Died == 0 ~ "Lived",
		Died == 1 ~ "Died",
	)) %>%
	mutate(Malignancy = case_when(
		Malignancy == 0 ~ "No",
		Malignancy == 1 ~ "Yes",
	)) %>%
	mutate(Transplant = case_when(
		Transplant == 0 ~ "No",
		Transplant == 1 ~ "Yes",
	)) %>% 
	mutate(Technology_Dependence = case_when(
		Technology_Dependence == 0 ~ "No",
		Technology_Dependence == 1 ~ "Yes",
	)) 

ages = sort(unlist(unique(df['Age_Group']), use.names = FALSE))
outcomes = sort(unlist(unique(df['Outcome']), use.names = FALSE))
genders = sort(unlist(unique(df['Gender']), use.names = FALSE))
seasons = sort(unlist(unique(df['Season_Admission']), use.names = FALSE))

# Define UI 
ui <- fluidPage(

	# App title 
	headerPanel("Pediatric Organ Dysfunction Explorer"),

	# Sidebar panel for inputs 
	sidebarPanel(

		h4("1. Select the subset of the data to include."),
		tabsetPanel(
			tabPanel("Age Group",
				checkboxGroupInput("AgeGroupCheckbox", "",
					ages,
					selected=ages
				),
			),
			tabPanel("Gender",
				checkboxGroupInput("GenderCheckbox", "",
					genders,
					selected=genders
				),
			),
			tabPanel("Season",
				checkboxGroupInput("SeasonCheckbox", "",
					seasons,
					selected=seasons
				),
			),
			tabPanel("Misc.",
				checkboxGroupInput("additionalCheckboxes", "",
					c("Malignancy"="Malignancy",
					  "Transplant"="Transplant",
					  "Technology Dependence"="Technology_Dependence"
					),
					selected=c("Malignancy", "Transplant", "Technology_Dependence")
				),
			),
		),


		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Specify how to aggregate the data."),
		selectInput(
			"div1", "First Aggregation Group", 
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
			"div2", "Second Aggregation Group (optional)", 
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

		hr(style = "border-top: 1px solid #000000;"),
		h4("3. Click the button below to update the plot."),
		actionButton("updatePlot", "Update Plot"),
	),


	# Main panel for displaying outputs 
	mainPanel(

		# Output: Formatted text for debugging (or a caption) 
		#p(textOutput("debug")),
		h4(textOutput("info")),

		# Output: Plot of the requested variable against mpg 
		#plotlyOutput("life_support_bar_plot")
		plotOutput("life_support_bar_plot")
	)
)




# Define server logic 
server <- function(input, output) {

	# for debugging
	debugging <- reactive({
		paste(input$div1, input$div2)
	})
	output$debug <- renderText({
		input$updatePlot
		isolate({
			debugging()
		})
	})

	# update the data
    # data <- reactive({

    #     temp <- subset(iris, Species == input$species)
    #     subset(temp, Sepal.Length < input$sepal_length)

    # })

	# information about the plot
	# 
	text_info <- reactive({
		txt <- paste("Life support type aggregated by",input$div1)
		if (input$div2 != "None") txt <- paste(txt, "and", input$div2)
		txt
	})


	output$info <- renderText({
		input$updatePlot
		isolate({
			text_info()
		})
	})

	# Generate the plot 
	# output$life_support_bar_plot <- renderPlotly(
	# 	generate_bar_plot("Life_Support_Type", c("Mech_Ventilation", "Vasoactives", "NPPV", "ECMO", "CRRT"), input$div1, input$div2, )
	# )
	output$life_support_bar_plot <- renderPlot({
		input$updatePlot
		isolate({
			generate_bar_plot("Life_Support_Type", c("Mech_Ventilation", "Vasoactives", "NPPV", "ECMO", "CRRT"), input$div1, input$div2)
		})
	})






}


select_and_summarize <- function(cols, selections, usedf) {
	# select frome a dataframe and summarize elements for bar chart

	Nsample <- "Nsample"
	Ntotal <- "Ntotal"

	# group the data to get the total number in each subset
	out <- usedf %>% 
		group_by_at(selections) %>% 
		mutate(Nsample = n()) %>%
		group_by_at(append(selections, Nsample)) %>%
		mutate(Ntotal = 0) %>%
		group_by_at(append(selections, c(Nsample, Ntotal))) %>%
		summarise_at(vars(one_of(cols)), sum, na.rm=TRUE)
	
	# get the total number using just div1 (I don't think there's a simple way to do this in the above command)
	div1 <- selections[1]
	div1_values = unlist(unique(out[div1]), use.names = FALSE)
	for (dd in div1_values){
		out[out[div1] == dd,]$Ntotal <- sum(out[out[div1] == dd,]$Nsample)
	}
	
	return(out)

}

calculate_pct_and_sig <- function(cols, selections, usedf) {
	# calculate percentages and errors
	pctdf <- usedf
	sigdf <- usedf

	# divide every column after Ntotal by the Ntotal value to get the percentage
	ncol <- length(selections) + 2
	pctdf[, -(1:ncol)] <- sweep(100.*pctdf[, -(1:ncol)], 1, pctdf$Ntotal, "/")

	# calculate the error using error propagation
	for (cc in cols){
		num <- usedf[cc]
		den <- usedf$Ntotal
		sigdf[cc] <- 100.*( num/den**2. + (num/den**2.)**2.*den )**0.5
		# assuming no error on denominator
		#sigdf1[cc] <- 100.*num**0.5/den
	}


	return(list("pct" = pctdf, "sig" = sigdf))
}

cbind_and_pivot <- function(plot_type, selections, psdf){

	# reorder this table so that it is easier to use with ggplot

	pctdf <- psdf$pct
	sigdf <- psdf$sig
	
	out <- pctdf
	out <- pivot_longer(data = out,
	   cols = -append(selections, c("Nsample", "Ntotal")),
	   names_to = plot_type,
	   values_to = "percent")


	tmpdf <- pivot_longer(data = sigdf,
	   cols = -append(selections, c("Nsample", "Ntotal")),
	   names_to = plot_type,
	   values_to = "sig_percent")

	out <- cbind(out, sig_percent=tmpdf$sig_percent)

	# select the columns we want
	out <- out[, append(selections, c(plot_type, "percent", "sig_percent"))]
	
	return(out)

}

set_fill_patterns <- function(col, usedf){
	#patterns <- c("none", "stripe", "crosshatch", "circle", "image", "placeholder", "magick", "gradient", "plasma")
	patterns <- c("stripe", "none", "crosshatch", "circle", "image", "placeholder", "magick", "gradient", "plasma")

	col_vals <- unlist(unique(usedf[col]), use.names = FALSE)
	col_patterns <- patterns[(1:length(col_vals))]
	names(col_patterns) <- col_vals
	
	return(col_patterns)
}

prep_bar_chart_data <- function(plot_type, cols, div1, div2, usedf){

	############################################
	# percentage within each group given the selections
	selections <- c(div1)
	if (div2 != "None") selections <- append(selections, div2)

	df1 <- select_and_summarize(cols, selections,usedf)
	psdf <- calculate_pct_and_sig(cols, selections, df1)
	outdf <- cbind_and_pivot(plot_type, selections, psdf)

	############################################
	# mortality percentage given the selections
	if (div1 != "Outcome" && div2 != "Outcome") selections <- append(selections, "Outcome")
	df1m <- select_and_summarize(cols, selections,usedf)
	died <- df1m[df1m$Outcome == "Died",]
	psdfm <- calculate_pct_and_sig(cols, selections, died)
	outdfm <- cbind_and_pivot(plot_type, selections, psdfm)


	############################################
	# set the patterns
	div2_patterns <- c("None" = "none")
	if (div2 != "None") div2_patterns <- set_fill_patterns(div2, outdf)
	
	return(list("df" = outdf, "dfm" = outdfm, "patterns" = div2_patterns))

}

generate_bar_plot <- function(plot_type, cols, div1, div2){

	bardf = prep_bar_chart_data(plot_type, cols, div1, div2, df)
	usedf1 = bardf$df
	usedf1m = bardf$dfm
	div2_patterns = bardf$patterns

	# create the plot

	#https://stackoverflow.com/questions/62393159/how-can-i-add-hatches-stripes-or-another-pattern-or-texture-to-a-barplot-in-ggp
	#WHY DO I USE position_dodge(0.9) IN THE ERROBAR PLOT??

	############################################
	# top panel (f1) shows percent in each group
	# bottom panel (f1m) shows mortality percent in each group

	# I don't think there's a clean way to do this without an if statement
	if (div2 == "None"){
		f1 <- ggplot(usedf1, aes_string(fill=div1,  y="percent", x=plot_type)) + 
			geom_bar(stat = "identity", position = "dodge", color="black") + 
			labs(x = plot_type, y = "Percentage")

		f1m <- ggplot(usedf1m, aes_string(fill=div1,  y="percent", x=plot_type)) + 
			geom_bar(stat = "identity", position = "dodge", color="black") + 
			labs(x = plot_type, y = "Mortality Percentage")
		
	} else {
		f1 <- ggplot(usedf1, aes_string(fill=div1, pattern=div2, y="percent", x=plot_type)) + 
			geom_bar_pattern(stat = "identity", position = "dodge",
						   color = "black", 
						   pattern_fill = "black",
						   pattern_angle = 45,
						   pattern_density = 0.1,
						   pattern_spacing = 0.025,
						   pattern_key_scale_factor = 0.6) +
			scale_pattern_manual(values = div2_patterns) +
			labs(x = plot_type, y = "Overall Percentage", pattern = div2) + 
			guides(pattern = guide_legend(override.aes = list(fill = "white")),
				  fill = guide_legend(override.aes = list(pattern = "none")))
		
		f1m <- ggplot(usedf1m, aes_string(fill=div1, pattern=div2, y="percent", x=plot_type)) + 
			geom_bar_pattern(stat = "identity", position = "dodge",
						   color = "black", 
						   pattern_fill = "black",
						   pattern_angle = 45,
						   pattern_density = 0.1,
						   pattern_spacing = 0.025,
						   pattern_key_scale_factor = 0.6) +
			scale_pattern_manual(values = div2_patterns) +
			labs(x = plot_type, y = "Mortality Percentage", pattern = div2) + 
			guides(pattern = guide_legend(override.aes = list(fill = "white")),
				  fill = guide_legend(override.aes = list(pattern = "none")))
	}

	f1 <- f1 +  
		scale_fill_brewer(palette = "Blues") +
		geom_errorbar(aes(ymin = percent - sig_percent, ymax = percent + sig_percent), width=.2, position=position_dodge(.9)) +
		coord_cartesian(ylim = c(0, 1.1*max((usedf1$percent + usedf1$sig_percent))), expand = FALSE) + 
		theme_bw()

	f1m <- f1m +  
		scale_fill_brewer(palette = "Blues") +
		geom_errorbar(aes(ymin = percent - sig_percent, ymax = percent + sig_percent), width=.2, position=position_dodge(.9)) +
		coord_cartesian(ylim = c(0, 1.1*max((usedf1m$percent + usedf1m$sig_percent))), expand = FALSE) + 
		theme_bw()


	ggarrange(f1, f1m, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
	# p <- ggarrange(f1, f1m, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
	# ggplotly(p, tooltip = "text")

}

shinyApp(ui, server)