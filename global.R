# version number
version <- numeric_version("0.0.0.9004")

# lengthy copy objects
bar_copy <- list(
	p("for single variable bar charts (i.e.) histograms, select histogram."),
	 br(), 
	 p("for bar charts where your data are already summarized, put the category in x and the summary value in y. put additional categorical variables in z."),
	 br(),
	 p("for bar charts with record level data, put the primary category in x, and the second category in z.")
	)

# set location of zip for export
# Sys.setenv(R_ZIPCMD="/usr/bin/zip")
