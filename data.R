valid_filetypes <- c("csv", "xls", "xlsx", "dta", "sas7bdat", "txt", "tsv")

ingest_xlsx <- function(workbook = input$infile$datapath,
												worksheet = input$which_sheet,
												range = input$cell_range) {
	req(input$which_sheet)
	if (range == "") {
		read_xlsx(workbook, worksheet)
	} else {
		validate_range <- try(cellranger::as.cell_limits(input$cell_range))
		validate(
			need(
				str_detect(input$cell_range, "[A-z]+[0-9]+:[A-z]+[0-9]+"),
				"Please enter the range in cell reference format (e.g. A1:Z26)"
				),
			need(validate_range, "Please enter a valid cell range.")
			)
		read_xlsx(workbook, worksheet, range = range)
	}
}

ingest_xls <- function(workbook = input$infile$datapath,
											 worksheet = input$which_sheet,
											 range = input$cell_range) {
	req(input$which_sheet)
	if (range == "") {
		read_xls(workbook, worksheet)
	} else {
		# these validate statements to not push through to the table 
		# but should prevent invalid cell ranges from being specified
		validate_range <- try(cellranger::as.cell_limits(input$cell_range))
		validate(
			need(
				str_detect(input$cell_range, "[A-z]+[0-9]+:[A-z]+[0-9]+"),
				"Please enter the range in cell reference format (e.g. A1:Z26)"
				),
			need(validate_range, "Please enter a valid cell range.")
			)
		read_xls(workbook, worksheet, range = range)
	}
}

ingest_delim <- function(file =  input$infile$datapath,
												 delim = NULL) {
	req(input$which_delim)
	print("ingest fired")
	read_delim(file, delim = delim)
}