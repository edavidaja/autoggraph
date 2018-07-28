valid_filetypes <- c("csv", "xls", "xlsx", "dta", "sas7bdat")

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
		read_xlsx(workbook, worksheet, range = input$cell_range)
	}
}

ingest_xls <- function(workbook = input$infile$datapath,
												worksheet = input$which_sheet,
												range = input$cell_range) {
	req(input$which_sheet)
	if (range == "") {
		read_xls(workbook, worksheet)
	} else {
		validate_range <- try(cellranger::as.cell_limits(input$cell_range))
		validate(
			need(
				str_detect(input$cell_range, "[A-z]+[0-9]+:[A-z]+[0-9]+"),
				"Please enter the range in cell reference format (e.g. A1:Z26)"
				),
			need(validate_range, "Please enter a valid cell range.")
			)
		read_xls(workbook, worksheet, range = input$cell_range)
	}
}
