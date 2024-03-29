app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("xls_upload")

app$setInputs(
  which_panel = "plots",
  chart_type = "scatterplot"
)

app$uploadFile(infile = "mpg.xls")
app$setInputs(x = "displ", y = "hwy", z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot()
