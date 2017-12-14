app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("xls_upload")

app$setInputs(
  which_panel = "plots",
  chart_type = "scatterplot"
)

app$uploadFile(infile = "mpg.xls")
# for some reason inputs have to be set twice
app$setInputs(x = "displ", y = "hwy", z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot()
