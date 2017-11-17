app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("data_upload")

app$setInputs(
  which_panel = "plots",
  chart_type = "scatterplot"
  )
app$uploadFile(infile = "iris_dat.csv")
# Input '`shinyjs-resettable-y_label`' was set, but doesn't have an input binding.
app$snapshot(items = list(input = TRUE, output = "graph"))
app$uploadFile(infile = "iris_dat.xlsx")
app$snapshot(items = list(input = TRUE, output = "graph"))
