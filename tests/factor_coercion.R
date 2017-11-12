app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("factor_coercion")

app$setInputs(which_panel = "plots", chart_type = "scatterplot")
app$uploadFile(infile = "mpg.csv")
# Input '`shinyjs-resettable-y_label`' was set, but doesn't have an input binding.
app$setInputs(x = "displ", y = "hwy", z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot(items = list(input = TRUE, output = "graph"))
