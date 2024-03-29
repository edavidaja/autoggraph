app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("area")

app$setInputs(which_panel = "plots")
app$setInputs(chart_type = "area")
app$uploadFile(infile = "economics_long.csv")
# Input '`shinyjs-resettable-y_label`' was set, but doesn't have an input binding.
app$setInputs(x = "date", y = "value", z = "variable", palette_selector = "qualitative")
app$setInputs(do_plot = "click")
app$snapshot()

app$setInputs(x_label = "Month", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "Series value", do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "Series name")
app$setInputs(do_plot = "click")
app$snapshot()