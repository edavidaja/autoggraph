app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("violin")

app$setInputs(which_panel = "plots", chart_type = "violin")
app$uploadFile(infile = "mpg.csv")
# Input '`shinyjs-resettable-y_label`' was set, but doesn't have an input binding.
app$setInputs(x = "drv", y = "displ")
app$setInputs(do_plot = "click")
app$snapshot()

app$setInputs(x_label = "Drive", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "Displacement", do_plot = "click")
app$snapshot()

app$setInputs(z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "cylinders", do_plot = "click")
app$snapshot()
