app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("boxplot")

app$setInputs(which_panel = "plots")
app$setInputs(chart_type = "boxplot")
app$uploadFile(infile = "mpg.csv")
# boxplot with flipped axes
app$setInputs(x = "manufacturer", y = "hwy", flip_axes = TRUE)
app$setInputs(do_plot = "click")
app$snapshot()
# sort by y
app$setInputs(reorder_x = "hwy")
app$setInputs(do_plot = "click")
app$snapshot()
# add factor variable
app$setInputs(z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot()
# facets
app$setInputs(wrap = "grid")
app$setInputs(do_plot = "click")
app$snapshot()
