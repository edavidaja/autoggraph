app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("scatterplot")

app$setInputs(which_panel = "plots")
app$setInputs(chart_type = "scatterplot")
app$uploadFile(infile = "mpg.csv")
# two variable scatter
app$setInputs(x = "displ", y = "hwy")
app$setInputs(do_plot = "click")
app$snapshot()
# three variable scatter
app$setInputs(z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot()
# faceted scater
app$setInputs(wrap = "grid")
app$setInputs(do_plot = "click")
app$snapshot()
# cleveland scatter
app$setInputs(wrap = "color", x = "model", z = "", flip_axes = TRUE)
app$setInputs(do_plot = "click")
app$snapshot()
# check that x axis can be sorted 
app$setInputs(reorder_x = "hwy", z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot()
