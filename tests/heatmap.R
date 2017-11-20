app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("heatmap")

app$setInputs(which_panel = "plots")
app$setInputs(chart_type = "heatmap")
app$uploadFile(infile = "mpg.csv")
# base heatmap
app$setInputs(x = "manufacturer", y = "class", w = "hwy")
app$setInputs(do_plot = "click")
app$snapshot()
# relabel colorbar
app$setInputs(w_label = "low, high")
app$setInputs(do_plot = "click")
app$snapshot()
# change color scheme
app$setInputs(palette_selector = "sequential")
app$setInputs(do_plot = "click")
app$snapshot()
# Relabel color bar
app$setInputs(x_label = "manufacturer", w_guide = "Miles per gallon")
app$setInputs(do_plot = "click")
app$snapshot()
