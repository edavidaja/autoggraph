app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("labels")
# tests for plot labels
# select plot type and variables, take snapshot
# change labels one by one and snapshot each

app$setInputs(which_panel = "plots", chart_type = "histogram")
app$uploadFile(infile = "mpg.csv")
# histogram tests -------------------------------------------------------------
app$setInputs(x = "displ", do_plot = "click")
app$snapshot()

app$setInputs(x_label = "displacement", do_plot = "click")
app$snapshot()

app$setInputs(z = "cyl", do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "cylinders", do_plot = "click")
app$snapshot()

app$setInputs(chart_type = "density", x_label = "", z_guide = "", z = "", do_plot = "click")
app$snapshot()

# density tests ---------------------------------------------------------------
app$setInputs(x_label = "displacement", do_plot = "click")
app$snapshot()

app$setInputs(z = "cyl", do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "cylinders", do_plot = "click")
app$snapshot()

# bar grpahs ------------------------------------------------------------------
app$setInputs(chart_type = "bar", x_label = "", z_guide = "", z = "", do_plot = "click")
app$snapshot()

app$setInputs(x_label = "displacement", do_plot = "click")
app$snapshot()

app$setInputs(z = "cyl", do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "cylinders", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "count", do_plot = "click")
app$snapshot()

# scatterplots ----------------------------------------------------------------
app$setInputs(chart_type = "scatterplot", x_label = "", y_label = "", z_guide = "", z = "", y = "hwy")
app$setInputs(do_plot = "click")
app$snapshot()

app$setInputs(x_label = "displacement", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "Miles per gallon (hwy)", do_plot = "click")
app$snapshot()

app$setInputs(z = "cyl")
app$setInputs(do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "cylinders", do_plot = "click")
app$snapshot()

app$setInputs(w = "cty", do_plot = "click")
app$snapshot()

app$setInputs(w_guide = "Miles Per Gallon (city)", do_plot = "click")
app$snapshot()

# boxplot ----------------------------------------------------------------
app$setInputs(chart_type = "boxplot", x_label = "", z_guide = "", z = "", y = "hwy", x = "class", w = "", w_guide = "", y_label = "", do_plot = "click")
app$snapshot()

app$setInputs(x_label = "class", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "Miles per gallon (hwy)", do_plot = "click")
app$snapshot()

app$setInputs(z = "cyl", do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "cylinders", do_plot = "click")
app$snapshot()

# heatmaps ----------------------------------------------------------------
app$setInputs(chart_type = "heatmap", x_label = "", y_label = "", z_guide = "", z = "", y = "manufacturer", w = "hwy", palette_selector = "diverging",  do_plot = "click")
app$snapshot()

app$setInputs(x_label = "class", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "manufacturer", do_plot = "click")
app$snapshot()

app$setInputs(w_guide = "Miles per gallon (highway)", do_plot = "click")
app$snapshot()

app$setInputs(w_label = "Low, High", do_plot = "click")
app$snapshot()

# line graphs  ----------------------------------------------------------------
app$uploadFile(infile = "economics.csv")
app$setInputs(chart_type = "line", x = "date", y = "pce", z = "", x_label = "", y_label = "", w_label="", z_guide = "", w_guide="", do_plot = "click")
app$snapshot()

app$setInputs(x_label = "date", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "Personal Consumption Expenditures", do_plot = "click")
app$snapshot()

app$uploadFile(infile = "economics_long.csv")
app$setInputs(x = "date", y = "value", z = "variable", palette_selector = "qualitative", do_plot = "click")
app$snapshot()

app$setInputs(z_guide = "measure", do_plot = "click")
app$snapshot()