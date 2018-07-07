app <- ShinyDriver$new("../", seed = 7038)
app$snapshotInit("line-step")

app$setInputs(which_panel = "plots")
app$setInputs(chart_type = "line")
app$uploadFile(infile = "economics.csv")
# single variable line plot
app$setInputs(x = "date", y = "uempmed")
app$setInputs(do_plot = "click")
app$snapshot()

app$setInputs(x_label = "Month", do_plot = "click")
app$snapshot()

app$setInputs(y_label = "Personal Consumption Expenditures", do_plot = "click")
app$snapshot()

app$uploadFile(infile = "economics_long.csv")
# multi-variable line plot
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


# facets with free y axes
app$setInputs(wrap = "grid", free_facet = "free_y")
app$setInputs(do_plot = "click")
app$snapshot()

# facet-relabling works
app$setInputs(z_label = "one, two, three, four, five")
app$setInputs(do_plot = "click")
app$snapshot()
