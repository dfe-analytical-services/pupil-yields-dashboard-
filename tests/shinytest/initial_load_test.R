app <- ShinyDriver$new("../../", loadTimeout = 6.e4)
app$snapshotInit("initial_load_test", screenshot = FALSE)

inputs <- c(
  "filter1",
  "filter2",
  "filter3",
  "filter4",
  "navlistPanel",
  "select_breakdown",
  "select_xaxis",
  "select_year",
  "selectArea",
  "selectLA",
  "tabsetpanels"
)

outputs <- c(
  "bar_headlines",
  "headline_title",
  "linePYtime_period"
)

app$snapshot(list(input = inputs, output = outputs))

app$setInputs(navlistPanel = "dashboard")
app$snapshot(list(input = inputs, output = outputs))

app$setInputs(navlistPanel = "dashboard", select_xaxis = "School type")
app$snapshot(list(input = inputs, output = outputs))


app$setInputs(navlistPanel = "Technical")
app$snapshot(list(input = inputs, output = outputs))
