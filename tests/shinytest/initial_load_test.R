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
  "headline_title",
  "table_headlines",
  "bar_headlines",
  "headlines_caption",
  "timeseries_title",
  "table_timeseries",
  "linePYtime_period",
  "timeseries_caption",
  "postcomtab_toggle",
  "pc_caption",
  "education.phase",
  "time.period",
  "send_title",
  "send_box_1",
  "send_box_3",
  "technicaltable",
  "line_PC"
)

message("Initial snapshot")
app$snapshot(list(input = inputs, output = outputs))

message("Default dashboard snapshot (i.e. headlines)")
app$setInputs(navlistPanel = "dashboard")
app$snapshot(list(input = inputs, output = outputs))

message("Set x-axis to tenure")
app$setInputs(select_xaxis = "Tenure")
Sys.sleep(2.0)
app$snapshot(list(input = inputs, output = outputs))

message("Set x-axis to number of bedrooms")
app$setInputs(select_xaxis = "Number of bedrooms")
Sys.sleep(2.0)
app$snapshot(list(input = inputs, output = outputs))

message("Set breakdown to housing type")
app$setInputs(select_breakdown = "Housing type")
Sys.sleep(2.0)
app$snapshot(list(input = inputs, output = outputs))

message("Set breakdown to school type")
app$setInputs(select_breakdown = "School type")
Sys.sleep(2.0)
app$snapshot(list(input = inputs, output = outputs))

message("Switch to time series tab")
app$setInputs(tabsetpanels = "Averages")
app$snapshot(list(input = inputs, output = outputs))

message("Switch to post-completion tab")
app$setInputs(tabsetpanels = "post-completion")
app$snapshot(list(input = inputs, output = outputs))

message("Switch to SEND tab")
app$setInputs(tabsetpanels = "SEND")
app$snapshot(list(input = inputs, output = outputs))

app$setInputs(navlistPanel = "technical")
app$snapshot(list(input = inputs, output = outputs))
