app <- ShinyDriver$new("../../", loadTimeout = 6.e4)
app$snapshotInit("initial_load_test", screenshot = FALSE)

app$snapshot()

app$setInputs(navlistPanel = "dashboard")
app$snapshot()

app$setInputs(navlistPanel = "dashboard", select_xaxis = "School type")
app$snapshot()


app$setInputs(navlistPanel = "Technical")
app$snapshot()
