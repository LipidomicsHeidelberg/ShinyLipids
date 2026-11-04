context("Plots")

input <- defaultInput()

path <- system.file("extdata/exampleDatabase.db", package = "ShinyLipids")
databaseConnection <- DBI::dbConnect(RSQLite::SQLite(), path)
rawData <- collectRawData(con = databaseConnection, id = 1)

plotData <- rawData %>%
  imputeMissingIf(input) %>% 
  addLipidProperties() %>% 
  standardizeWithinTechnicalReplicatesIf(input) %>%
  filterRawDataFor(input) %>%
  standardizeWithin(input) %>%
  createPlotData(input)

allPlotOptions <- list(
  "Show points"                = "points",
  "Show bars"                  = "bars",
  "Show average"               = "mean",
  "Show value of means"        = "values",
  "Show value of points"       = "ind_values",
  "Transform y-axis log1p"     = "log",
  "Show N per sample"          = "N",
  "Label points"               = "label",
  # "Swap x- and y-axis"         = "swap",
  "Free y scale for facets"    = "free_y",
  "Run pairwise t-tests"       = "signif")

test_that("Plot with all options except swapped x and y work", {
  tmpInput <- input
  tmpInput$mainPlotAdditionalOptions <- allPlotOptions
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = input)
  expect_is(plt, "ggplot")
})

test_that("Main Plot works with facets", {
  tmpInput <- input
  tmpInput$aesFacetCol <- "category"
  tmpInput$errorbarType <- "SD"
  
  data <- rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>%
    standardizeWithin(tmpInput)

  plotData <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")

  plotData <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  tmpInput$aesFacetCol <- "category"
  tmpInput$aesFacetRow <- "oh"
  tmpInput$mainPlotAdditionalOptions <- list(
    "points", "bars", "swap"
  )

  plotData <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput)
  
  plt <- createMainPlot(plotData = plotData,
                        meanPlotData = summarisePlotData(plotData, input = tmpInput),
                        pairwiseComparisons = doAllPairwiseComparisons(plotData, tmpInput),
                        input = tmpInput)
  expect_is(plt, "ggplot")
})

test_that("Heatmap works with defaults", {
  plt <- createHeatmap(plotData = plotData, input)
  expect_is(plt, "ggplot")
})


test_that("Heatmap works with facets", {
  tmpInput <- input
  data <- rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>%
    standardizeWithin(tmpInput) 
  
  plt <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  tmpInput$aesFacetCol <- "category"
  data <- rawData %>%
    imputeMissingIf(tmpInput) %>% 
    addLipidProperties() %>% 
    standardizeWithinTechnicalReplicatesIf(tmpInput) %>%
    filterRawDataFor(tmpInput) %>%
    standardizeWithin(tmpInput) 
  
  plt <- data %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  input$aesFacetRow <- "category"
  
  plt <- data %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
  
  tmpInput <- input
  input$aesFacetRow <- "category"
  input$aesFacetCol <- "func_cat"
  
  plt <- data %>%
    createPlotData(tmpInput) %>% 
    createHeatmap(tmpInput)
  expect_is(plt, "ggplot")
})

# TODO
test_that("Default PCA works", {
  
  wideData <- createWideData(plotData = plotData, input)
  pcaPrep <- createPcaPrep(wideData, input)
  
  pcaTidy <- tidy(pcaPrep, id = "pca")
  pcaJuice <- juice(pcaPrep)
  
  plt1 <- createPcaScoresPlot(pcaJuice, pcaTidy, input)
  plt2 <- createPcaLoadingsPlot(pcaTidy, input)
  
  expect_is(plt1, "ggplot")
  expect_is(plt2, "ggplot")
})

# Regression tests for the Features -> Plot combinations ----------------------

mainPlotFor <- function(...) {
  tmpInput <- modifyList(input, list(...))
  pd <- rawData %>%
    imputeMissingIf(tmpInput) %>%
    addLipidProperties() %>%
    filterRawDataFor(tmpInput) %>%
    standardizeWithin(tmpInput) %>%
    createPlotData(tmpInput)
  mpd <- summarisePlotData(pd, tmpInput)
  allClass <- if (tmpInput$stackMode == "stack_percent_all") mpd else NULL
  plt <- createMainPlot(pd, mpd, NULL, tmpInput, allClassMeanPlotData = allClass)
  list(plt = plt, built = ggplot_build(plt), plotData = pd, meanPlotData = mpd)
}

layerOf <- function(plt, geom) {
  which(purrr::map_lgl(plt$layers, ~ inherits(.x$geom, geom)))[1]
}

test_that("SEM is SD / sqrt(N)", {
  mpd <- summarisePlotData(plotData, input)
  ok <- mpd$N > 1
  expect_equal(mpd$SEM[ok], mpd$SD[ok] / sqrt(mpd$N[ok]))
  expect_equal(mpd$CI_upper[ok] - mpd$value[ok],
               qt(0.975, mpd$N[ok] - 1) * mpd$SEM[ok])
})

test_that("Side by side proportions sum to 100 and points match their bars", {
  res <- mainPlotFor(stackMode = "sbs_percent", aesFacetCol = "class")
  bars <- res$plt$layers[[layerOf(res$plt, "GeomCol")]]$data
  sums <- bars %>% group_by(class) %>% summarise(s = sum(value))
  expect_equal(sums$s, rep(100, nrow(sums)))
  pointMeans <- res$plt$data %>%
    group_by(class, sample) %>%
    summarise(m = mean(value), .groups = "drop") %>%
    inner_join(bars, by = c("class", "sample"))
  expect_equal(pointMeans$m, pointMeans$value)
})

test_that("Proportion of total sums to 100 per sample", {
  res <- mainPlotFor(stackMode = "stack_percent_all")
  bars <- res$plt$layers[[layerOf(res$plt, "GeomCol")]]$data
  sums <- bars %>% group_by(sample) %>% summarise(s = sum(value))
  expect_equal(sums$s, rep(100, nrow(sums)))
})

test_that("Stacked proportions are labelled up to 100%", {
  res <- mainPlotFor(stackMode = "stack_percent", aesX = "sample", aesColor = "class")
  labels <- res$built$layout$panel_params[[1]]$y$get_labels()
  expect_true("100%" %in% labels)
})

test_that("Duplicated features in facets and groups don't error", {
  expect_is(mainPlotFor(aesFacetCol = "category", aesFacetRow = "category")$plt, "ggplot")
  expect_is(mainPlotFor(aesFacetCol = "class", stackMode = "sbs_percent")$plt, "ggplot")
  expect_is(mainPlotFor(aesX = "db", aesColor = "db", aesFacetCol = "db")$plt, "ggplot")
})

test_that("Swapped axes work with facets, log scale and zoom", {
  res <- mainPlotFor(aesFacetCol = "category", errorbarType = "SD",
                     mainPlotAdditionalOptions = c("points", "bars", "swap", "log"))
  expect_is(res$plt$coordinates, "CoordFlip")
  expect_is(ggplotGrob(res$plt), "gtable")
})

test_that("Proportions are refused after baseline subtraction", {
  expect_error(mainPlotFor(stackMode = "sbs_percent", baselineSample = "1 - 0min Stim"),
               "baseline")
})

test_that("Pairwise comparisons return an empty table when no test is possible", {
  tmpInput <- input
  tmpInput$aesX <- "sample"
  res <- doAllPairwiseComparisons(plotData, tmpInput)
  expect_true(all(c("group1", "group2", "p.value") %in% names(res)))
  expect_equal(nrow(res), 0)
})

test_that("Heatmap handles a missing color and duplicated facets", {
  res <- mainPlotFor(aesFacetCol = "category", aesFacetRow = "category")
  tmpInput <- modifyList(input, list(aesFacetCol = "category", aesFacetRow = "category"))
  expect_is(ggplot_build(createHeatmap(res$meanPlotData, tmpInput)), "ggplot_built")
  tmpInput$aesColor <- ""
  expect_error(createHeatmap(plotData, tmpInput), "select a feature")
})
