app_server <- function(input, output, session) {
  # Share link mode ####
  # Read URL parameters once at session start
  .query         <- isolate(parseQueryString(session$clientData$url_search))
  .share_mode    <- isTRUE(.query[["shared"]] == "1")
  .share_dataset <- if (!is.null(.query[["dataset"]])) as.integer(.query[["dataset"]]) else NULL

  if (.share_mode && !is.null(.share_dataset)) {
    # Auto-select the dataset once metadata is loaded
    observe({
      choices <- metaData()$id
      names(choices) <- paste(metaData()$id, "-", metaData()$title)
      updateSelectInput(session, "ID", choices = choices, selected = .share_dataset)
    })
    # Hide the dataset selector
    shinyjs::hide("ID")
    # Hide the Tables sidebar menu item and its tab body
    shinyjs::runjs("$('[data-value=\"metadata\"]').closest('li').hide();")
    shinyjs::runjs("$('#shiny-tab-metadata').hide();")
    # Land on the main plot tab instead of Tables
    shinydashboard::updateTabItems(session, "tab", selected = "main")
  }

  # Reconnect: show banner instead of silently looping ####
  session$allowReconnect(TRUE)

  # Setup ####
  databaseConnection <- golem::get_golem_options("db")
  
  # Data ####
  # * Metadata / list of data sets ####
  metaData <- reactive({
    collectMetaData(databaseConnection)
  })
  
  # * rawData ####
  rawData <- reactive({
    validate(need(input$ID, "Please select a dataset first."))
    collectRawData(id = input$ID, con = databaseConnection) %>% 
      imputeMissingIf(input) %>% 
      addLipidProperties(lipidClassOrder = collectLipidClassOrder(databaseConnection))
  })
  
  # * filteredData ####
  filteredData <- reactive({
    rawData() %>%
      filterRawDataFor(input)
  })
  
  # * mainData ####
  mainData <- reactive({
    validate(need(nrow(filteredData()) > 0,
                  "The data was filtered such that there is no data left."))
    filteredData() %>%
      standardizeWithin(input)
  })
  
  # * plotData ####
  # summarized based on
  # selected plot type, standards and aes (aesthetics)
  plotData <- reactive({
    req(mainData())
    validate(
      need(
        !(input$summariseTechnicalReplicates &
            (input$aesColor    == "sample_replicate_technical" |
               input$aesX      == "sample_replicate_technical" |
               input$aesFacetCol == "sample_replicate_technical" |
               input$aesFacetRow == "sample_replicate_technical")
        ),
        "You are currently averaging over technical replicates (see the samples tab in the sidebar)
                  and thus can't use this feature in your plots."
      ),
      need(
        input$aesX != "",
        "Please select a feature to display on the x-axis"))
    
    mainData() %>%
      createPlotData(input)
  })
  
  # * meanPlotData ####
  ## for bars/averages
  meanPlotData <- reactive({
    req(plotData())
    plotData() %>%
      summarisePlotData(input)
  })
  
  # * PairwiseComparisons ####
  # from pairwise t-tests on log-transformed data
  pairwiseComparisons <- reactive({
    validate(
      need(
        input$aesColor == "sample",
        "To compare between samples, chose sample as the color"
      ),
      need(
        input$aesX != "sample",
        "To compare between samples, chose a feature other than sample for the x-axis"
      )
    )
    validate(
      need(
        length(unique(plotData()$sample)) > 1,
        "You need at least 2 samples to compare them"
      ),
      need(
        testAllMoreThanOneReplicate(plotData(), input$aesX, input$aesColor),
        "You need more than 1 replicate per sample for everything visible in the plot"))
    
    doAllPairwiseComparisons(data = plotData(), input)
  })
  
  
  # Updating input choices ####
  # * Update selectInput for data sets
  # based on sets loaded and row selected select clicked row in table
  
  # Help modal: full Data tab
  observeEvent(input$help_data_tab, {
    showModal(modalDialog(
      title = "Data tab — what does each option do?",
      size  = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <p>For standardization: <code>value / sum(group) × 100</code>.
        The Y-axis switches from µM to Mol %.
        <em>Visible</em> means only lipids passing the current filters.</p>

        <p><strong>Selecting multiple options:</strong>
        The grouping becomes the intersection of all selected features — the denominator
        is the sum within each unique combination. Finer groups → smaller denominator → larger percentages.
        Example: <em>Sample replicate</em> + <em>Class</em> → each species as % of its class within each replicate.
        Selecting a more specific and a less specific feature (e.g. <em>Sample</em> + <em>Sample replicate</em>)
        is equivalent to selecting only the more specific one.</p>

        <table class='table table-bordered table-condensed' style='font-size:13px;'>
          <thead><tr>
            <th>Option</th><th>Formula</th><th>What sums to 100% / Effect</th>
          </tr></thead>
          <tbody>
            <tr><th colspan='3' style='background:#f5f5f5;'>Standardize to 100% within (dropdown)</th></tr>
            <tr><td><em>(nothing selected)</em></td>
                <td>—</td>
                <td>No standardization — Y-axis stays in µM</td></tr>
            <tr><td>Sample</td>
                <td><code>value / sum(sample) × 100</code></td>
                <td>All visible lipids per sample (replicates pooled)</td></tr>
            <tr><td>Sample replicate</td>
                <td><code>value / sum(replicate) × 100</code></td>
                <td>All visible lipids per biological replicate, each normalized independently</td></tr>
            <tr><td>Class</td>
                <td><code>value / sum(class, measurement) × 100</code></td>
                <td>All visible species per class per measurement</td></tr>
            <tr><td>Lipid species</td>
                <td><code>value / sum(species) × 100</code></td>
                <td>All occurrences of one species across the visible data</td></tr>
            <tr><td>Category</td>
                <td><code>value / sum(category, measurement) × 100</code></td>
                <td>All visible species per category per measurement</td></tr>
            <tr><td>Functional category</td>
                <td><code>value / sum(func_cat, measurement) × 100</code></td>
                <td>All visible species per functional category per measurement</td></tr>
            <tr><td>Double bonds</td>
                <td><code>value / sum(db, measurement) × 100</code></td>
                <td>All visible species per double bond count per measurement</td></tr>
            <tr><td>Hydroxylation state</td>
                <td><code>value / sum(oh, measurement) × 100</code></td>
                <td>All visible species per hydroxylation state per measurement</td></tr>
            <tr><td>Chain Length</td>
                <td><code>value / sum(length, measurement) × 100</code></td>
                <td>All visible species per chain length per measurement</td></tr>
            <tr><td>Chains</td>
                <td><code>value / sum(chains, measurement) × 100</code></td>
                <td>All visible species per chain composition per measurement</td></tr>
            <tr><td>Chain sums</td>
                <td><code>value / sum(chain_sums, measurement) × 100</code></td>
                <td>All visible species per chain sums per measurement</td></tr>

            <tr><th colspan='3' style='background:#f5f5f5;'>Checkboxes</th></tr>
            <tr><td>Standardize to 100% within technical replicates</td>
                <td><code>value / sum(technical replicate) × 100</code></td>
                <td>All visible lipids per technical replicate, applied before averaging technical replicates</td></tr>
            <tr><td>Average technical replicates</td>
                <td><code>mean(technical replicates)</code></td>
                <td>Technical replicates are averaged into one value per biological replicate before plotting</td></tr>
            <tr><td>Impute missing values as 0</td>
                <td><code>NA → 0</code></td>
                <td>Lipids not detected in a sample are set to 0 µM instead of being omitted. Affects means and standardization denominators.</td></tr>
          </tbody>
        </table>
      ")
    ))
  })

  # Help modal: full Features -> Plot tab
  observeEvent(input$help_features_tab, {
    showModal(modalDialog(
      title = "Features → Plot tab — what does each option do?",
      size  = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <p>These options decide <em>how the data is grouped and drawn</em>, for the main plot,
        the heatmap and the PCA. Each <strong>point</strong> is one replicate (biological, or technical
        if <em>Average technical replicates</em> is off). Its value is the <strong>sum of all visible
        lipids</strong> that share the selected features. Each <strong>bar</strong> is the mean of its points.
        <em>Visible</em> means only lipids passing the current filters.</p>

        <table class='table table-bordered table-condensed' style='font-size:13px;'>
          <thead><tr>
            <th>Option</th><th>What it does</th><th>Notes</th>
          </tr></thead>
          <tbody>
            <tr><th colspan='3' style='background:#f5f5f5;'>Mappings (dropdowns)</th></tr>
            <tr><td>Feature to display on x-axis</td>
                <td>One group of bars per value of this feature</td>
                <td>Required. In the PCA, the values of this feature become the dimensions
                    (it can't be sample there).</td></tr>
            <tr><td>Feature to display by color</td>
                <td>Splits each x position into one bar per value, shown side by side or stacked</td>
                <td>Empty = one bar per x position. Needed for stacked bars, and used as the y-axis of the heatmap.
                    Pairwise t-tests and the PCA need <em>Sample</em> here.</td></tr>
            <tr><td>Facetting in columns</td>
                <td>Splits the plot into side-by-side panels, one per value</td>
                <td>Each panel only shows the x values present in it.</td></tr>
            <tr><td>Facetting in rows</td>
                <td>Splits the plot into stacked panels, one per value</td>
                <td>Choosing the same feature for rows and columns facets it once.
                    The PCA needs both facet fields empty.</td></tr>

            <tr><th colspan='3' style='background:#f5f5f5;'>Features</th></tr>
            <tr><td>Sample</td>
                <td>The sample group. Its replicates are the points.</td>
                <td>For t-tests, use it as color, not on the x-axis.</td></tr>
            <tr><td>Class</td>
                <td>Lipid class, e.g. <code>PC</code>, <code>PE O-</code></td>
                <td>Taken from the species name</td></tr>
            <tr><td>Lipid species</td>
                <td>The individual lipid, e.g. <code>PC 34:1</code></td>
                <td>x-axis only</td></tr>
            <tr><td>Category</td>
                <td>Lipid category from the database, e.g. <code>GP</code>, <code>SP</code></td>
                <td></td></tr>
            <tr><td>Functional category</td>
                <td>Functional category from the database</td>
                <td></td></tr>
            <tr><td>Double bonds</td>
                <td>Total number of double bonds over all chains</td>
                <td></td></tr>
            <tr><td>Hydroxylation state</td>
                <td>Total number of hydroxyl groups (the number after <code>;</code>)</td>
                <td></td></tr>
            <tr><td>Chain Length</td>
                <td>Total number of carbon atoms over all chains</td>
                <td>x-axis only</td></tr>
            <tr><td>Chains</td>
                <td>Chain composition as written, e.g. <code>16:0/18:1</code></td>
                <td>x-axis only</td></tr>
            <tr><td>Chain sums</td>
                <td>Summed length:double bonds;OH, e.g. <code>34:1</code></td>
                <td>x-axis only</td></tr>

            <tr><th colspan='3' style='background:#f5f5f5;'>Bar stacking</th></tr>
            <tr><td>Side by side by amount</td>
                <td>Bars next to each other, height = mean amount</td>
                <td>Default. Y-axis in µM (or Mol % if standardized on the Data tab).</td></tr>
            <tr><td>Stack bars by amount</td>
                <td>Bars stacked on top of each other at each x position</td>
                <td>Needs a color feature. Points, error bars, means, labels and log scale are not shown.</td></tr>
            <tr><td>Side by side by proportion within group</td>
                <td><code>bar / sum(bars at the same x, in the same panel) × 100</code></td>
                <td>The bars at each x position add up to 100%. Points and error bars are scaled by the same factor.</td></tr>
            <tr><td>Stack bars by proportion within group</td>
                <td>Like the one above, but stacked: every x position fills 0–100%</td>
                <td>Needs a color feature. Points, error bars, means, labels and log scale are not shown.</td></tr>
            <tr><td>Show proportion of total (all classes)</td>
                <td><code>bar / sum(all lipids of all classes) × 100</code>, shown side by side</td>
                <td>The total ignores <em>Select / Remove classes</em> but keeps all other filters.
                    It is taken per sample (or replicate) if sample is used for x, color or facets,
                    otherwise over everything.</td></tr>

            <tr><th colspan='3' style='background:#f5f5f5;'>Good to know</th></tr>
            <tr><td colspan='3'>The three proportion modes are not available while a baseline sample is subtracted
                (Data tab), because the values then sum to about zero.</td></tr>
            <tr><td colspan='3'><em>Swap x- and y-axis</em> (plot options) also swaps the facets,
                so column panels become row panels.</td></tr>
          </tbody>
        </table>
      ")
    ))
  })

  # Reset button, using shinyjs
  observeEvent(input$resetEverything, {
    names(defaultInput()) %>% 
      c("quickSpeciesProfileClass") %>%
      walk(shinyjs::reset)
  })
  
  observe({
    choices <- metaData()$id
    names(choices) <- paste(metaData()$id, "-", metaData()$title)
    selection <- input$metaDataTable_rows_selected
    if (!is.null(selection)) {
      updateSelectInput(session,
                        "ID",
                        choices  = choices,
                        selected = choices[[selection]])
    }
  })
  
  # * Updating filtering options by dataset ####
  observe({
    updateAllSelectizeInputs <- partial(updateAllSelectizeInputs,
                                        data = rawData(),
                                        session = session)
    updateAllSelectizeInputs("samplesToSelect", "sample", NULL)
    updateAllSelectizeInputs("baselineSample", "sample", "")
    updateAllSelectizeInputs("samplesToRemove", "sample", NULL)
    updateAllSelectizeInputs("replicatesToSelect", "sample_replicate", NULL)
    updateAllSelectizeInputs("replicatesToRemove", "sample_replicate", NULL)
    updateAllSelectizeInputs("technicalReplicatesToRemove", "sample_replicate_technical", NULL)
    updateAllSelectizeInputs("categoryToSelect", "category", NULL)
    updateAllSelectizeInputs("functionalCategoryToSelect", "func_cat", NULL)
    updateAllSelectizeInputs("lipidClassToSelect", "class", NULL)
    updateAllSelectizeInputs("lipidClassToRemove", "class", NULL)
    updateAllSelectizeInputs("quickSpeciesProfileClass", "class", "")
    
    updateAllRangeInputs <- partial(updateAllRangeInputs,
                                    data = rawData(),
                                    session = session)
    updateAllRangeInputs("filterLengthRange", "length")
    updateAllRangeInputs("filterDoubleBondsRange", "db")
    updateAllRangeInputs("filterOhRange", "oh")
  })
  
  observe({
    if (is.null(input$samplesToSelect)) {
      updateSelectizeInput(session,
                           "samplesToRemove",
                           choices = levels(rawData()$sample)
      )
    }
    if (!is.null(input$samplesToSelect)) {
      updateSelectizeInput(session,
                           "samplesToRemove",
                           choices = unname(input$samplesToSelect)
      )
    }
  })
  
  # * Update inputs based on selected default quickoption ####
  observeEvent(input$quickSpeciesProfileClass, {
    if (input$quickSpeciesProfileClass != "") {
      shinyjs::reset("aesColor")
      shinyjs::reset("aesFacetRow")
      shinyjs::reset("aesFacetCol")
      shinyjs::reset("categoryToSelect")
      shinyjs::reset("functionalCategoryToSelect")
      shinyjs::reset("lipidClassToRemove")

      updateSelectInput(session, "aesX", selected = "lipid")
      updateSelectizeInput(session, "standardizationFeatures",
                           selected = c("class", "sample_replicate"))
      updateSelectizeInput(session, "lipidClassToSelect",
                           selected = unname(input$quickSpeciesProfileClass))
    }
  })
  
  observeEvent(input$quickClassProfile, {
    shinyjs::reset("aesColor")
    shinyjs::reset("aesFacetRow")
    shinyjs::reset("aesFacetCol")
    shinyjs::reset("technicalReplicatesToRemove")
    shinyjs::reset("standardizationFeatures")
    shinyjs::reset("categoryToSelect")
    shinyjs::reset("lipidClassToSelect")
    shinyjs::reset("lipidClassToRemove")
    shinyjs::reset("functionalCategoryToSelect")

    updateSelectInput(session, "aesX", selected = "class")
  })
  
  # Table Outputs ####
  # * Metadata table ####
  output$metaDataTable <- DT::renderDT({
    validate(
      need(req(metaData()), "No metadata loaded")
    )
    if (input$showFullMeta == TRUE) {
      metaData()
    } else {
      metaData()[c("id", "title", "date_upload", "status", "sample_from")]
    }
  },
  server = FALSE, selection = list(mode = "single", selected = 1),
  options = dtOptions
  )
  
  # * Main Dataset as a table ####
  output$mainDataTable <- DT::renderDT({
    mainData()
  },
  filter = "none",
  rownames = FALSE,
  options  = dtOptions
  )
  
  # * meanPlotDataTable ####
  output$meanPlotDataTable <- DT::renderDT({
    req(meanPlotData())
    meanPlotData() %>%
      select(value,
             !!sym(input$aesX),
             everything())
  },
  filter           = "none",
  rownames         = FALSE,
  options          = dtOptions
  )
  
  # * pairwiseComparisonsTable ####
  output$pairwiseComparisonsTable <- DT::renderDT({
    validate(need("signif" %in% input$mainPlotAdditionalOptions,
                  "This table only shows when you tick \"Run pairwise t-tests\" in the plot options next to this. "))
    pairwiseComparisons()
  }, options = dtOptions, caption = "Calculated via t-tests on log-transformed data. \
                                  P-values corrected with the Benjamini-Hochberg procedure.")
  
  # Plots ####
  # * Main Plot ####
  # ** Ranges for zooming by clicking on the plot  ####

  
  
  # ** Main Plot Object ####
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$mainPlotDoubleClick, {
    brush <- input$mainPlotBrush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  }) 
  # * allClassMeanPlotData ####
  # Totals from data without class filtering, for "proportions relative to all"
  allClassMeanPlotData <- reactive({
    if (input$stackMode != "stack_percent_all") return(NULL)
    req(rawData())

    inputMod <- reactiveValuesToList(input)
    inputMod$lipidClassToSelect <- NULL
    inputMod$lipidClassToRemove <- NULL

    rawData() %>%
      filterRawDataFor(inputMod) %>%
      standardizeWithin(inputMod) %>%
      createPlotData(inputMod) %>%
      summarisePlotData(inputMod)
  })

  mainPlot <- reactive({
    req(plotData(), meanPlotData())
    createMainPlot(plotData              = plotData(),
                   meanPlotData          = meanPlotData(),
                   pairwiseComparisons   = pairwiseComparisons(),
                   input                 = input,
                   ranges                = ranges,
                   allClassMeanPlotData  = allClassMeanPlotData())
  })
  
  # ** Main Plot Render ####
  output$mainPlot <- renderPlot({
    mainPlot()
  })
  
  # * Heatmap ####
  # ** Heatmap Object ####
  heatmapPlot <- reactive({
    createHeatmap(plotData = meanPlotData(), input)
  })
  
  # ** Heatmap Render ####
  output$heatPlot <- renderPlot({
    heatmapPlot()
  })
  
  # * Dimensionality reduction ####
  
  # ** Updating pca-options ####
  # update number of principal components,
  # they should not exceed the dimensions of the data
  observe({
    req(wideData())
    updateSliderInput(session,
                      "pcaNumberPrincipalComponents",
                      max = min(dim(wideData())))
  })
  
  # ** data for dimensionality reduction ####
  wideData <- reactive({
    req(plotData())
    validate(
      need(input$aesColor == "sample",
           "To perform a PCA, please set color to sample in the mappings"),
      need((input$aesX != "sample" &
              input$aesX != "sample_replicate" &
              input$aesX != "sample_replicate_technical"),
           "To perform a PCA, please select a feature other than sample as your x-axis in the mappings"
      ),
      need(input$aesFacetCol == "",
           "To perform a PCA, please remove any facetting in the mappings"),
      need(input$aesFacetRow == "",
           "To perform a PCA, please remove any facetting in the mappings"),
      need(length(unique(plotData()[[input$aesX]])) > 1,
           "Not enough datapoints to perform PCA")
    )
    createWideData(plotData = plotData(), input)
  })
  
  # ** pcaPrep ####
  pcaPrep <- reactive({
    createPcaPrep(wideData(), input)
  })
  
  pcaTidy <- reactive(tidy(pcaPrep(), id = "pca"))
  pcaJuice <- reactive(juice(pcaPrep()))
  
  # ** pcaInfo ####
  output$pcaInfo <- renderPlot({
    req(pcaPrep())
    pcaPlotPercentVariation(pcaPrep(), input)
  })
  
  # ** Scores ####
  pcaScoresPlot <- reactive({
    req(pcaJuice())
    createPcaScoresPlot(pcaJuice = pcaJuice(), pcaTidy = pcaTidy(), input)
  })
  
  output$pcaScoresPlot <- renderPlot({
    pcaScoresPlot()
  })
  
  # ** Loadings ####
  pcaLoadingsPlot <- reactive({
    createPcaLoadingsPlot(pcaTidy = pcaTidy(), input)
  })
  
  output$pcaLoadingsPlot <- renderPlot({
    pcaLoadingsPlot()
  })
  
  # Download handlers ####
  output$saveMetaCSV <-
    downloadHandler(
      filename = function() {
        paste0("datasets_info.csv")
      },
      content = function(file) {
        write.csv(x = metaData(), file = file)
      }
    )
  
  output$saveRawCSV <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_raw_data.csv")
      },
      content = function(file) {
        write.csv(x = rawData(), file = file)
      }
    )
  
  output$saveMainCSV <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_filtered_data.csv")
      },
      content = function(file) {
        write.csv(x = mainData(), file = file)
      }
    )
  
  output$savePlotData <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_plot_data.csv")
      },
      content = function(file) {
        write.csv(x = plotData(), file = file)
      }
    )
  
  output$saveMeanPlotData <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_means.csv")
      },
      content = function(file) {
        write.csv(x = meanPlotData(), file = file)
      }
    )
  
  output$saveMainPlot <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_plot.pdf")
      },
      content = function(file) {
        ggsave(file, plot = mainPlot(), width = input$mainWidth,
               height = input$mainHeight)
      }
    )
  
  output$saveMainPlotRDS <- 
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_plot.rds")
      },
      content = function(file) {
        saveRDS(mainPlot(), file)
      }
    )
  
  output$saveHeatmap <- 
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_heatmap.pdf")
      },
      content = function(file) {
        ggsave(file, plot = heatmapPlot(),
               width = input$heatWidth,
               height = input$heatHeight)
      }
    )
  
  output$saveHeatmapRDS <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_heatmap.rds")
      },
      content = function(file) {
        saveRDS(heatmapPlot(), file)
      }
    )
  
  output$savePCAScores <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_pcaScoresPlot.pdf")
      },
      content = function(file) {
        ggsave(file, plot = pcaScoresPlot(),
               width = input$pcaWidth,
               height = input$pcaHeight)
      }
    )
  
  output$savePCALoadings <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_pcaLoadingsPlot.pdf")
      },
      content = function(file) {
        ggsave(file, plot = pcaLoadingsPlot(),
               width = input$pcaWidth,
               height = input$pcaHeight)
      }
    )
  
  output$savePCAScoresRDS <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "_pcaScoresPlot.rds")
      },
      content = function(file) {
        saveRDS(pcaScoresPlot(), file)
      }
    )
  
  output$savePCALoadingsRDS <-
    downloadHandler(
      filename = function() {
        tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
        tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
        paste0(Sys.Date(), "_", tmp, "pcaScoresPlot.rds")
      },
      content = function(file) {
        saveRDS(pcaLoadingsPlot(), file)
      }
    )
  
  # End ####
  # Note: stopApp() and dbDisconnect() removed from onSessionEnded.
  # shiny-server uses one shared R process for all users; calling stopApp()
  # when any session ends would kill the app for every connected user.
}
