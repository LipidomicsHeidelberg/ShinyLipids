# TODO preserve width of single bars while not shifting points to others bars
# TODO reduce cyclomatic complexity of createMainPlot

#' Create the main plot
#' 
#' Uses ggplot2.
#'
#' @param plotData :: tibble. Data for the plot, pass it from reactive plotData().
#' For \code{input$stackMode == "stack_percent_all"} its values must already be
#' percent of the total (see \code{standardizeToTotal}).
#' @param meanPlotData :: tibble. Data of means, pass from reactive meanPlotData()
#' @param pairwiseComparisons :: tibble. Pairwise t-tests from pairwiseComparisons()
#' @param input :: list. Input list from shiny ui. Uses
#' @param ranges :: list(x, y). Ranges of the plot zoom
#' - aesX :: string.
#' - aesColor :: string.
#' - aesFacetCol :: string.
#' - aesFacetRow :: string.
#' - mainPlotAdditionalOptions :: character vector. Options:
#' list("points", "bars", "mean", "values", "ind_values", "log", 
#' "N", "label", "swap", "free_y", "signif")
#' - errorbarType :: string. "None" | "SD" | "SEM" | "CI"
#' - summariseTechnicalReplicates :: boolean.
#' - standardizationFeatures :: character vector | NULL.
#' - standardizeWithinTechnicalReplicate :: boolean.
#'
#' @return :: ggplot object
#' @export
createMainPlot <- function(plotData,
                           meanPlotData,
                           pairwiseComparisons,
                           input,
                           ranges = list(x = NULL, y = NULL)) {
  
  # Friendly X-axis label lookup
  xAxisLabels <- c(
    "sample"                    = "Sample",
    "sample_replicate"          = "Sample replicate",
    "sample_replicate_technical"= "Sample (technical replicate)",
    "class"                     = "Lipid class",
    "lipid"                     = "Lipid species",
    "category"                  = "Category",
    "func_cat"                  = "Functional category",
    "db"                        = "Number of double bonds",
    "oh"                        = "Hydroxylation state",
    "length"                    = "Chain length",
    "chains"                    = "Chains",
    "chain_sums"                = "Chain sums"
  )
  xAxisName <- if (!is.null(input$aesX) && input$aesX %in% names(xAxisLabels)) {
    xAxisLabels[[input$aesX]]
  } else {
    input$aesX
  }

  opts <- input$mainPlotAdditionalOptions

  # The same feature in rows and columns can't be facetted twice: facet it once
  aesFacetCol <- input$aesFacetCol
  aesFacetRow <- input$aesFacetRow
  if (aesFacetRow != "" && aesFacetRow == aesFacetCol) {
    aesFacetRow <- ""
  }
  # Columns that identify one bar (the grouping used by summarisePlotData)
  meanCols <- unique(c(input$aesX, input$aesColor, aesFacetCol, aesFacetRow))
  meanCols <- meanCols[meanCols != ""]

  # Numeric features are shown as categories on the discrete x-axis
  numericFeatures <- intersect(c("length", "db", "oh"), names(plotData))
  plotData <- plotData %>%
    mutate(across(all_of(numericFeatures), factor))
  meanPlotData <- meanPlotData %>%
    mutate(across(any_of(numericFeatures), factor))

  # Bar stacking mode (from dropdown)
  stackMode <- if (is.null(input$stackMode)) "none" else input$stackMode
  isStacked <- stackMode %in% c("stack_amount", "stack_percent") &&
    "bars" %in% opts

  if (isStacked) {
    validate(need(
      input$aesColor != "",
      "Stacked bars require a color feature to be set (e.g. class, category, or lipid species)."
    ))
  }

  # After baseline subtraction values sum to ~0, so proportions are meaningless
  isProportionMode <- stackMode %in% c("sbs_percent", "stack_percent_all") ||
    (isStacked && stackMode == "stack_percent")
  if (isProportionMode) {
    validate(need(
      is.null(input$baselineSample) || input$baselineSample == "",
      "Proportions can't be shown after subtracting a baseline sample (Data tab). Remove the baseline or choose another bar stacking mode."
    ))
  }

  # "% of all classes": plotData already holds every replicate's share of all
  # its lipids (see standardizeToTotal), so the bars are mean proportions.
  isPercentAll <- stackMode == "stack_percent_all"

  # "proportion within group" side by side: rescale bars and points by the same
  # factor per bar, so points stay centred on their bar and error bars keep
  # their relative size.
  isSbsPercent <- stackMode == "sbs_percent"
  if (isSbsPercent) {
    groupCols <- unique(c(input$aesX, aesFacetCol, aesFacetRow))
    groupCols <- groupCols[groupCols != ""]
    meanPlotData <- meanPlotData %>%
      group_by(!!!syms(groupCols)) %>%
      mutate(.scaleFactor = 100 / sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(.scaleFactor = if_else(is.finite(.scaleFactor), .scaleFactor, NA_real_))
    plotData <- plotData %>%
      left_join(select(meanPlotData, all_of(meanCols), ".scaleFactor"), by = meanCols) %>%
      mutate(value = value * .scaleFactor) %>%
      select(-".scaleFactor")
    meanPlotData <- meanPlotData %>%
      mutate(across(any_of(c("value", "SD", "SEM", "CI_lower", "CI_upper")),
                    ~ .x * .scaleFactor)) %>%
      select(-".scaleFactor")
  }

  # Log scale: stacking happens after the transformation, which would add up
  # log values, so stacked bars always use a linear scale.
  isLog     <- "log" %in% opts
  useLog    <- isLog && !isStacked
  isSwapped <- "swap" %in% opts

  plt <- ggplot(plotData, aes(x = !!sym(input$aesX), y = value)) %>%
    mainPlotAddColors(input$aesColor, plotData) %>%
    mainPlotAddBars(opts, meanPlotData,
                    if (isStacked) stackMode else "none")

  # Points, error bars, means, value labels are incompatible with stacked bars (but OK for % of all)
  if (!isStacked) {
    plt <- plt %>%
      mainPlotAddPoints(opts) %>%
      mainPlotAddErrorBars(input$errorbarType, meanPlotData, clampAtZero = useLog) %>%
      mainPlotAddMeans(opts, meanPlotData) %>%
      mainPlotAddValues(opts, meanPlotData) %>%
      mainPlotAddPointValues(opts) %>%
      mainPlotLabelPoints(opts, input$summariseTechnicalReplicates)
  }

  plt <- plt %>%
    mainPlotAddFacets(aesFacetRow, aesFacetCol, opts)

  # Show N (sample count annotation on top of bar, incompatible with stacked bars)
  if ("N" %in% opts && !isStacked) {
    plt <- plt +
      geom_text(
        data     = meanPlotData,
        aes(y = value, label = paste0("n=", N)),
        # sit above the "value of means" label when both are shown
        vjust    = if ("values" %in% opts) -1.7 else -0.5,
        hjust    = 0.5,
        size     = 3.5,
        fontface = "bold",
        color    = "grey30",
        position = position_dodge(width = 0.9)
      )
  }

  # Y-axis: name, labels, transformation
  isStandardized <- any(input$standardizationFeatures != "") ||
    input$standardizeWithinTechnicalReplicate

  if (isStacked && stackMode == "stack_percent") {
    # position_fill() stacks to 1
    yAxisName   <- "proportion within group [ % ]"
    yAxisLabels <- scales::percent_format(scale = 100, accuracy = NULL)
  } else if (isPercentAll) {
    yAxisName   <- "mean proportion of total (all classes) [ % ]"
    yAxisLabels <- scales::percent_format(scale = 1, accuracy = NULL)
  } else if (isSbsPercent) {
    yAxisName   <- "mean proportion within group [ % ]"
    yAxisLabels <- scales::percent_format(scale = 1, accuracy = NULL)
  } else if (isStandardized) {
    yAxisName   <- "mean amount [ Mol % ]"
    yAxisLabels <- scales::percent_format(scale = 1, accuracy = NULL)
  } else {
    yAxisName   <- "mean amount [ \u00b5M ]"
    yAxisLabels <- if (useLog) waiver() else scales::number_format()
  }
  if (useLog) {
    yAxisName <- paste0(yAxisName, ", log1p scale")
  }
  yAxisTransformation <- if (useLog) "log1p" else "identity"

  # Plot caption: explain N annotation if shown
  plotCaption <- c(
    if ("N" %in% opts && !isStacked) "n = number of samples per group",
    if (isLog && isStacked) "log1p scale is not available for stacked bars"
  )
  plotCaption <- if (length(plotCaption) > 0) paste(plotCaption, collapse = "\n") else NULL

  plt <- plt +
    scale_y_continuous(
      expand = expansion(mult = c(
        if ("N" %in% opts) 0.08 else 0, 0.05)),
      name   = yAxisName,
      labels = yAxisLabels,
      trans  = yAxisTransformation) +
    scale_x_discrete(name = xAxisName) +
    labs(caption = plotCaption)

  # Zoom ranges come from the brush in screen coordinates (x = horizontal axis).
  # Clip only while zoomed, so data outside the zoom isn't drawn over the axes.
  clip <- if (is.null(ranges$x) && is.null(ranges$y)) "off" else "on"
  if (isSwapped) {
    plt <- plt +
      coord_flip(xlim = ranges$y, ylim = ranges$x, clip = clip)
  } else {
    plt <- plt +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, clip = clip)
  }

  # Highlite significant hits
  if ("signif" %in% opts &&
      !is.null(pairwiseComparisons) &&
      all(c("p.value", input$aesX) %in% names(pairwiseComparisons))) {
    signif <- filter(pairwiseComparisons, p.value <= 0.05) %>%
      distinct(!!sym(input$aesX)) %>%
      mutate(across(any_of(numericFeatures), factor))
    if (nrow(signif) > 0) {
      plt <- plt +
        geom_text(
          data = signif,
          aes(!!sym(input$aesX), Inf, label = "*", vjust = 1, hjust = 0.5),
          inherit.aes = F,
          size        = 10
        )
    }
  }
  return(plt)
}


# subfunctions ------------------------------------------------------------
mainPlotAddColors <- function(plt, aesColor, plotData) {
  if (aesColor != "") {
    colorCount <- plotData[[aesColor]] %>%
      unique() %>% 
      length()
    plt <- plt +
      aes(
        color = factor(!!sym(aesColor)),
        fill  = factor(!!sym(aesColor)))
  } else {
    colorCount <- 0
  }
  
  plt +
    mainTheme() +
    mainScale(colorCount) +
    guides(
      color = guide_legend(ncol = 12,
                           nrow = as.integer(colorCount / 12) + 1,
                           title = aesColor),
      fill = guide_legend(ncol = 12, # useful for way too many colors
                          nrow = as.integer(colorCount / 12) + 1,
                          title = aesColor
      )
    )
}

  
mainPlotAddBars <- function(plt, mainPlotAdditionalOptions, meanPlotData,
                            stackMode = "none") {
  if ("bars" %in% mainPlotAdditionalOptions) {
    barPosition <- switch(stackMode,
      "stack_amount"      = position_stack(),
      "stack_percent"     = position_fill(),
      "stack_percent_all" = position_stack(),  # data already pre-computed as proportions
      position_dodge2(width = 0.9)             # "none"
    )
    plt + geom_col(data = meanPlotData, position = barPosition)
  } else plt
}


mainPlotAddPoints <- function(plt, mainPlotAdditionalOptions) {
  if ("points" %in% mainPlotAdditionalOptions) {
    plt +
      geom_point(
        position    = position_dodge(width = 0.9),
        pch         = 21,
        alpha       = 1,
        color       = "black",
        show.legend = FALSE
      )
  } else plt
}

mainPlotAddErrorBars <- function(plt, errorbarType, meanPlotData, clampAtZero = FALSE) {
  if (errorbarType != "None") {
    errorbarData <- meanPlotData %>%
      mutate(
        .ymin = switch(errorbarType,
                       "SD"  = value - SD,
                       "SEM" = value - SEM,
                       "CI"  = CI_lower),
        .ymax = switch(errorbarType,
                       "SD"  = value + SD,
                       "SEM" = value + SEM,
                       "CI"  = CI_upper)
      )
    if (clampAtZero) {
      # log1p is undefined below -1: don't let error bars of positive means cross 0
      errorbarData <- mutate(errorbarData, .ymin = pmax(.ymin, pmin(value, 0)))
    }
    plt +
      geom_errorbar(
        data = errorbarData,
        position = position_dodge(width = 0.9),
        aes(ymin = .ymin, ymax = .ymax),
        linewidth = 0.8,
        width     = 0.3,
        alpha     = 0.9,
        color     = "black"
      )
  } else plt
}

mainPlotAddMeans <- function(plt, mainPlotAdditionalOptions, meanPlotData) {
  if ("mean" %in% mainPlotAdditionalOptions) {
    # ggplot2 4.0 replaced `fatten` with `middle.linewidth`
    middleLine <- if (utils::packageVersion("ggplot2") >= "4.0.0") {
      list(middle.linewidth = 2)
    } else {
      list(fatten = 2)
    }
    plt +
      do.call(geom_crossbar, c(list(
        data      = meanPlotData,
        mapping   = aes(ymin = value, ymax = value),
        position  = position_dodge(width = 0.9),
        linewidth = 1.0,
        width     = 0.7,
        color     = "black"
      ), middleLine))
  } else plt
}

mainPlotAddFacets <- function(plt, aesFacetRow, aesFacetCol, mainPlotAdditionalOptions) {
  if (aesFacetCol == "" && aesFacetRow == "") {
    return(plt)
  }
  facetRow  <- if (aesFacetRow != "") vars(!!sym(aesFacetRow))
  facetCol  <- if (aesFacetCol != "") vars(!!sym(aesFacetCol))
  freeValue <- "free_y" %in% mainPlotAdditionalOptions

  if ("swap" %in% mainPlotAdditionalOptions) {
    # With coord_flip the categories run vertically: transpose the grid with the
    # axes so each panel only gets its own categories and a matching height.
    plt +
      facet_grid(
        rows   = facetCol,
        cols   = facetRow,
        scales = if (freeValue) "free" else "free_y",
        space  = "free_y"
      )
  } else {
    plt +
      facet_grid(
        rows   = facetRow,
        cols   = facetCol,
        scales = if (freeValue) "free" else "free_x",
        space  = "free_x"
      )
  }
}

mainPlotAddValues <- function(plt, mainPlotAdditionalOptions, meanPlotData) {
  if ("values" %in% mainPlotAdditionalOptions) {
    plt +
      geom_text(
        data = meanPlotData,
        aes(label = round(value, 2)),
        vjust     = 0,
        color     = "black",
        position  = position_dodge(width = 0.9)
      )
  } else plt
}

mainPlotAddPointValues <- function(plt, mainPlotAdditionalOptions) {
  if ("ind_values" %in% mainPlotAdditionalOptions) {
    plt +
      geom_text(
        aes(label = round(value, 2)),
        vjust    = 0,
        color    = "black",
        position = position_dodge(width = 0.9)
      )
  } else plt
}

mainPlotLabelPoints <- function(plt, mainPlotAdditionalOptions, summariseTechnicalReplicates) {
  if ("label" %in% mainPlotAdditionalOptions) {
    plt +
      geom_text(aes(label = !!sym(ifelse(
        summariseTechnicalReplicates,
        "sample_replicate",
        "sample_replicate_technical")
      )
      ),
      vjust    = 0,
      hjust    = 0,
      color    = "black",
      position = position_dodge(width = 0.9)
      )
  } else plt
}
