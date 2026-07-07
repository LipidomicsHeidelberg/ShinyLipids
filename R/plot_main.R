# TODO preserve width of single bars while not shifting points to others bars
# TODO reduce cyclomatic complexity of createMainPlot

#' Create the main plot
#' 
#' Uses ggplot2.
#'
#' @param plotData :: tibble. Data for the plot, pass it from reactive plotData()
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
                           ranges = list(x = NULL, y = NULL),
                           allClassMeanPlotData = NULL) {
  
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

  if ("length" %in% names(plotData)) {
    plotData <- plotData %>%
      mutate(length = factor(length))

    meanPlotData <- meanPlotData %>%
      mutate(length = factor(length))
  }
  

  
  # Bar stacking mode (from dropdown)
  stackMode <- input$stackMode
  isStacked <- stackMode %in% c("stack_amount", "stack_percent") &
    "bars" %in% input$mainPlotAdditionalOptions

  if (isStacked) {
    validate(need(
      input$aesColor != "",
      "Stacked bars require a color feature to be set (e.g. class, category, or lipid species)."
    ))
  }

  # "% of all classes": compute proportions from unfiltered-by-class data (shown side by side)
  isPercentAll <- stackMode == "stack_percent_all" &
    "bars" %in% input$mainPlotAdditionalOptions &
    !is.null(allClassMeanPlotData)
  if (isPercentAll) {
    colorFacetCols <- c(input$aesColor, input$aesFacetCol, input$aesFacetRow)
    colorFacetCols <- colorFacetCols[colorFacetCols != ""]

    if (length(colorFacetCols) > 0) {
      totals <- allClassMeanPlotData %>%
        group_by(!!!syms(colorFacetCols)) %>%
        summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
    } else {
      totals <- allClassMeanPlotData %>%
        summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
    }

    joinCols <- intersect(colorFacetCols, names(meanPlotData))
    meanPlotData <- meanPlotData %>%
      left_join(totals, by = if (length(joinCols) > 0) joinCols else character()) %>%
      mutate(value = value / total_value)

    joinCols <- intersect(colorFacetCols, names(plotData))
    plotData <- plotData %>%
      left_join(totals, by = if (length(joinCols) > 0) joinCols else character()) %>%
      mutate(value = value / total_value)
  }

  # "proportion within group" side by side: pre-compute proportions, display dodged
  isSbsPercent <- stackMode == "sbs_percent" & "bars" %in% input$mainPlotAdditionalOptions
  if (isSbsPercent) {
    groupCols <- c(input$aesX, input$aesFacetCol, input$aesFacetRow)
    groupCols <- groupCols[groupCols != ""]
    meanPlotData <- meanPlotData %>%
      group_by(!!!syms(groupCols)) %>%
      mutate(value = value / sum(value, na.rm = TRUE)) %>%
      ungroup()
    plotData <- plotData %>%
      group_by(!!!syms(groupCols)) %>%
      mutate(value = value / sum(value, na.rm = TRUE)) %>%
      ungroup()
  }

  plt <- ggplot(plotData, aes(x = !!sym(input$aesX), y = value)) %>%
    mainPlotAddColors(input$aesColor, plotData) %>%
    mainPlotAddBars(input$mainPlotAdditionalOptions, meanPlotData,
                    if (isStacked) stackMode else "none")

  # Points, error bars, means, value labels are incompatible with stacked bars (but OK for % of all)
  if (!isStacked) {
    plt <- plt %>%
      mainPlotAddPoints(input$mainPlotAdditionalOptions) %>%
      mainPlotAddErrorBars(input$errorbarType, meanPlotData) %>%
      mainPlotAddMeans(input$mainPlotAdditionalOptions, meanPlotData) %>%
      mainPlotAddValues(input$mainPlotAdditionalOptions, meanPlotData) %>%
      mainPlotAddPointValues(input$mainPlotAdditionalOptions) %>%
      mainPlotLabelPoints(input$mainPlotAdditionalOptions, input$summariseTechnicalReplicates)
  }

  plt <- plt %>%
    mainPlotAddFacets(input$aesFacetRow, input$aesFacetCol, input$mainPlotAdditionalOptions)

  # Show N (sample count annotation on top of bar, incompatible with stacked bars)
  if ("N" %in% input$mainPlotAdditionalOptions & !isStacked) {
    plt <- plt +
      geom_text(
        data     = meanPlotData,
        aes(y = value, label = paste0("n=", N)),
        vjust    = -0.5,
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
  isLog <- "log" %in% input$mainPlotAdditionalOptions

  if (isPercentAll) {
    yAxisName            <- "mean proportion of total (all classes) [ % ]"
    yAxisLabels          <- scales::percent_format(accuracy = 1)
    yAxisTransformation  <- "identity"
  } else if (stackMode == "stack_percent" & isStacked) {
    yAxisName            <- "proportion within group [ % ]"
    yAxisLabels          <- scales::percent_format(accuracy = 1)
    yAxisTransformation  <- "identity"
  } else if (isSbsPercent) {
    yAxisName            <- "proportion within group [ % ]"
    yAxisLabels          <- scales::percent_format(accuracy = 1)
    yAxisTransformation  <- "identity"
  } else if (isLog && isStandardized) {
    yAxisName            <- "mean amount [ Mol % ], log1p scale"
    yAxisLabels          <- scales::percent_format(scale = 1, accuracy = NULL)
    yAxisTransformation  <- "log1p"
  } else if (isLog && !isStandardized) {
    yAxisName            <- "mean amount [ \u00b5M ], log1p scale"
    yAxisLabels          <- waiver()
    yAxisTransformation  <- "log1p"
  } else if (!isLog && isStandardized) {
    yAxisName            <- "mean amount [ Mol % ]"
    yAxisLabels          <- scales::percent_format(scale = 1, accuracy = NULL)
    yAxisTransformation  <- "identity"
  } else {
    yAxisName            <- "mean amount [ \u00b5M ]"
    yAxisLabels          <- scales::number_format()
    yAxisTransformation  <- "identity"
  }

  # Plot caption: explain N annotation if shown
  plotCaption <- if ("N" %in% input$mainPlotAdditionalOptions & !isStacked) {
    "n = number of samples per group"
  } else {
    NULL
  }

  plt <- plt +
    scale_y_continuous(
      expand = expansion(mult = c(
        if_else("N" %in% input$mainPlotAdditionalOptions, 0.08, 0), 0.05)),
      name   = yAxisName,
      labels = yAxisLabels,
      trans  = yAxisTransformation) +
    scale_x_discrete(name = xAxisName) +
    labs(caption = plotCaption) +
    coord_cartesian(xlim = ranges$x, ylim = ranges$y, clip = "off")
  
  # Swap X and Y
  if ("swap" %in% input$mainPlotAdditionalOptions) {
    validate(
      need(
        !("log" %in% input$mainPlotAdditionalOptions),
        "Swapped X and Y Axis are currently not supported for a logarithmic Y-Axis"
      )
    )
    plt <- plt +
      coord_flip()
  }
  
  # Highlite significant hits
  if ("signif" %in% input$mainPlotAdditionalOptions) {
    signif <- filter(pairwiseComparisons, p.value <= 0.05) %>%
      distinct(!!sym(input$aesX))
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

mainPlotAddErrorBars <- function(plt, errorbarType, meanPlotData) {
  if (errorbarType != "None") {
    plt +
      geom_errorbar(
        data = meanPlotData,
        position = position_dodge(width = 0.9),
        aes(ymin = switch(
          errorbarType,
          "SD"   = value - SD,
          "SEM"  = value - SEM,
          "CI"   = CI_lower
        ),
        ymax = switch(
          errorbarType,
          "SD"  = value + SD,
          "SEM" = value + SEM,
          "CI"  = CI_upper
        )),
        linewidth = 0.8,
        width     = 0.3,
        alpha     = 0.9,
        color     = "black"
      )
  } else plt
}

mainPlotAddMeans <- function(plt, mainPlotAdditionalOptions, meanPlotData) {
  if ("mean" %in% mainPlotAdditionalOptions) {
    plt +
      geom_crossbar(
        data      = meanPlotData,
        aes(ymin  = value, ymax = value),
        position  = position_dodge(width = 0.9),
        linewidth = 1.0,
        width     = 0.7,
        fatten    = 2,
        color     = "black"
      )
  } else plt
}

mainPlotAddFacets <- function(plt, aesFacetRow, aesFacetCol, mainPlotAdditionalOptions) {
  if (aesFacetCol != "" | aesFacetRow != "") {
    facet_col <- vars(!!sym(aesFacetCol))
    facet_row <- vars(!!sym(aesFacetRow))
    
    if (aesFacetCol == "") {
      facet_col <- NULL
    }
    if (aesFacetRow == "") {
      facet_row <- NULL
    }
    
    plt +
      facet_grid(
        cols   = facet_col,
        rows   = facet_row,
        scales = if_else("free_y" %in% mainPlotAdditionalOptions, "free", "free_x"),
        space  = "free_x"
      )
  } else plt
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
