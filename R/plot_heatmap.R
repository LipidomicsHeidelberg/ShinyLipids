#' Create a Heatmap from data
#'
#' @param plotData tibble. typically meanPlotData()
#' @param input :: list. Uses:
#' - standardizationFeatures NULL | character vector.
#' - aesX :: string.
#' - aesColor :: string.
#' - aesFacetCol :: string.
#' - aesFacetRow :: string.
#' - heatLabSize :: numeric. Size of axis labels
#' - heatColor :: string. Color palette
#'   One of c("viridis", "magma", "plasma", "inferno", "cividis")
#' - heatLogScale :: logical.
#'
#' @return ggplot. Heatmap
#' @export
createHeatmap <- function(plotData, input) {
  validate(need(
    input$aesColor != "",
    "To draw a heatmap, please select a feature to display by color (used as the y-axis of the heatmap)"
  ))
  # The same feature in rows and columns can't be facetted twice: facet it once
  aesFacetCol <- input$aesFacetCol
  aesFacetRow <- input$aesFacetRow
  if (aesFacetRow != "" && aesFacetRow == aesFacetCol) {
    aesFacetRow <- ""
  }

  # Name of the color scale: proportion of total, standardized or plain amounts
  if (identical(input$stackMode, "stack_percent_all")) {
    fillName <- "proportion of total (all classes) [ % ]"
  } else if (any(input$standardizationFeatures != "") || input$standardizeWithinTechnicalReplicate) {
    fillName <- "amount [ Mol % ]"
  } else {
    fillName <- "amount [ \u00b5M ]"
  }
  if (input$heatLogScale) {
    plotData$value <- log1p(plotData$value)
    fillName <- paste("log1p", fillName)
  }
  
  plt <- ggplot(plotData) +
    aes(
      x = factor(!!sym(input$aesX)),
      y = factor(!!sym(input$aesColor)),
      fill = value
    ) +
    geom_raster() +
    mainTheme() +
    theme(
      axis.text.y      = element_text(size = input$heatLabSize, colour = "black"),
      plot.background  = element_blank(),
      panel.grid       = element_blank(),
      panel.background = element_rect(colour = NA, fill = "grey80")
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_viridis_c(option = input$heatColor) +
    labs(
      y    = input$aesColor,
      x    = input$aesX,
      fill = fillName
    )
  
  if (aesFacetCol != "" & aesFacetRow != "") {
    plt <- plt +
      facet_grid(
        rows   = vars(!!sym(aesFacetRow)),
        cols   = vars(!!sym(aesFacetCol)),
        scales = "free"
      )
  } else {
    if (aesFacetCol != "") {
      plt <- plt +
        facet_wrap(facets = vars(!!sym(aesFacetCol)), scales = "free")
    }
    if (aesFacetRow != "") {
      plt <- plt +
        facet_wrap(facets = vars(!!sym(aesFacetRow)), scales = "free")
    }
  }
  plt
}
