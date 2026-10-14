# Entry points consume summaries only. SCSS styles HTML; widget marks are styled here.
chart_theme <- function() {
  ggplot2::theme_minimal(base_size = 16, base_family = "sans") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e4ebef"),
      plot.margin = ggplot2::margin(12, 16, 12, 12),
      axis.title = ggplot2::element_text(colour = "#213c50"))
}

as_widget <- function(plot) {
  widget <- plotly::ggplotly(plot, tooltip = "text")
  widget <- plotly::layout(widget, font = list(family = "Arial, sans-serif", size = 15),
    paper_bgcolor = "white", plot_bgcolor = "white", margin = list(t = 15))
  plotly::config(widget, displaylogo = FALSE, responsive = TRUE,
                 modeBarButtonsToRemove = list("sendDataToCloud"))
}

plot_recruitment <- function(summary, show_plan = FALSE) {
  d <- summary$recruitment
  d$hover <- paste0(format(d$date, "%d %b %Y"), "<br>Randomised: ", d$randomised)
  p <- ggplot2::ggplot(d, ggplot2::aes(date, randomised, text = hover, group = 1)) +
    ggplot2::geom_line(colour = "#087e8b", linewidth = 1.3) +
    ggplot2::labs(x = "Date", y = "Cumulative randomised patients") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::scale_y_continuous(limits = c(0, if (show_plan) 300 else NA)) + chart_theme()
  if (show_plan) {
    d$hover <- paste0(format(d$date, "%d %b %Y"), "<br>Illustrative recruitment plan: ", round(d$plan, 1))
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(y = plan),
                              colour = "#657786", linetype = "dashed", linewidth = 0.8)
  }
  as_widget(p)
}

plot_sites <- function(summary) {
  d <- summary$sites
  d$hover <- paste0("Site ", d$SITEID, " | ", d$COUNTRY, "<br>", d$activation_status,
    "<br>Randomised: ", d$randomised, "<br>Target: ", d$TARGET)
  p <- ggplot2::ggplot(d, ggplot2::aes(SITEID, randomised, text = hover)) +
    ggplot2::geom_col(fill = "#087e8b", width = 0.65) +
    ggplot2::geom_point(ggplot2::aes(y = TARGET), shape = 18, colour = "#213c50", size = 3.5) +
    ggplot2::labs(x = "Site (diamond = target)", y = "Randomised patients") + chart_theme()
  as_widget(p)
}

plot_terms <- function(summary) {
  d <- summary$terms
  if (!nrow(d)) {
    return(plotly::layout(plotly::plot_ly(x = numeric(), y = numeric(), type = "scatter", mode = "markers"),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = "No qualifying TEAEs at this cut", x = 0.5, y = 0.5, showarrow = FALSE))))
  }
  d$hover <- paste0(d$AETERM, "<br>Affected patients / safety N: ", d$incidence)
  p <- ggplot2::ggplot(d, ggplot2::aes(subjects, stats::reorder(AETERM, subjects), text = hover)) +
    ggplot2::geom_col(fill = "#087e8b", width = 0.65) +
    ggplot2::labs(x = "Distinct affected patients", y = NULL) + chart_theme()
  as_widget(p)
}

site_table <- function(summary) {
  d <- summary$sites[c("SITEID", "COUNTRY", "activation_status", "TARGET", "randomised",
                      "active", "completed", "discontinued", "teae", "serious_teae")]
  names(d) <- c("Site", "Country", "Activation", "Target", "Rand.", "Active", "Compl.", "Disc.", "TEAE", "Ser. TEAE")
  widget <- DT::datatable(d, rownames = FALSE, selection = "none", class = "stripe hover row-border",
    filter = "top", style = "default", width = "100%", fillContainer = FALSE,
    autoHideNavigation = FALSE,
    options = list(dom = "lftip", pageLength = 12, lengthMenu = c(6, 12, 24),
                   bPaginate = TRUE, paging = TRUE, lengthChange = TRUE,
                   info = TRUE, language = list(
                     info = "Showing _START_ to _END_ of _TOTAL_ entries",
                     infoEmpty = "Showing 0 to 0 of 0 entries",
                     infoFiltered = "(filtered from _MAX_ total entries)"),
                   scrollX = FALSE, autoWidth = FALSE, order = list(list(0, "asc"))))
  # Quarto 1.4 includes its table scripts before widget dependencies. Its supported
  # relocation attribute puts the bundled jQuery dependency first (no CDN/copy).
  for (i in seq_along(widget$dependencies)) {
    if (widget$dependencies[[i]]$name == "jquery") {
      widget$dependencies[[i]]$script <- list(list(
        src = widget$dependencies[[i]]$script, `data-relocate-top` = "true"))
    }
  }
  widget
}
