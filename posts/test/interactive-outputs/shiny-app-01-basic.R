# app.R

# ---- Robust package setup ----

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

required_packages <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "plotly",
  "DT",
  "htmlwidgets",
  "pharmaverseadamjnj"
)

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    type = "binary",
    dependencies = TRUE
  )
}

# ---- Load packages only after all checks/installations are complete ----

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(htmlwidgets)
library(pharmaverseadamjnj)

# ---- Data preparation ----

adsl <- pharmaverseadamjnj::adsl

adsl2 <- adsl %>%
  select(USUBJID, TRT01P, TRTDURD, WEIGHTBL, HEIGHTBL) %>%
  filter(!is.na(TRT01P) & !is.na(TRTDURD))

adsl3 <- adsl2 %>%
  mutate_if(is.character, as.factor)

tf_colors <- c(
  "Placebo" = "#0072B2",
  "Xanomeline High Dose" = "#D55E00",
  "Xanomeline Low Dose" = "#CC79A7"
)

# ---- UI ----

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),

  titlePanel("Quarto Dashboard"),

  fluidRow(
    column(
      width = 6,
      h4("Treatment Duration (Days) by Treatment Group"),
      plotlyOutput("duration_plot", height = "420px")
    ),
    column(
      width = 6,
      h4("Baseline Weight (kg) vs Baseline Height (cm)"),
      plotlyOutput("height_weight_plot", height = "420px")
    )
  ),

  fluidRow(
    column(
      width = 12,
      h4("Subject-Level Data"),
      DTOutput("subject_table")
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {

  output$duration_plot <- renderPlotly({
    f1 <- ggplot(
      data = adsl3,
      aes(x = TRT01P, y = TRTDURD, fill = TRT01P)
    ) +
      geom_boxplot() +
      scale_x_discrete(name = "Treatment Group") +
      scale_y_continuous(
        limits = c(0, 250),
        breaks = seq(0, 250, 50),
        expand = c(0.05, 0.05),
        name = "Treatment Duration (Days)"
      ) +
      scale_fill_manual(
        values = tf_colors,
        name = "Treatment Group"
      ) +
      theme_bw() +
      theme(legend.position = "top")

    ggplotly(f1) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.02,
          yanchor = "bottom"
        ),
        margin = list(t = 45)
      )
  })

  output$height_weight_plot <- renderPlotly({
    f2 <- ggplot(
      data = adsl3,
      aes(
        x = HEIGHTBL,
        y = WEIGHTBL,
        shape = TRT01P,
        colour = TRT01P
      )
    ) +
      geom_point() +
      scale_x_continuous(
        limits = c(0, 150),
        breaks = seq(0, 150, 10),
        name = "Baseline Height (cm)"
      ) +
      scale_y_continuous(
        limits = c(0, 150),
        breaks = seq(0, 150, 10),
        name = "Baseline Weight (kg)"
      ) +
      scale_shape_manual(
        values = c(16, 17, 15),
        name = "Treatment Group"
      ) +
      scale_colour_manual(
        values = tf_colors,
        name = "Treatment Group"
      ) +
      theme_bw() +
      theme(legend.position = "top")

    ggplotly(f2) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.02,
          yanchor = "bottom"
        ),
        margin = list(t = 45)
      )
  })

  output$subject_table <- renderDT({
    my_table_2 <- adsl3 %>%
      rename(
        "Unique Subject Identifier" = USUBJID,
        "Treatment Group" = TRT01P,
        "Total Treatment Duration (Days)" = TRTDURD,
        "Baseline Weight (kg)" = WEIGHTBL,
        "Baseline Height (cm)" = HEIGHTBL
      )

    datatable(
      my_table_2,
      extensions = c("ColReorder"),
      rownames = FALSE,
      filter = "top",
      class = "stripe hover compact",
      fillContainer = FALSE,
      autoHideNavigation = FALSE,
      options = list(
        colReorder = TRUE,
        bPaginate = TRUE,
        paging = TRUE,
        lengthChange = TRUE,
        searching = TRUE,
        info = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 12, 20, 50, 100),
        dom = '<"top"lf>rt<"bottom"ip>',
        language = list(
          info = "Showing _START_ to _END_ of _TOTAL_ entries"
        )
      )
    )
  })
}

# ---- Run app ----

shinyApp(ui = ui, server = server)
