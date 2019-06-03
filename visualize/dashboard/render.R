library(flexdashboard)
library(rmarkdown)

setwd("dashboard")
render("sample.Rmd",
        output_file = "rendered.html",
        params = list(rate = 50)
)

install.packages('metricsgraphics')
install.packages('rbokeh')
install.packages('plotly')
install.packages('highcharter')
install.packages('dygraphs')
