library(flexdashboard)
library(rmarkdown)

setwd("dashboard")
render("sample.Rmd",
        output_file = "rendered.html",
        params = list(rate = 50)
)
