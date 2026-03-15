# Deploy the celiac-webapp to shinyapps.io
library(rsconnect)

rsconnect::deployApp(
    appDir = "/home/haig/Repos/celiac-webapp",
    account = "celiac"
)

