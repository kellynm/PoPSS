infoLabelInputUI <- function(id, label = "", title = "help", trigger = "hover", placement = "right", size ="extra-small", icons = icon("info")){
  theme = "infoLabel.css"
  ns <- NS(id)
  tagList(
    h5(label, bsButton(inputId = ns("info"), icon = icons, label = "", size = size)),
    bsTooltip(id = ns("info"), title = title, trigger = trigger, placement = "right")
  )
}
infoLabelInput <- function(input,output, session) {
}

ui <- fluidPage(
  theme = "infoLabel.css",
  #h5("yay", bsButton(inputId = "info", icon = icon("info"), label = "", size = "extra-small")),
  numericInput("start", label = infoLabelInputUI("start", title = "Year to start simulation"), value = 2000, min = 1960, max = 2020),
  numericInput("start2", label = infoLabelInputUI("start2", label = "Awesome!", title = "Year to start simulation yada yada yada"), value = 2000, min = 1960, max = 2020)
)
server <- function(input,output) {
}

shinyApp(ui,server)