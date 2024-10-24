# Інсталюємо та завантажуємо пакети
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("DT")) install.packages("DT", dependencies = TRUE) # Для таблиці
library(shiny)
library(ggplot2)
library(DT)  # Завантажуємо бібліотеку для роботи з таблицями
library(dplyr) # Для обробки даних

# Front-end: Опис інтерфейсу
ui <- fluidPage(
  titlePanel("Характеристики діамантів"),  # Заголовок додатка
  
  sidebarLayout(
    sidebarPanel(  
      # Випадаючий список для вибору характеристики
      selectInput("select", h3("Вибрати характеристику"), 
                  choices = names(diamonds), 
                  selected = "carat")  # Встановлюємо за замовчуванням "carat"
    ),
    
    mainPanel(
      plotOutput("hist"),           # Графік гістограми
      dataTableOutput("table")      # Вивід таблиці
    )
  )
)

# Back-end: Логіка серверної частини
server <- function(input, output, session) {
  
  # Реактивний датасет
  data_2 <- reactive({
    if (input$select == "carat") {
      browser()  # Зупинка програми для налагодження
    }
    diamonds[[input$select]]
  })
  
  # Генерація таблиці
  output$table <- renderDataTable({
    diamonds %>% 
      arrange(desc(carat)) %>%
      head(100) %>%
      select(carat, cut, color, price)
  }, options = list(pageLength = 5))
  
  # Генерація гістограми для вибраної характеристики
  output$hist <- renderPlot({
    ggplot(diamonds, aes(x = data_2())) + 
      geom_bar(fill = "blue", color = "white") +
      labs(title = paste("Гістограма для:", input$select), x = input$select, y = "Кількість")
  })
}

# Запуск додатка
shinyApp(ui = ui, server = server)