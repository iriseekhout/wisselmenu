library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(shiny)
library(kableExtra)
library(knitr)


ui <- dashboardPage(
    dashboardHeader(title = "Wissel menu"),
    dashboardSidebar(
        dateInput(
            inputId = "week",
            label = "Week",
            value = NULL,
            min = NULL,
            max = NULL,
            format = "dd-mm-yyyy",
            startview = "month",
            weekstart = 1,
            language = "nl",
            width = NULL,
            autoclose = TRUE,
            datesdisabled = NULL,
            daysofweekdisabled = NULL
        ),
        checkboxGroupInput(
            "weekdays",
            label = "Welke dagen van de week?",
            choices = c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag"),
            selected = c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")
        ),
        numericInput("personen",
                     label = "Aantal personen",
                     value = 4)

    ),
    dashboardBody(
        fluidRow(
           # valueBoxOutput("calories"),
            #valueBoxOutput("over_nutrient"),
            #valueBoxOutput("rich_nutrient")
        ),
        fluidRow(
            box(title = "Menu",
                solidHeader = T,
                width = 3,
                collapsible = T,
                tableOutput("menu"),

               ),
            tabBox(
                width = 9,
                tabPanel("Maandag",
                         valueBoxOutput("ma_menu"),
                         valueBoxOutput("ma_recept"),
                         tableOutput("ma_ingredienten")
                ),
                tabPanel("Dinsdag",
                         valueBoxOutput("di_menu"),
                         valueBoxOutput("di_recept"),
                         tableOutput("di_ingredienten")),
                tabPanel("Woensdag",
                         valueBoxOutput("wo_menu"),
                         valueBoxOutput("wo_recept"),
                         tableOutput("wo_ingredienten")),
                tabPanel("Donderdag",
                         valueBoxOutput("do_menu"),
                         valueBoxOutput("do_recept"),
                         tableOutput("do_ingredienten")),
                tabPanel("Vrijdag",
                         valueBoxOutput("vr_menu"),
                         valueBoxOutput("vr_recept"),
                         tableOutput("vr_ingredienten")),
                tabPanel("Zaterdag",
                         valueBoxOutput("za_menu"),
                         valueBoxOutput("za_recept"),
                         tableOutput("za_ingredienten")),
                tabPanel("Zondag",
                         valueBoxOutput("zo_menu"),
                         valueBoxOutput("zo_recept"),
                         tableOutput("zo_ingredienten"))

            )
            ),
        fluidRow(
            box(title = "Boodschappen", solidHeader = T,
                width = 6, collapsible = T,
                dataTableOutput("boodschappen")
               )
        )

    ) # body

)

