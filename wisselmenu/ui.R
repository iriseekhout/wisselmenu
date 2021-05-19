library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(plotly)
library(shiny)
library(kableExtra)
library(knitr)
library(tidyr)


ui <- dashboardPage(
    dashboardHeader(title = "Wissel menu"),
    dashboardSidebar(
        selectInput("method",
                    label = "Menu samenstellen",
                    choices = c("pick", "select", "random")),

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
            conditionalPanel(
                condition = "output.pick",
            box(title = "Selecteer gerechten",
                solidHeader = T,
                width = 9,
                collapsible = T,
                dataTableOutput("gerechten"))

        ),
        conditionalPanel(
            condition = "output.select",
            box(title = "Selecteer soort gerecht per dag",
                solidHeader = T,
                width = 9,
                collapsible = T,
                uiOutput("select_genres"))

        ),
        box(title = "Menu",
                solidHeader = T,
                width = 3,
                collapsible = T,
                tableOutput("menu"),

               )
        ),
           fluidRow(
               box( title = "Week menu",
                    collapsible = T,
                    width = 12,
               tabBox(
                width = 12,
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
            )
            ),
        fluidRow(
            box(title = "Extra boodschappen toevoegen",
                solidHeader = T,
                width = 6, collapsible = T,
                p("Door items uit de onderstaande lijst aan te klikken worden ze toegevoegd aan de boodschappenlijst."),
                dataTableOutput("boodschappenextra")
                ),
            box(title = "Boodschappenlijst", solidHeader = T,
                width = 6, collapsible = T,
                p("Met de knop hieronder kun je zelf boodschappen toevoegen aan de lijst. Doe dit op het laatst want bij selecteren uit lijst links verdwijnen de extra entries."),
                actionButton("addData", "Items toevoegen"),
                br(),
                br(),
                dataTableOutput("boodschappen")
               )
        )

    ) # body

)

