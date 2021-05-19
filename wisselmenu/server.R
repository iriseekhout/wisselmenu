
# Define server logic required to draw a histogram
server <- function(input, output, session) {

    menu1 <- reactive({
        set.seed(seed = as.numeric(input$week))
        menubase[sample(1:nrow(menubase),size = length(input$weekdays)),] %>%
            mutate(dag = input$weekdays)
    })

    output$menu <- function() {
        menu1() %>%
            select(dag, naam, tijd, kcal) %>%
            knitr::kable("html") %>%
            kable_styling("striped", full_width = F)
    }


    ingredienten1 <- reactive({
        menu1() %>%
            full_join(recepten) %>%
            mutate(aantal = as.numeric(pp) * input$personen) %>%
        select(dag, aantal, unit, ingredienten)
    })


    output$ma_menu <- renderText({
        menu1() %>% filter(dag == "Maandag") %>% select(naam) %>% unlist()

    })

    output$ma_menu <- renderValueBox({
        valueBox(value = menu1() %>% filter(dag == "Maandag") %>% select(naam) %>% unlist(),
                 subtitle = paste("Bereiding: ", menu1() %>% filter(dag == "Maandag") %>% select(tijd) %>% unlist(), "minuten."),
                 color = "light-blue")

    })
    output$ma_recept <- renderValueBox({
        valueBox(value = "Recept:",
                 subtitle = menu1() %>% filter(dag == "Maandag") %>% select(recept) %>% unlist(),
                 color = "light-blue")

    })
    output$ma_ingredienten <- function() {
        ingredienten1() %>%
            filter(dag == "Maandag") %>%
            select(-dag) %>%
            knitr::kable("html") %>%
            kable_styling("striped", full_width = F)
    }

    output$di_menu <- renderValueBox({
      valueBox(value = menu1() %>% filter(dag == "Dinsdag") %>% select(naam) %>% unlist(),
               subtitle = paste("Bereiding: ", menu1() %>% filter(dag == "Dinsdag") %>% select(tijd) %>% unlist(), "minuten."),
               color = "light-blue")

    })
    output$di_recept <- renderValueBox({
      valueBox(value = "Recept:",
               subtitle = menu1() %>% filter(dag == "Dinsdag") %>% select(recept) %>% unlist(),
               color = "light-blue")

    })
    output$di_ingredienten <- function() {
      ingredienten1() %>%
        filter(dag == "Dinsdag") %>%
        select(-dag) %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }
    output$wo_menu <- renderValueBox({
      valueBox(value = menu1() %>% filter(dag == "Woensdag") %>% select(naam) %>% unlist(),
               subtitle = paste("Bereiding: ", menu1() %>% filter(dag == "Woensdag") %>% select(tijd) %>% unlist(), "minuten."),
               color = "light-blue")

    })
    output$wo_recept <- renderValueBox({
      valueBox(value = "Recept:",
               subtitle = menu1() %>% filter(dag == "Woensdag") %>% select(recept) %>% unlist(),
               color = "light-blue")

    })
    output$wo_ingredienten <- function() {
      ingredienten1() %>%
        filter(dag == "Woensdag") %>%
        select(-dag) %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }
    output$do_menu <- renderValueBox({
      valueBox(value = menu1() %>% filter(dag == "Donderdag") %>% select(naam) %>% unlist(),
               subtitle = paste("Bereiding: ", menu1() %>% filter(dag == "Donderdag") %>% select(tijd) %>% unlist(), "minuten."),
               color = "light-blue")

    })
    output$do_recept <- renderValueBox({
      valueBox(value = "Recept:",
               subtitle = menu1() %>% filter(dag == "Donderdag") %>% select(recept) %>% unlist(),
               color = "light-blue")

    })
    output$do_ingredienten <- function() {
      ingredienten1() %>%
        filter(dag == "Donderdag") %>%
        select(-dag) %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }
    output$vr_menu <- renderValueBox({
      valueBox(value = menu1() %>% filter(dag == "Vrijdag") %>% select(naam) %>% unlist(),
               subtitle = paste("Bereiding: ", menu1() %>% filter(dag == "Vrijdag") %>% select(tijd) %>% unlist(), "minuten."),
               color = "light-blue")

    })
    output$vr_recept <- renderValueBox({
      valueBox(value = "Recept:",
               subtitle = menu1() %>% filter(dag == "Vrijdag") %>% select(recept) %>% unlist(),
               color = "light-blue")

    })
    output$vr_ingredienten <- function() {
      ingredienten1() %>%
        filter(dag == "Vrijdag") %>%
        select(-dag) %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }
    output$za_menu <- renderValueBox({
      valueBox(value = menu1() %>% filter(dag == "Zaterdag") %>% select(naam) %>% unlist(),
               subtitle = paste("Bereiding: ", menu1() %>% filter(dag == "Zaterdag") %>% select(tijd) %>% unlist(), "minuten."),
               color = "light-blue")

    })
    output$za_recept <- renderValueBox({
      valueBox(value = "Recept:",
               subtitle = menu1() %>% filter(dag == "Zaterdag") %>% select(recept) %>% unlist(),
               color = "light-blue")

    })
    output$za_ingredienten <- function() {
      ingredienten1() %>%
        filter(dag == "Zaterdag") %>%
        select(-dag) %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }
    output$zo_menu <- renderValueBox({
      valueBox(value = menu1() %>% filter(dag == "Zondag") %>% select(naam) %>% unlist(),
               subtitle = paste("Bereiding: ", menu1() %>% filter(dag == "Zondag") %>% select(tijd) %>% unlist(), "minuten."),
               color = "light-blue")

    })
    output$zo_recept <- renderValueBox({
      valueBox(value = "Recept:",
               subtitle = menu1() %>% filter(dag == "Zondag") %>% select(recept) %>% unlist(),
               color = "light-blue")

    })
    output$zo_ingredienten <- function() {
      ingredienten1() %>%
        filter(dag == "Zondag") %>%
        select(-dag) %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }

    boodschappen <- reactive({
      menu1() %>%
        full_join(recepten) %>%
        left_join(producten) %>%
        select(pp, unit, ingredienten, categorie) %>%
        group_by(categorie,ingredienten, unit) %>%
        summarise(aantal = sum(as.numeric(pp) * input$personen),
                  .groups = "drop") %>%
        select(categorie,aantal, unit, ingredienten)
    })
    output$boodschappen <-
        renderDataTable(
        boodschappen(),
        extensions="Buttons", options=list(dom="Bfrtip",
                                           lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                           pageLength = -1,
                                           buttons = list(c("copy", "csv", "excel", "pdf", "print"),
                                                          list(

                                                          extend = "collection",
                                                          text = 'Show less',
                                                          action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(15);
                                    dt.ajax.reload();
                                }")
                                           ),
                                           list(

                                             extend = "collection",
                                             text = 'Show all',
                                             action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")
                                           )
                                           )
        )
        )


    # # value boxes
    # output$calories <- renderValueBox({
    #     valueBox(paste0(nutrition_df()$Value[nutrition_df()$NutrientID == 208], "kcal"),
    #              "Calories", icon = icon("fire"), color = "yellow")
    # })


}
