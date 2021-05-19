
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## pick ----
  output$pick <- reactive({
    FALSE
    if(input$method == "pick") TRUE
  })
  outputOptions(output, "pick", suspendWhenHidden = FALSE)

  output$gerechten <- renderDataTable({
    menubase %>% select(genre, naam, tijd, kcal)
  })

  ## pick ----
  output$select <- reactive({
    FALSE
    if(input$method == "select") TRUE
  })
  outputOptions(output, "select", suspendWhenHidden = FALSE)

  selectgenres <- reactive({
    lapply(as.list(input$weekdays), function(q){
      selectInput(inputId = q,
                  label = q,
                  choices = menubase %>% select(genre) %>% unique(),
                  width ="50%",
                  )
                  })
  })


  output$select_genres <- renderUI({
    selectgenres()
  })

  ## random nothing extra -----

    menu1 <- reactive({
      menu <- data.frame(dag = input$weekdays, naam = NA, tijd = NA, kcal = NA, recept = NA)
      set.seed(seed = as.numeric(input$week))

      if(input$method == "random"){
      menu <- menubase[sample(1:nrow(menubase),size = length(input$weekdays)),] %>%
        mutate(dag = input$weekdays)
      }
      if(input$method == "pick" & length(input$gerechten_rows_selected) >0){
        menu <- menubase[input$gerechten_rows_selected,] %>%
          mutate(dag = input$weekdays[1:length(input$gerechten_rows_selected)])
      }
      if(input$method == "select"){
        selections <- lapply(as.list(input$weekdays), function(q){
                        daggenre <- menubase %>% filter(genre == input[[q]]) %>%
                          sample_n(size = 1) %>%
                          mutate(dag = q)
                      })
        menu <- do.call('rbind', selections)

      }

      menu

    })

    output$menu <- function() {
        menu1() %>%
            select(dag, naam, tijd, kcal) %>%
            knitr::kable("html") %>%
            kable_styling("striped", full_width = F)
    }


    ingredienten1 <- reactive({
        menu1() %>%
            full_join(recepten, by = "naam") %>%
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


    #misschien nog toevoegen dat sommige dingen snel geselecteerd kunnen worden.
    output$boodschappenextra <- renderDataTable(
      andereboodschappen %>% select(categorie, producten)
    )


    boodschappenlijst <- reactive({
      lijst <-
      recepten %>%
        filter(naam %in% menu1()$naam) %>%
        left_join(producten, by = "ingredienten") %>%
        rename(producten = ingredienten) %>%
        group_by(categorie,producten, unit) %>%
        summarise(aantal = sum(as.numeric(pp) * input$personen),
                  .groups = "drop") %>%
        arrange(categorie) %>%
        select(categorie,aantal, unit, producten)
      if(length(input$boodschappenextra_rows_selected) > 0){
      lijst <- lijst %>%
        bind_rows({andereboodschappen %>%
          slice(input$boodschappenextra_rows_selected)}) %>%
        arrange(categorie)
      }
      lijst
    })

    # store 'empty' tibble
    user_table <-
      recepten %>%
      left_join(producten, by = "ingredienten") %>%
      rename(aantal = pp,
             producten = ingredienten) %>%
      select(categorie, aantal, unit, producten) %>%
      slice(1) %>%
      # transpose the first row of test into two columns
      gather(key = "column_name", value = "value") %>%
      # replace all values with ""
      mutate(value = "") %>%
      # reshape the data from long to wide
      spread(column_name, value) %>%
      # rearrange the column order to match that of test
      select(categorie, aantal, unit, producten)


    output$boodschappen <-
        renderDataTable(
        boodschappenlijst(),
        extensions="Buttons",
        editable = TRUE,
        rownames = FALSE,
        server = FALSE,
        options=list(dom="Bfrtip",
                     lengthChange = TRUE,
                     #lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
                     pageLength = -1,
                     buttons = list(
                       c("copy", "csv", "excel", "pdf", "print"),
                       list(
                         extend = "collection",
                         text = 'Show less',
                         action = DT::JS(
                           "function ( e, dt, node, config ) {
                                    dt.page.len(15);
                                    dt.ajax.reload();
                                }"
                         )
                       ),
                       list(
                         extend = "collection",
                         text = 'Show all',
                         action = DT::JS(
                           "function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }"
                         )
                       )
                     )
        )
        )



    # store a proxy of tbl
    proxy <- dataTableProxy(outputId = "boodschappen")

    # each time addData is pressed, add user_table to proxy
    observeEvent(eventExpr = input$addData, {
      proxy %>%
        addRow(user_table)
    })


}
