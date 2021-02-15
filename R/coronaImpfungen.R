library(readr)
library(dplyr)
library(tidyr)
library(scales)

einwohnerZahlLkEbe <- 143649

impfungenRaw <- read_delim(
  file = "data/lra-ebe-corona/impfungenLkEbe.csv",
  delim = ",",
  col_names = TRUE,
  col_types = cols(
    datum = col_date(format = "%Y-%m-%d"),
    erstimpfungen = col_integer(),
    zweitimpfungen = col_integer(),
    onlineanmeldungen = col_integer()
  )
)

ui <- function(request, id) {
  ns <- NS(id)
  tagList(
    h2("Corona-Impfungen im Landkreis Ebersberg"),

    fluidRow(
      valueBoxOutput(ns("valueBoxErstimpfungen")),
      valueBoxOutput(ns("valueBoxImpfquote")),
      valueBoxOutput(ns("valueBoxStand"))
    ),

    flowLayout(
      dateRangeInput(ns("dateRange"),
        label = NULL,
        start = min(impfungenRaw$datum),
        end = max(impfungenRaw$datum),
        min = min(impfungenRaw$datum),
        max = max(impfungenRaw$datum),
        format = "d. M yyyy",
        weekstart = 1,
        language = "de",
        separator = "bis"
      ),
      checkboxInput(ns("showNumbers"),
        label = "Zahlenwerte anzeigen",
        value = FALSE
      )
    ),
  
    fluidRow(
      box(
        title = "Geimpfte Personen (Erst-/Zweitgeimpfte)",
        highchartOutput(ns("geimpfte"), height = 300)
      ),
      box(
        title = "Verabreichte Impfdosen",
        highchartOutput(ns("impfdosen"), height = 300)
      ),
      box(
        title = "Online-Registrierungen",
        tagList(
          highchartOutput(ns("onlineanmeldungen"), height = 300),
          HTML("Noch nicht angemeldet? Hier geht's zur bayerischen Impfregistrierung: <a href=\"https://impfzentren.bayern/citizen\">https://impfzentren.bayern/citizen</a>")
        )
      ),
    ),

    fluidRow(
      box(
        title = "Datengrundlage und Methodik",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        tagList(
          p(HTML("Datengrundlage sind die Corona-Pressemeldungen des <a href = \"https://lra-ebe.de/\">Landratsamts Ebersberg</a> (<a href=\"https://lra-ebe.de/aktuelles/aktuelle-meldungen/corona-virus-aktuelle-pressemeldungen-0121/\">Aktuelle Informationen</a>, <a href=\"https://lra-ebe.de/aktuelles/informationen-zum-corona-virus/corona-pressearchiv/\">Corona-Pressearchiv</a>). Dort wird in unregelmäßigen Abständen die Zahl der verabreichten Erst- und Zweitimpfungen sowie die Anzahl der über das <a href=\"https://impfzentren.bayern/citizen\">Online-Portal</a> registrierten Landkreisbürger*innen veröffentlicht.")),
        ),
      ),
    ),
  )
}


# Define the server logic for a module
server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      setBookmarkExclude(c("dateRange"))

      getDateScale <- function() {
        list(
          scale_x_date(
            name = NULL,
            breaks = breaks_pretty(8),
            date_minor_breaks = "1 days",
            date_labels = "%d.%m.",
            expand = expansion(add = c(0.5, 1))
          ),
          coord_cartesian(xlim = c(input$dateRange[1], input$dateRange[2]))
        )
      }

      getYScale <- function() {
        scale_y_continuous(
          name = NULL,
          breaks = breaks_pretty(5),
          expand = expansion(mult = c(0.02, 0.1))
        )
      }

      output$valueBoxErstimpfungen <- renderValueBox({
        lastRow <- impfungenRaw %>% filter(!is.na(erstimpfungen)) %>% slice_tail()
        valueBox(
          lastRow$erstimpfungen,
          paste("Geimpfte (mind. Erstimpfung, Stand:\u00A0", format(lastRow$datum, "%d.%m.%Y"), ")", sep = ""),
          color = "purple",
          icon = icon("user-check")
        )
      })

      output$valueBoxImpfquote <- renderValueBox({
        lastRow <- impfungenRaw %>% filter(!is.na(erstimpfungen)) %>% slice_tail()
        valueBox(
          paste(format(round(lastRow$erstimpfungen / einwohnerZahlLkEbe * 100, 1), nsmall = 1), "%", sep = ""),
          paste("der Bevölkerung ist geimpft (Stand:\u00A0", format(lastRow$datum, "%d.%m.%Y"), ")", sep = ""),
          color = "purple",
          icon = icon("percent")
        )
      })

      output$valueBoxStand <- renderValueBox({
        lastRow <- impfungenRaw %>% filter(!is.na(onlineanmeldungen)) %>% slice_tail()
        valueBox(
          paste(format(round(lastRow$onlineanmeldungen / einwohnerZahlLkEbe * 100, 1), nsmall = 1), "%", sep = ""),
          paste("der Bevölkerung ist online registriert (Stand:\u00A0", format(lastRow$datum, "%d.%m.%Y"), ")", sep = ""),
          color = "purple",
          icon = icon("laptop")
        )
      })
  
      output$geimpfte <- renderHighchart({
        # hchart(fallzahlen, "column", hcaes(x=datum, y=aktuell))
        hc <- highchart() %>% 
          hc_chart(animation=FALSE) %>% 
          hc_plotOptions(series=list(dataLabels=list(enabled=input$showNumbers))) %>%
          hc_xAxis(labels = list(format="{value:%d.%m.%Y}<br>")) %>%
          hc_scrollbar(enabled=TRUE, liveRedraw=TRUE) %>%
          hc_rangeSelector(enabled = TRUE, inputDateFormat="%d.%m.%Y", selected=0) %>%
          # hc_navigator(enabled=TRUE) %>%
          hc_legend(enabled=FALSE) %>%
          hc_tooltip(headerFormat="{point.key:%d.%m.%Y}<br>", shared=TRUE) %>%
          hc_add_series(impfungenRaw, "area", hcaes(x=datum, y=erstimpfungen), name="Erstimpfungen", animation=FALSE, connectNulls=TRUE) %>%
          hc_add_series(impfungenRaw, "area", hcaes(x=datum, y=zweitimpfungen), name="Zweitimpfungen", animation=FALSE, connectNulls=TRUE)
      })
  
      output$impfdosen <- output$aktuell <- renderHighchart({
        hc <- highchart() %>% 
          hc_chart(animation=FALSE) %>% 
          hc_plotOptions(series=list(dataLabels=list(enabled=input$showNumbers))) %>%
          hc_xAxis(labels = list(format="{value:%d.%m.%Y}<br>")) %>%
          hc_scrollbar(enabled=TRUE, liveRedraw=TRUE) %>%
          hc_rangeSelector(enabled = TRUE, inputDateFormat="%d.%m.%Y", selected=0) %>%
          # hc_navigator(enabled=TRUE) %>%
          hc_legend(enabled=FALSE) %>%
          hc_tooltip(headerFormat="", pointFormat="{point.x:%d.%m.%Y}: <b>{point.y}</b>") %>%
          hc_add_series(impfungenRaw, "line", hcaes(x=datum, y=erstimpfungen+zweitimpfungen), name="Impfdosen", animation=FALSE, connectNulls=TRUE)
      })

      output$onlineanmeldungen <- output$aktuell <- renderHighchart({
        hc <- highchart() %>% 
          hc_chart(animation=FALSE) %>% 
          hc_plotOptions(series=list(dataLabels=list(enabled=input$showNumbers))) %>%
          hc_xAxis(labels = list(format="{value:%d.%m.%Y}<br>")) %>%
          hc_scrollbar(enabled=TRUE, liveRedraw=TRUE) %>%
          hc_rangeSelector(enabled = TRUE, inputDateFormat="%d.%m.%Y", selected=0) %>%
          # hc_navigator(enabled=TRUE) %>%
          hc_legend(enabled=FALSE) %>%
          hc_tooltip(headerFormat="", pointFormat="{point.x:%d.%m.%Y}: <b>{point.y}</b>") %>%
          hc_add_series(impfungenRaw, "line", hcaes(x=datum, y=onlineanmeldungen), name="Anmeldungen", animation=FALSE, connectNulls=TRUE)
      })
    }
  )
}
