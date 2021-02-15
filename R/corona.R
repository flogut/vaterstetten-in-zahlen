library(readr)
library(dplyr)
library(tidyr)
library(scales)

einwohnerZahl <- 24404

fallzahlenRaw <- read_delim(
  file = "data/lra-ebe-corona/fallzahlenVat.csv",
  delim = ",",
  col_names = TRUE,
  col_types = cols(
    datum = col_date(format = "%Y-%m-%d"),
    kumulativ = col_integer(),
    aktuell = col_integer()
  )
)

fallzahlen <- fallzahlenRaw %>%
  mutate(neuinfektionen = kumulativ - lag(kumulativ, 1)) %>%
  complete(datum = seq(min(datum), max(datum) + 1, "days"), fill = list(neuinfektionen = 0)) %>%
  mutate(neuinfektionen = c(neuinfektionen[-n()], NA)) %>%
  mutate(inzidenz7 = lag(cumsum(neuinfektionen) - lag(cumsum(neuinfektionen), 7)) / einwohnerZahl * 100000) %>%
  mutate(neuinfektionen = c(NA, neuinfektionen[-1]))

ui <- function(request, id) {
  ns <- NS(id)
  tagList(
    h2("Corona-Fallzahlen in Vaterstetten"),

    fluidRow(
      box(
        title = "Disclaimer",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        tagList(
          p(HTML("<strong>Alle Angaben ohne Gewähr.</strong> Bitte halten Sie sich an die Vorgaben des zuständigen Gesundheitsamts. Die hier veröffentliche 7-Tage-Inzidenz ist <u>nicht</u> relevant für lokale Corona-Beschränkungen. Geringe Zahlen in Vaterstetten sind nicht automatisch ein Beweis für eine geringe Infektionsgefahr in Vaterstetten."))
        )
      )
    ),

    fluidRow(
      valueBoxOutput(ns("valueBoxAktuell")),
      valueBoxOutput(ns("valueBoxInzidenz")),
      valueBoxOutput(ns("valueBoxStand"))
    ),

    flowLayout(
      checkboxInput(ns("showNumbers"),
        label = "Zahlenwerte anzeigen",
        value = FALSE
      )
    ),

    fluidRow(
      box(
        title = "Neuinfektionen (absolut)",
        highchartOutput(ns("neuinfektionen"), height = 300)
      ),
      box(
        title = "7-Tages-Inzidenz pro 100.000 Einwohner",
        highchartOutput(ns("inzidenz7"), height = 300)
      ),
      box(
        title = "Aktuelle Fälle (absolut)",
        highchartOutput(ns("aktuell"), height = 300)
      ),
    ),

    fluidRow(
      box(
        title = "Datengrundlage und Methodik",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        tagList(
          p(HTML("Datengrundlage sind die SARS-CoV-2-Fallzahlen des <a href = \"https://lra-ebe.de/\">Landratsamts Ebersberg</a> (<a href=\"https://lra-ebe.de/aktuelles/aktuelle-meldungen/corona-virus-aktuelle-pressemeldungen-0121/\">Aktuelle Informationen</a>, <a href=\"https://lra-ebe.de/aktuelles/informationen-zum-corona-virus/corona-pressearchiv/\">Corona-Pressearchiv</a>). Das Gesundheitsamt veröffentlicht an jedem Werktag die kumulativen Fallzahlen und aktuellen Fälle, aufgeschlüsselt nach Kommunen, jeweils zum Stand des vorherigen Tages um 16 Uhr. Da die Zahlen nur in Form einer Grafik und nicht in einem maschinenlesbaren Format vorliegen, müssen diese händisch für dieses Projekt eingetragen werden. Auch wenn auf eine größtmögliche Sorgfalt geachtet wird, besteht daher beim Übertragen natürlich die Gefahr von Tippfehlern.")),
          p("Für die Berechnung der 7-Tage-Inzidenz für einen Tag X werden die Neuinfektionen der 7 vorangegangenen Tage, nicht aber des Tages X summiert. Das entspricht der Berechnungsweise des RKI. So ist es möglich, für den heutigen Tag eine 7-Tage-Inzidenz anzugeben, obwohl der Datenstand des Gesundheitsamtes bei gestern liegt.")
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

      output$valueBoxAktuell <- renderValueBox({
        lastRowWithAktuell <- fallzahlen %>% filter(!is.na(aktuell)) %>% slice_tail()
        valueBox(
          lastRowWithAktuell$aktuell,
          "Aktuelle Fälle",
          color = "purple",
          icon = icon("user-check")
        )
      })

      output$valueBoxInzidenz <- renderValueBox({
        lastRow <- fallzahlen %>% slice_tail()
        valueBox(
          format(round(lastRow$inzidenz7, 1), nsmall = 1),
          "7-Tages-Inzidenz",
          color = "purple",
          icon = icon("chart-line")
        )
      })

      output$valueBoxStand <- renderValueBox({
        lastRowWithAktuell <- fallzahlen %>% filter(!is.na(aktuell)) %>% slice_tail()
        valueBox(
          format(lastRowWithAktuell$datum, "%d. %b %Y"),
          "Datenstand des Gesundheitsamtes",
          color = "purple",
          icon = icon("calendar-day")
        )
      })

      output$neuinfektionen <- renderHighchart({
        hc <- highchart() %>% 
          hc_chart(animation=FALSE) %>% 
          hc_plotOptions(series=list(dataLabels=list(enabled=input$showNumbers))) %>%
          hc_xAxis(labels = list(format="{value:%d.%m.%Y}<br>")) %>%
          hc_scrollbar(enabled=TRUE, liveRedraw=TRUE) %>%
          hc_rangeSelector(enabled = TRUE, inputDateFormat="%d.%m.%Y", selected=0) %>%
          # hc_navigator(enabled=TRUE) %>% # Shows a zoom bar below the plot, needs more space than 400px
          hc_legend(enabled=FALSE) %>%
          hc_tooltip(headerFormat="", pointFormat="{point.x:%d.%m.%Y}: <b>{point.y}</b>") %>%
          hc_add_series(fallzahlen, "column", hcaes(x=datum, y=neuinfektionen), name="Neuinfektionen", animation=FALSE)
      })
      
      output$inzidenz7 <- renderHighchart({
        hc <- highchart() %>% 
          hc_chart(animation=FALSE) %>% 
          hc_plotOptions(series=list(dataLabels=list(enabled=input$showNumbers, format="{point.y:.0f}"))) %>%
          hc_xAxis(labels = list(format="{value:%d.%m.%Y}<br>")) %>%
          hc_scrollbar(enabled=TRUE, liveRedraw=TRUE) %>%
          hc_rangeSelector(enabled = TRUE, inputDateFormat="%d.%m.%Y", selected=0) %>%
          # hc_navigator(enabled=TRUE) %>%
          hc_legend(enabled=FALSE) %>%
          hc_tooltip(headerFormat="", pointFormat="{point.x:%d.%m.%Y}: <b>{point.y:.0f}</b>") %>%
          hc_add_series(fallzahlen, "line", hcaes(x=datum, y=inzidenz7), name="Inzidenz", animation=FALSE, connectNulls=TRUE)
      })
      
      output$aktuell <- renderHighchart({
        hc <- highchart() %>% 
          hc_chart(animation=FALSE) %>% 
          hc_plotOptions(series=list(dataLabels=list(enabled=input$showNumbers))) %>%
          hc_xAxis(labels = list(format="{value:%d.%m.%Y}<br>")) %>%
          hc_scrollbar(enabled=TRUE, liveRedraw=TRUE) %>%
          hc_rangeSelector(enabled = TRUE, inputDateFormat="%d.%m.%Y", selected=0) %>%
          # hc_navigator(enabled=TRUE) %>%
          hc_legend(enabled=FALSE) %>%
          hc_tooltip(headerFormat="", pointFormat="{point.x:%d.%m.%Y}: <b>{point.y}</b>") %>%
          hc_add_series(fallzahlen, "line", hcaes(x=datum, y=aktuell), name="Aktuell", animation=FALSE, connectNulls=TRUE)
      })
    }
  )
}
