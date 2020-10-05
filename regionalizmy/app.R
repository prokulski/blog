library(tidyverse)
library(lubridate)
library(shiny)
library(shinyjs)
library(leaflet)
library(RMySQL)

# library(GAlogger)
# ga_set_tracking_id("UA-1427545-15")
# ga_set_approval(consent = TRUE)
# ga_collect_pageview("/regiony")


# dla poprawnej obsługi pl literek - m.in. sortowanie
Sys.setlocale("LC_CTYPE", "pl_PL.UTF-8")

# dostęp do bazy
dbname = "test_db"
user = "lemur"
password = "pr0cma1l"
host = "localhost"


# listy do wyboru
majonez <- c("Koronny", "Mikado", "Roleski", "Splendido", "Mosso", "Hellmann\'s", "Kętrzyński", "Kielecki", "Winiary")
majonez <- c("Wybierz", sort(majonez), "inny", "ten, który jest w domu")
names(majonez) <- majonez

ketchup <- c("Krokus", "Roleski", "Pudliszki", "Włocławek", "Kotlin", "Hellmann\'s", "Heinz", "Kamis", "Rybak")
ketchup <- c("Wybierz", sort(ketchup), "inny", "ten, który jest w domu")
names(ketchup) <- ketchup

ekstraklasa <- c("Legia Warszawa", "Jagiellonia Białystok", "Lech Poznań", "Górnik Zabrze",
                 "Wisła Płock", "Wisła Kraków", "Zagłębie Lubin", "Korona Kielce", "Cracovia",
                 "Śląsk Wrocław", "Pogoń Szczecin", "Arka Gdynia", "Lechia Gdańsk",
                 "Piast Gliwice", "Miedź Legnica", "Zagłębie Sosnowiec")
ekstraklasa <- c("Wybierz", sort(ekstraklasa), "żadnej")
names(ekstraklasa) <- ekstraklasa

# łączymy się z bazą
dbCon <- function() {
  # łączymy się z bazą
  sterownik <- dbDriver("MySQL")
  polaczenie <- dbConnect(sterownik, dbname = dbname, user = user, password = password, host = host)

  # to po to, żeby pl literki się dobrze zapisywały
  dbSendQuery(polaczenie, "SET NAMES utf8;")
  dbSendQuery(polaczenie, "SET CHARACTER SET utf8;")
  dbSendQuery(polaczenie, "SET character_set_connection=utf8;")

  return(polaczenie)
}

# odłączenie od bazy
dbDisCon <- function(polaczenie) {
  dbDisconnect(polaczenie)
}


# zapisanie danych to DB
saveData <- function(df, polaczenie) {
  dbWriteTable(polaczenie, "regionalizmy", df, append = TRUE, row.names = FALSE)
}


# pobranie aktualnych wyników
readData <- function(polaczenie) {
  df <- dbGetQuery(polaczenie, 'SELECT "majonez" AS typ, majonez AS nazwa, COUNT(*) AS liczba FROM regionalizmy GROUP BY majonez
                    UNION
                    SELECT "ketchup" AS typ, ketchup AS nazwa, COUNT(*) AS liczba FROM regionalizmy GROUP BY ketchup
                    UNION
                    SELECT "ekstraklasa" AS typ, ekstraklasa AS nazwa, COUNT(*) AS liczba FROM regionalizmy GROUP BY ekstraklasa
                    UNION
                    SELECT "wszystkie" AS typ, "wszystkie" AS nazwa, COUNT(*) AS liczba FROM regionalizmy;
                    ')

}


# wygląd aplikacji (najprostszy)
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  # kod GA
  tags$head(includeScript("ga_tracking.js")),

  titlePanel("Ankieta preferencji według miejsca zamieszkania"),

  mainPanel(
    div(id = "myapp",
        # mapa
        h4("Zaznacz gdzie mieszkasz:"),
        leafletOutput("mymap", height = "400px", width = "500px"),

        selectInput("majonez_input", "Wybierz ulubiony majonez:", majonez),
        selectInput("ketchup_input", "Wybierz ulubiony ketchup:", ketchup),
        selectInput("ekstraklasa_input", "Wybierz drużynę, której kibicujesz w Ekstraklasie:", ekstraklasa),

        p("Aby wysłać wyniki trzeba wybrać odpowiedź na każde z trzech pytań oraz zaznaczyć miejsce w którym jesteś na mapie."),

        actionButton("goButton", "Wyślij ankietę!")
    ),
    div(id = "thanks", style = "width:550px;margin:5px;",
        h4("Dziękuję!"),
        p("Dotychczasowe wyniki popularności:"),
        plotOutput("majonez_wykres", width = "500px"),
        plotOutput("ketchup_wykres", width = "500px"),
        plotOutput("ekstraklasa_wykres", width = "500px"),
        p(HTML(paste0("Odpowiedzi zebrane w ankiecie zostaną podsumowane (także geograficznie) i opublikowane",
                      "<ul><li>",
                      a(href = "http://prokulski.net/?utm_source=shiny&utm_medium=regiony", "na blogu Łukasza Prokulskiego"),
                      "</li><li>",
                      a(href = "http://fb.com/DaneAnalizy", "na fanpage'u Dane i Analizy"),
                      "</li></ul>"))),
        p("Zaglądaj w te miejsca, aby nie przegapić wyników!"),
        HTML('<iframe width="500" height="500" style="border: none; overflow: hidden;" src="https://www.facebook.com/plugins/page.php?href=https%3A%2F%2Fwww.facebook.com%2FDaneAnalizy%2F&amp;tabs=timeline&amp;width=500&amp;height=500&amp;small_header=true&amp;adapt_container_width=true&amp;hide_cover=true&amp;show_facepile=false&amp;appId=431407730559418" scrolling="no" frameborder="0" allowtransparency="true"></iframe>')
    ),
    width = 12
  )
)


# mechanika aplikacji
server <- function(input, output) {

  # button na początek nieaktywny
  shinyjs::disable("goButton")

  # podziękowania showane
  shinyjs::hide("thanks")

  # początkowe ustawienie mapy
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%
      # środek mapki to domyślny marker
      # addMarkers(lng = 19.15, lat = 52.19) %>%
      # mapa pokazuje całą Polskę na początek
      setView(lng = 19.15, lat = 52.19, zoom = 6)
  )


  # czekamy na kliknięcie w mapę
  click_point <- eventReactive(input$mymap_click, {
    # współrzędne kliknięcia
    input$mymap_click
  })


  # pobieramy dane z pól i mapy
  wynik <- reactive({
    # zapisanie danych do bazy
    click_lat <- click_point()$lat
    click_long <- click_point()$lng
    majonez_wybor <- input$majonez_input
    ketchup_wybor <- input$ketchup_input
    ekstraklasa_wybor <- input$ekstraklasa_input

    return(list(done = TRUE,
                tabela = tibble(timestamp = now(),
                                majonez = majonez_wybor,
                                ketchup = ketchup_wybor,
                                ekstraklasa = ekstraklasa_wybor,
                                long = click_long,
                                lat = click_lat)))
  })


  # sprawdzamy czy pola są wypełnione
  # można kliknąć guzik jeśli wszystko jest wypełnione
  observe({
    if(wynik()$done && wynik()$tabela$majonez != "Wybierz" && wynik()$tabela$ketchup != "Wybierz" && wynik()$tabela$ekstraklasa != "Wybierz") {
      shinyjs::enable("goButton")
    } else {
      shinyjs::disable("goButton")
    }
  })



  # kliknięcie w mapę - przesunięcie markera
  observe({
    # współrzędne kliknięcia
    click_lat <- click_point()$lat
    click_long <- click_point()$lng

    # na mapie...
    leafletProxy("mymap") %>%
      # czyścimy markery
      clearMarkers() %>%
      # ustawiamy marker w miejscu gdzie kliknięto
      addMarkers(lng = click_long, lat = click_lat)
  })


  # kliknięcie w guzik
  observeEvent(input$goButton, {
    if(wynik()$done) {
      # połączenie do bazy
      pol <- dbCon()

      # zapisanie danych do bazy
      saveData(wynik()$tabela, pol)

      # pobranie już zebranych danych
      dane <- readData(pol)

      # rozłączenie z bazą
      dbDisCon(pol)


      # chowamy całą aplikację
      shinyjs::hide("myapp")

      # uzupełniamy wykresy
      n_glosow <- dane %>% filter(typ == "wszystkie") %>% pull(liczba)

      output$majonez_wykres <- renderPlot({
        dane %>%
          filter(typ == "majonez") %>%
          arrange(liczba, nazwa) %>%
          mutate(nazwa = fct_inorder(nazwa)) %>%
          ggplot() +
          geom_col(aes(nazwa, liczba), fill = "lightgreen") +
          coord_flip() +
          theme_minimal() +
          labs(title = paste0("Najpopularniejszy majonez (oddanych ", n_glosow, " głosów)"),
               x = "", y = "liczba głosów")
      })

      output$ketchup_wykres <- renderPlot({
        dane %>%
          filter(typ == "ketchup") %>%
          arrange(liczba, nazwa) %>%
          mutate(nazwa = fct_inorder(nazwa)) %>%
          ggplot() +
          geom_col(aes(nazwa, liczba), fill = "lightgreen") +
          coord_flip() +
          theme_minimal() +
          labs(title =  paste0("Najpopularniejszy ketchup (oddanych ", n_glosow, " głosów)"),
               x = "", y = "liczba głosów")

      })
      output$ekstraklasa_wykres <- renderPlot({
        dane %>%
          filter(typ == "ekstraklasa") %>%
          arrange(liczba, nazwa) %>%
          mutate(nazwa = fct_inorder(nazwa)) %>%
          ggplot() +
          geom_col(aes(nazwa, liczba), fill = "lightgreen") +
          coord_flip() +
          theme_minimal() +
          labs(title =  paste0("Najpopularniejsza drużyna Ekstraklasy (oddanych ", n_glosow, " głosów)"),
               x = "", y = "liczba głosów")

      })

      # pokazujemy podziękowania
      shinyjs::show("thanks")
    }
  })
}

shinyApp(ui = ui, server = server)
