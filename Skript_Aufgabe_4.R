# Preisgestaltung

source(
  "/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_3/Aufgabe_3.R"
)

read_delim(
  "/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_1/internet_access_csv_2005_2022.csv",
  delim = ";"
)

# Berechnung des Preisfaktor
# PF = Durchschnittliche Haushaltsausgaben / 12€
# Berechnet die Prozentzahl, wie viel % der Ausgaben 

# https://data.oecd.org/eduresource/education-spending.htm#indicator-chart
# https://data.oecd.org/eduresource/private-spending-on-education.htm -> jetzt in das Verhältnis mit den gesamten haushaltsausgaben setzen
# das ergibt einen Faktor - dann berechnen was die 12€ in Deutschland an diesem Prozentsatz ausmachen 
# das ergibt dann den Faktorden man multipliziert

# Beispiel:
#P Deutschland = (durchschnittliche Hauhsaltsausgaben / 12€ für die App) * 100
# dann erhält man einen Preisfaktor
# diese Prozentzahl kann man dann mit den durchschnittlichen Hauhsaltsausgaben für andere Länder berechnen
# Leider gibt es keine Daten dazu, wie viel Geld private Haushalte für Lehrbücher, Nachhilfe etc ausgubt. SOnst könnte man die 12€ mit dieser Summe
# ins Verhältnis setzen
# 1,2% auf die Bildungsausgabne sind die menschen durchschnittlich bereit zu bezahlen für die App
# Inflation noch einberechnen & evtl. Währungseffekte
# BSP: durschnittliche Haushaltsausgaben: 10.000€
# P-Faktor: 12€/10.000€ = 0,12%
# Frankreich: Haushaltsaugsaben: 9500€
# 0,12% = x / 9500€            / *9500€ :100% 
# 11,4€ = x

app_price_germany <- 12 
average_spending_germany <- combined_dataset_spendings_with_predicts[combined_dataset_spendings_with_predicts$LOCATION == "DEU" &combined_dataset_spendings_with_predicts$TIME == 2022, "Spending_Value"]
price_factor <- app_price_germany / average_spending_germany

# Erstelle ein leeres Data Frame app_price_for_country
app_price_for_country <- data.frame()
helper_row <- data.frame()

# sortiere Datensatz nach land und Jahr 
combined_dataset_spendings_with_predicts <- combined_dataset_spendings_with_predicts[order(combined_dataset_spendings_with_predicts$LOCATION, combined_dataset_spendings_with_predicts$TIME ),]
combined_dataset_spendings_with_predicts <- as.data.frame(combined_dataset_spendings_with_predicts)
View(combined_dataset_spendings_with_predicts)
# Schleife durch die Datensätze in avg_spending_by_country
for (i in 1:nrow(combined_dataset_spendings_with_predicts)) {
 # item <- combined_dataset_spendings_with_predicts[i,]  # Den aktuellen Datensatz auswählen
  
  time <- combined_dataset_spendings_with_predicts[i,]$TIME
  loc <- combined_dataset_spendings_with_predicts[i,]$LOCATION
  val <- combined_dataset_spendings_with_predicts[i,]$Spending_Value
  
  # Erstelle einen neuen Datensatz, um ihn in app_price_for_country einzufügen
  helper_row <- data.frame(
    Location = loc,
    TIME = time,
    App_price = (price_factor * val)
  )
  
  
  # Füge den neuen Datensatz zu app_price_for_country hinzu
  app_price_for_country <- rbind(app_price_for_country, helper_row)
}
View(app_price_for_country)
# Ändere den Spaltennamen von "avg_spending" auf "app_price"
colnames(app_price_for_country)[colnames(app_price_for_country) == "Avg_Spending"] <- "App_price"
# App Preis auf 2 Nachkommastellen runden
app_price_for_country$App_price <- as.double(round(app_price_for_country$App_price, 2))
# Nach absteigenden Apppreisprognosen sortieren

# Gruppieren Sie den Data Frame nach "LOCATION" und wählen Sie das höchste Jahr
result <- app_price_for_country %>%
  group_by(Location) %>%
  summarize(TIME = max(TIME), App_price = max(App_price))
result <- result[order(-result$App_price), ]
# Erstelle das Balkendiagramm mit den ersten 11 Ländern, da in Deutschland die App bereits existiert
price_calculation_plot <- ggplot(head(result,11), aes(x = Location, y = App_price)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Land", y = "Preisprognose in US-Dollar") +
  theme_light() +
  coord_flip()  # Um die Balken horizontal anzuzeigen
# Speichern das Streudiagramm      
ggsave(filename = '/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_4/Preisplott für die App.png', plot = price_calculation_plot)
# data Frame kann man noch als excel rausschreiben
write.csv2(result,'/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_4/Preisprognose App.csv')




# Eventuell kann man ja noch den PISA Score miteinberechnen - welches Länder noch dringend aufzuholenn haben:
# https://data.oecd.org/pisa/mathematics-performance-pisa.htm