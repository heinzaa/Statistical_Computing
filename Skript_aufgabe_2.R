source(
  "/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_1/Skript_aufgabe_1.R"
)

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)



# Data frames nach Länder aufsteigend sortieren
internet_access_data <- as.data.frame(internet_access_data)
View(internet_access_data)
sorted_internet_access_data <-
  internet_access_data[order(internet_access_data$LOCATION, internet_access_data$TIME), ]
View(sorted_internet_access_data)
sorted_avg_spending_by_country_year <-
  avg_spending_by_country_year[order(avg_spending_by_country_year$LOCATION,
                                     avg_spending_by_country_year$TIME), ]

#unique_locations <- unique_locations[order(unique_locations)]

# Ein leeres Dataset erstellen, um die Korrelationswerte für jedes Land zu speichern
correlation_data <- data.frame(Land = character(), Korrelation = numeric()) 


sorted_internet_access_data <-
  as.data.frame(sorted_internet_access_data)
sorted_avg_spending_by_country_year <-
  as.data.frame(sorted_avg_spending_by_country_year)

View(sorted_internet_access_data)


# Die Vektoren brauchen für eine Korrelation die gleiche Länge ->
# deswegen immer nur die Jahre nehmen - die beide Datensätze enthalten
# Loope über die Länder - die sind bei deiden gleich
# kann maximal bei 2005 anfangen
# ich merge die Daten an den Jahren & Locations !!!
merged_df <-
  merge(
    sorted_internet_access_data,
    sorted_avg_spending_by_country_year,
    by = c("TIME", "LOCATION"),
    all = FALSE
  )
sorted_merged_df <-
  merged_df[order(merged_df$LOCATION, merged_df$TIME), ]
unique_locations <- unique(sorted_merged_df$LOCATION)
unique_countries_vector <- unique_locations

for (country in unique_countries_vector) {
  # Filtere die Daten für das aktuelle Land
  filtered_merged_data_by_country <-
    sorted_merged_df[sorted_merged_df$LOCATION == country, ]
  # Berechnung der Korrelation
  correlation <- cor(
    filtered_merged_data_by_country$Value,
    filtered_merged_data_by_country$Spending_Value,
    use = "pairwise.complete.obs"
  )
  # Ein neuen Datensatz erstellen, um die Ergebnisse hinzuzufügen
  new_row <- data.frame(Land = country, Korrelation = correlation)
  correlation_data <- rbind(correlation_data, new_row)
  # Streudiagramm erstellen
  plot_correlation_network_household <- ggplot(data = filtered_merged_data_by_country, aes(x = Value, y = Spending_Value)) +
    geom_point(size = 5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Hinzufügen der Regressionsgerade
    geom_line(colour = "green")+
    labs(title = paste("Streudiagramm für ",country," mit Korrelation r: ", round(correlation, 3)),
         x = "Breitbandzugang in %",
         y = "Haushaltsausgaben in US-Dollar") +
    theme_bw()
  # Dateipfad für das Speichern der Plots
  file_path <- paste0(
    "/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_2/Korrelation Plots/Korrelation_Breitbandzugang_vs_Haushaltsausgaben_für_",country,".png"
  )
  # Speichern das Streudiagramm
  ggsave(filename = file_path, plot = plot_correlation_network_household)
}

# Sortieren Sie das correlation_data-Dataset nach der Korrelationsspalte (absteigende Reihenfolge)
correlation_data <- correlation_data[order(correlation_data$Korrelation, decreasing = TRUE), ]
# Speichern Sie das sortierte Dataset in eine CSV-Datei
write.csv(correlation_data, file = "Korrelationswerte_nach_Land.csv", row.names = FALSE)