source(
  "/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_2/Skript_aufgabe_2.R"
)
library(writexl)
# leeres Dataframe erstellen
prediction_data_spending <- data.frame()

View(combined_dataset_spendings_with_predicts)

for (country in unique_locations) {
  filtered_data_avg_spending_by_person <-
    avg_spending_by_person[avg_spending_by_person$LOCATION == country,]
  
  # Erstellen eines linearen Regressionsmodell basierend auf den letzten Jahren
  # Die Variaböe Spending.value wird anhand der Jahre(Time) erklärt
  lm_model <- lm(Spending_Value~ TIME, data = filtered_data_avg_spending_by_person)
  
  # Generieren Sie Vorhersagen für die nächsten zwei Jahre
  future_years <- data.frame(TIME = max(filtered_data_avg_spending_by_person$TIME) + 1:2)
  predictions <- predict(lm_model, newdata = future_years)
  
  # Speichern Sie die Vorhersagen
  prediction_data_spending <- rbind(prediction_data_spending, data.frame(LOCATION = country, TIME = future_years$TIME, Spending_Value = predictions))
}
# avg_spending_
combined_dataset_spendings_with_predicts <- data.frame()
# Hinzufügen der Vorhersagen zu Originaldaten
combined_dataset_spendings_with_predicts <- rbind(prediction_data_spending, avg_spending_by_country_year)
View(combined_dataset_spendings_with_predicts)
write.csv2(prediction_data,'/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_3/Prediction_Haushaltsausgaben_2023_2024.csv')




# Mit Predictions die GGplots erstellen
for (country in unique_countries) {
  # Filter die Daten für das aktuelle Land
  filtered_data_predict_avg_spending_by_person <-
    combined_dataset_spendings_with_predicts[combined_dataset_spendings_with_predicts$LOCATION == country,]
  
  # Erstelle die Zeitreihengrafik für das aktuelle Land
  plot_avg_predict_spending_by_person <-
    ggplot(data = filtered_data_predict_avg_spending_by_person, aes(x = TIME, y = Spending_Value)) +
    geom_line(color = "blue", na.rm = TRUE) +

    labs(
      title = paste("Durchschnittliche Ausgaben pro Kopf mit Prognosen", country),
      x = "Jahr",
      y = "Ausgaben pro Kopf in US dollars"
    )
  # Jedes Land als eigenes Plot speichern
  ggsave(
    filename = paste0(
      "/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_3/Plots mit Prognosen/Ausgaben pro Kopf mit Prognosen/Ausgaben_pro_Kopf_mit_Prognosen_für_",
      country,
      ".png"
    ),
    plot = plot_avg_predict_spending_by_person
  )
}























prediction_data_network_usage <- data.frame()

combined_dataset_network_with_predicts <- rbind(internet_access_data)
for (country in unique_locations) {
  filtered_data <-
    internet_access_data[internet_access_data$LOCATION == country,]
  
  combined_dataset_network_with_predicts <- rbind(filtered_data)
  
  # Erstellen lineares Regressionsmodell basierend auf den letzten Jahren
  lm_model_2 <- lm(Value ~ TIME, data = filtered_data)
  
  # Generieren der Vorhersagen für die nächsten zwei Jahre
  future_years_2 <- data.frame(TIME = max(filtered_data$TIME) + 1:2)
  predictions_2 <- predict(lm_model_2, newdata = future_years_2)
  
  # Speichern der Vorhersagen
  columanName_flag_codes <- "Flag Codes"
  prediction_data_network_usage <- rbind(prediction_data_network_usage, data.frame(LOCATION = country, TIME = future_years_2$TIME, Value = predictions_2))
  
}
combined_dataset_network_with_predicts <- data.frame()
# Hinzufügen der Vorhersagen zu Originaldaten
combined_dataset_network_with_predicts <- rbind(prediction_data_network_usage, internet_access_data[, 1:3])
View(combined_dataset_network_with_predicts)
prediction_data_network_usage$Value <- round(prediction_data_network_usage$Value, digits = 2)
write.csv2(prediction_data_network_usage,'/Users/Privat/IUBH-Fernstudium/Semester/Semester 6/Statistik /Aufgabe_3/Prediction_Internetzugang_2023_2024.csv')