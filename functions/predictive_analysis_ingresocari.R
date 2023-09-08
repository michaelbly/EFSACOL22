# Load the required library for regression
library(caret)

# Load your dataset
data <- df

data$ingreso_pp <- as.numeric(data$ingreso_pp)
data$ingreso_pp <- ifelse(data$ingreso_pp > 500000,500000,data$ingreso_pp)
data$ingreso_pp <- ifelse(is.na(data$ingreso_pp),500000,data$ingreso_pp)
summary(data$ingreso_pp)

# Split the data into training and testing sets
trainIndex <- createDataPartition(data$cari, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Build the regression model
regModel <- train(cari ~ ingreso_pp, data = trainData, method = "gam", na.action = na.pass)

# Make predictions on a new set of observations
new_obs <- data.frame(ingreso_pp = c(1, 100500, 300000000))  # Replace with your new observations
new_predictions <- predict(regModel, newdata = new_obs)

# Print the predicted scores for the new observations
cat("Predicted scores for new observations:\n")
print(new_predictions)



intercept <- 4
ggplot(data, aes(x = ingreso_pp, y = cari)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Ingreso per cápita", y = "Cari") +
  ggtitle("Regression between Cari and Ingreso per cápita")
















municipio <- read.csv("data/raw_data/cocalero_study/dataset_cocalero_municipio.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")
#municipio$ingreso_pp <- as.numeric(as.factor(municipio$ingreso_pp))
#municipio_ingreso <- as.data.frame(municipio$ingreso_mensual)
municipio$ingreso_pp <- municipio$ingreso_pp * 10

library(mgcv)
data <- data.frame(ingreso_pp = c(0, 42410, 82410, 115720, 166010, 1809260),
                   cari = c(4, 3.5, 2.9, 2.2, 1.8, 1))  # Replace with your new observations


# Build the regression model with the centered response variable
model <- loess(cari ~ ingreso_pp, data = data)


# Define new observations for "ingreso_pp"
new_obs <- data.frame(ingreso_pp = c(0, 250000, 500000))  # Replace with your new observations

# Make predictions
prediction_cari <- predict(model, newdata = municipio)
prediction_cari <- ifelse(prediction_cari < 1,1,prediction_cari)

# Print the predicted scores for the new observations
cat("Predicted scores for new observations:\n")
print(prediction_cari)
summary(prediction_cari)

predictions_municipio <- cbind(municipio, prediction_cari)
predictions_municipio$cari_categoria <- round(predictions_municipio$prediction_cari,0)
predictions_municipio <- predictions_municipio %>% dplyr::mutate(cari_categoria = case_when(
  predictions_municipio$cari_categoria == 1 ~ "seguridad_alimentaria",
  predictions_municipio$cari_categoria == 2 ~ "seguridad_alimentaria_marginal",
  predictions_municipio$cari_categoria == 3 ~ "inseguridad_alimentaria_moderada",
  predictions_municipio$cari_categoria == 4 ~ "inseguridad_alimentaria_severa"))

predictions_municipio$prediction_cari <- round(predictions_municipio$prediction_cari, 1)
write.csv(predictions_municipio, "output/dataset/predictions_cari_municipio.csv")


# Create a scatter plot with regression line
ggplot(data, aes(x = ingreso_pp, y = cari)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression", x = "Ingreso_pp", y = "Cari")



# Set the scipen option to suppress scientific notation
options(scipen = 999, digits = 10)

# Create a histogram of "ingreso_pp"
ggplot(municipio, aes(x = ingreso_pp)) +
  geom_histogram(binwidth = 100) +
  labs(title = "Distribution of ingreso_pp", x = "ingreso_pp", y = "Frequency")


summary(df$ingreso_pp)
summary(municipio$ingreso_pp)
