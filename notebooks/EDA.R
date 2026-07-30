#Libraries
library(tidyverse)
library(ggplot2)
library(corrplot)

# Read the data -----------------------------------------------------------------------
setwd("C:/Users/hp/Desktop/FOOD PRODUCTION PROJECT/notebooks")

df <- read_csv(
  "../data/processed/crop_production_clean.csv"
)

head(df)

#OVERVIEW OF THE DATASET --------------------------------------------------------
dim(df)
unique(df$crop)
colSums(is.na(df))

# AVERAGE PRODUCTION ------------------------------------------------------
mean(df$Production_tonnes)
aggregate(Production_tonnes ~ crop,
          data = df,
          mean)
prod <- aggregate(Production_tonnes ~ crop, data = df, mean)

barplot(
  prod$Production_tonnes,
  names.arg = prod$crop,
  main = "",
  xlab = "Crop",
  ylab = "Production (tonnes)",col="red")


# AVERAGE YIELD -----------------------------------------------------------
yield <-aggregate(Yield_kg_per_ha ~ crop, data = df,mean)
yield
barplot(yield$Yield_kg_per_ha,
        names.arg =yield$crop,
        main ="",
        xlab = "Crop",
        ylab ="Yield (kg/ha)", col = "lightgreen")


# AVERAGE AREA HARVESTED --------------------------------------------------
area <- aggregate(Area_Harvested_ha ~ crop, data = df, mean)
area
barplot(area$Area_Harvested_ha,
        names.arg = area$crop,
        main ="",
        xlab = "Crop",
        ylab ="Area Harvested(ha)", 
        col ="lightblue")


# PRODUCTION OVER TIME ----------------------------------------------------
par(mfrow = c(1, 2))

# Maize
plot(
  Production_tonnes ~ year,
  data = df[df$crop == "Maize", ],
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Year",
  ylab = "Maize Production (tonnes)",
  main = ""
)

# Rice
plot(
  Production_tonnes ~ year,
  data = df[df$crop == "Rice", ],
  type = "l",
  col = "red",
  lwd = 2,
  xlab = "Year",
  ylab = " Rice Production (tonnes)",
  main = ""
)

# Reset plotting layout
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))

# Cassava
plot(
  Production_tonnes ~ year,
  data = df[df$crop == "Cassava", ],
  type = "l",
  col = "darkgreen",
  lwd = 2,
  xlab = "Year",
  ylab = "CassaveProduction (tonnes)",
  main = ""
)

# Yams
plot(
  Production_tonnes ~ year,
  data = df[df$crop == "Yams", ],
  type = "l",
  col = "purple",
  lwd = 2,
  xlab = "Year",
  ylab = " Yam Production (tonnes)",
  main = ""
)

par(mfrow = c(1, 1))

# AREA HARVESTED OVER TIME ------------------------------------------------
par(mfrow = c(1, 2))

# Maize
plot(
  Yield_kg_per_ha ~ year,
  data = df[df$crop == "Maize", ],
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Year",
  ylab = "Area Harvested (Maize) in ha",
  main = ""
)

# Rice
plot(
  Yield_kg_per_ha ~ year,
  data = df[df$crop == "Rice", ],
  type = "l",
  col = "red",
  lwd = 2,
  xlab = "Year",
  ylab = "Area Harvested (Rice) in ha",
  main = ""
)

# Reset plotting layout
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))

# Cassava
plot(
  Yield_kg_per_ha ~ year,
  data = df[df$crop == "Cassava", ],
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Year",
  ylab = "Area Harvested (Cassava) in ha",
  main = ""
)

# Yam
plot(
  Area_Harvested_ha ~ year,
  data = df[df$crop == "Yams", ],
  type = "l",
  col = "red",
  lwd = 2,
  xlab = "Year",
  ylab = "Area Harvested (Yam) in ha",
  main = ""
)

# Reset plotting layout
par(mfrow = c(1, 1))


# Yield Over Time ---------------------------------------------------------
par(mfrow = c(1, 2))

# Maize
plot(
  Yield_kg_per_ha ~ year,
  data = df[df$crop == "Maize", ],
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Year",
  ylab = "Maize Yield (kg/ha)",
  main = ""
)

# Rice
plot(
  Yield_kg_per_ha ~ year,
  data = df[df$crop == "Rice", ],
  type = "l",
  col = "red",
  lwd = 2,
  xlab = "Year",
  ylab = "Rice Yield (kg/ha)",
  main = ""
)

# Reset plotting layout
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))

# Cassava
plot(
  Yield_kg_per_ha ~ year,
  data = df[df$crop == "Cassava", ],
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Year",
  ylab = "Cassava Yield (kg/ha)",
  main = ""
)

# Yam
plot(
  Yield_kg_per_ha ~ year,
  data = df[df$crop == "Yams", ],
  type = "l",
  col = "red",
  lwd = 2,
  xlab = "Year",
  ylab = " Yam Yield (kg/ha)",
  main = ""
)
# Reset plotting layout
par(mfrow = c(1, 1))


# PRODUCTION VS AREA ------------------------------------------------------

plot(
  Production_tonnes ~ Area_Harvested_ha,
  data = df,
  pch = 19,
  col = as.factor(df$crop),
  xlab = "Area Harvested (ha)",
  ylab = "Production (tonnes)",
  main = ""
)

legend(
  "topleft",
  legend = levels(as.factor(df$crop)),
  col = 1:4,
  pch = 19
)


# PRODUCTION VS YIELD -----------------------------------------------------

plot(
  Production_tonnes ~ Yield_kg_per_ha,
  data = df,
  pch = 19,
  col = as.factor(df$crop),
  xlab = "Yield (kg/ha)",
  ylab = "Production (tonnes)",
  main = ""
)

legend(
  "topleft",
  legend = levels(as.factor(df$crop)),
  col = 1:4,
  pch = 19
)


# CORRELATION $ CORRELATION MATRIX ----------------------------------------
cor_data <- df[, c("Area_Harvested_ha",
                   "Yield_kg_per_ha",
                   "Production_tonnes")]

corrplot(
  cor(cor_data),
  method = "number"
)
