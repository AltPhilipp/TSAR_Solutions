##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Self-Study 2.2: Outlying Time Series
# gwendolin.wilke@fhnw.ch
##############################################

library(fpp3)
library(broom)

## Compute features (and omit series with missing features)
PBS_feat <- PBS |>
  features(Cost, feature_set(pkgs = "feasts")) |>
  select(-`...26`) |>
  na.omit()

## Compute principal components
PBS_prcomp <- PBS_feat |>
  select(-Concession, -Type, -ATC1, -ATC2) |>
  prcomp(scale = TRUE) |>
  augment(PBS_feat)

## Plot the first two components
PBS_prcomp |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point()

## Pull out most unusual series from first principal component
outliers <- PBS_prcomp |>
  filter(.fittedPC1 > 6)
outliers |>
  select(ATC1, ATC2, Type, Concession)

## Visualise the unusual series
PBS |>
  semi_join(outliers, by = c("Concession", "Type", "ATC1", "ATC2")) |>
  autoplot(Cost) +
  facet_grid(vars(Concession, Type, ATC1, ATC2)) +
  labs(title = "Outlying time series in PC space")