##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Self-Study 2.1: Classical Decomposition
# gwendolin.wilke@fhnw.ch
##############################################

library(fpp3)

# ---------- Task 1

# Use tail() to get the last 5 years of data
gas <- tail(aus_production, 5 * 4) |> 
  select(Gas)

# Plot the data
#     There is some strong seasonality and a trend.
gas |>
  autoplot(Gas) + labs(y = "Petajoules")

# Decompose the time series using the classical decomposition
#     The decomposition has captured the seasonality and a slight trend.
decomp <- gas |>
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) |>
  components()

# Plot the decomposition
decomp |> autoplot()

# Plot the seasonally adjusted data
as_tsibble(decomp) |>
  autoplot(season_adjust) +
  labs(title = "Seasonally adjusted data", y = "Petajoules")

# Add 300 to the gas production in the last quarter of 2007 to produce an outlier
# -   The "seasonally adjusted" data now shows some seasonality because
#     the outlier has affected the estimate of the seasonal component.
gas |>
  mutate(Gas = if_else(Quarter == yearquarter("2007Q4"), Gas + 300, Gas)) |>
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) |>
  components() |>
  as_tsibble() |>
  autoplot(season_adjust) +
  labs(title = "Seasonally adjusted data", y = "Petajoules")


# Add 300 to the gas production in the last quarter of 2010 to produce an outlier
#     The seasonally adjusted data now show no seasonality because the outlier is in 
#     the part of the data where the trend can't be estimated.
gas |>
  mutate(Gas = if_else(Quarter == yearquarter("2010Q2"), Gas + 300, Gas)) |>
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) |>
  components() |>
  as_tsibble() |>
  autoplot(season_adjust) +
  labs(title = "Seasonally adjusted data", y = "Petajoules")

# ---------- Task 2

# Describing the decomposition of the Australian labour force data:
#
#   * The Australian labour force has been decomposed into 3 components (trend, seasonality, and remainder) using an STL decomposition.
#   * The trend element has been captured well by the decomposition, as it smoothly increases with a similar pattern to the data. 
#     The trend is of the same scale as the data (indicated by similarly sized grey bars), and contributes most to the decomposition 
#     (having the smallest scale bar).
#   * The seasonal component changes slowly throughout the series, with the second seasonal peak diminishing as time goes on -- 
#     this component is the smallest contribution original data (having the largest scale bar).
#   * The remainder is well-behaved until 1991/1992 when there is a sharp drop. There also appears to be a smaller drop in 1993/1994. 
#     There is sometimes some leakage of the trend into the remainder component when the trend window is too large. This appears to have happened here. It would be better if the recession of 1991-1992, and the smaller dip in 1993, were both included in the trend estimate rather than the remainder estimate. This would require a smaller trend window than what was used.
#   * In the bottom graph, the seasonal component is shown using a sub-series plot. December is the highest employment month, followed 
#     by March and September. The seasonal component changes mostly in March (with a decrease in the most recent years). 
#     July and August are the months with the next largest changes. The least changing is June with the rest are somewhere between these. 
#     December and September show increases in the most recent years.

# Is the recession of 1991/1992 visible in the estimated components?
#   
#   Yes. The remainder shows a substantial drop during 1991 and 1992 coinciding with the recession.
