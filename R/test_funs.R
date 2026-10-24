workshop_species <- "Opakapaka"
number_of_drops <- 5

n_rows <- 10  # Example number of rows
drops <- number_of_drops

species_data <- data.frame(
    Species = rep(workshop_species, n_rows),
    Year = seq(1990,1999,by=1),
    Effort = rep(drops, n_rows),
    Count = runif(n=n_rows, min = 12, max = 20)
  )


     model_fit <- fit_schaefer_model(
        data = species_data,
        use_cpue = TRUE
      )
      