zip.dt <_ data.table::fread(file=destfile)
zip.no.label <- as.matrix()

n.input.output <- ncol(zip.no.label)
n.intermediate <- 50
my_activation <- activation_relu
model <- keras::keras_model_sequential() %>%
  keras::layer_dense(
    input_shape = n.input.output,
    units = n.intermediate,
    activation = my_activation
  ) %>%
  keras::layer_dense(
    name = "code",
    units = 2,
    activation = my_activation
  ) %>%
  keras::layer_dense(
    units.n.intermediate,
    activation = my_activation
  ) %>%
  keras::layer_dense(
    units = n.input.output,
    activation = my_activation
  )

compiled.model <- keras::compile(
  model,
  optimizer = keras::optimizer_sgd(lr = 0.01),
  loss = keras::loss_mean_squared_error
)

fit.history <- kreas::fit(
  compiled.model,
  zip.no.label, zip.no.label, epochs = 5
)
