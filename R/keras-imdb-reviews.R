###################################
## Keras IMDB Review Classification
###################################

## This code is based on https://github.com/jjallaire/deep-learning-with-r-notebooks/blob/master/notebooks/3.4-classifying-movie-reviews.Rmd

library(keras)

imdb <- dataset_imdb(num_words = 10000)  ## load only data for 10000 most common words
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb

## %<-% is the 'multi-assignment' operator; it's the same as:
## train_data <- imdb$train$x
## train_labels <- imdb$train$y
## test_data <- imdb$test$x
## test_labels <- imdb$test$y

str(train_data[[1]])

train_labels[[1]]

## One-hot-encode your lists to turn them into vectors of 0s and 1s. e.g.,  c(3, 5) becomes
## 10,000-dimensional vector that would be all zeros except for indices 3 and 5

vectorize_sequences <- function(sequences, dimension = 10000) {
  # Create an all-zero matrix of shape (len(sequences), dimension)
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    # Sets specific indices of results[i] to 1s
    results[i, sequences[[i]]] <- 1
  results
}

# Our vectorized training data
x_train <- vectorize_sequences(train_data)
# Our vectorized test data
x_test <- vectorize_sequences(test_data)

str(x_train[1,])

# Our vectorized labels
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

## Specify the NN model
## two hidden layers with 16 nodes each, fully connected
model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

## compile the model; pick loss function and optimizer
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)


## In order to monitor during training the accuracy of the model on data that it
## has never seen before, we will create a "validation set" by setting apart 10,000
## samples from the original training data:
val_indices <- 1:10000

x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

## Train model for 20 epochs (20 iterations over all training samples) in mini-batches of 512 samples.
## Also monitor loss and accuracy on the 10,000 samples that we set apart by passing the validation 
## data as the `validation_data` argument:
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)


str(history)

plot(history)


## looks like model trained to 4 epochs is best
## redo model fit to 4 epochs and evaluate on testing data;
model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)

results

## apply model to new data using predict method
model %>% predict(x_test[1:10,])

#######################################
## Keras IMDB Review Classification Lab
#######################################

## 1. Consider using 1 or 3 hidden layers and see how it affects validation and test accuracy.
## 2. Try to use layers with more hidden units or less hidden units: 32 units, 64 units...
## 3. Try to use the `mse` loss function instead of `binary_crossentropy`.
## 4. Try to use the `tanh` activation (an activation that was popular in the early days of 
##    neural networks) instead of `relu`.