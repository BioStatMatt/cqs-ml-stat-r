#####################################
## Keras MNIST Zipcode Classification
#####################################

library(keras)

## MNIST images have shape (28 height, 28 width, 1 channels)
## basic components of a convolutional NN
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(28, 28, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")

summary(model)

## Need to 'flatten' the last layer
## Add densly connected layer and output layer

model <- model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")

summary(model)

## load MNIST image data
mnist <- dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% mnist

train_images <- array_reshape(train_images, c(60000, 28, 28, 1))
train_images <- train_images / 255
str(train_images)

test_images <- array_reshape(test_images, c(10000, 28, 28, 1))
test_images <- test_images / 255

train_labels <- to_categorical(train_labels)
str(train_labels)

test_labels <- to_categorical(test_labels)


## compile the model
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

## train the model              
model %>% fit(
  train_images, train_labels, 
  epochs = 5, batch_size=64
)

## evaluate model on test data
results <- model %>% evaluate(test_images, test_labels)
results


#########################################
## Keras MNIST Zipcode Classification Lab
#########################################

## 1. Modify the first convolutional layer to use 'same' padding. How does that affect 
##    the number of model parameters, test error?

## 2. Remove the second to last (dense) layer. How does that affect the number of 
##    model parameters, test error?

