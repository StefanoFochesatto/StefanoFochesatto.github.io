# Importing Keras and Mnist Data
library(keras)
c(c(xtrain, ytrain), c(xtest, ytest)) %<-% dataset_mnist()

## Reshaping the data
xtrain = xtrain/255
xtest = xtest/255

x_train = array_reshape(xtrain, dim=c(dim(xtrain)[1], 28, 28, 1))
x_test = array_reshape(xtest, dim=c(dim(xtest)[1], 28, 28, 1))

# Defining the Convolutional Auto Encoder
enc_input = layer_input(shape = c(28, 28, 1))
enc_output = enc_input %>%
  layer_conv_2d(12,kernel_size=c(3,3), activation="relu", padding="same") %>%
  layer_max_pooling_2d(c(2,2), padding="same") %>%
  layer_conv_2d(4,kernel_size=c(3,3), activation="relu", padding="same") %>%
  layer_max_pooling_2d(c(4,4), padding="same")  %>%
  layer_flatten() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 6, activation = "relu") ## Latent Space Layer

dec_output = enc_output %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_reshape(c(4,4,4))  %>%
  layer_conv_2d(4, kernel_size=c(3,3), activation="relu", padding="same") %>%
  layer_upsampling_2d(c(4,4)) %>%
  layer_conv_2d(12, kernel_size=c(3,3), activation="relu") %>%
  layer_upsampling_2d(c(2,2)) %>%
  layer_conv_2d(1, kernel_size=c(3,3), activation="sigmoid", padding="same")

aen = keras_model(enc_input, dec_output)
aen %>% compile(optimizer="Adam", loss="mean_squared_error")
## Printing the structure of network model
summary(aen)

## Training the Model with RMSprop on binary cross entropy loss function
aen %>% fit(x_train, x_train, epochs=35, batch_size=128)

## Predicting and pulling values from latentSpace
layer_name <- 'latentSpace'
latentSpace_model <- keras_model(inputs = aen$input, outputs = get_layer(aen, 'dense_10')$output)
latentSpace_output <- predict(latentSpace_model, x_test)
pred = aen %>% predict(x_test)

## Sampling the Latent Space for Convenient Plotting. 
latentSpace_outputSample <- latentSpace_output[ seq(1, length(latentSpace_output[,1]), by=10),]
ytestSample <- as.data.frame(ytest)
ytestSample <- as.data.frame(ytest)[ seq(1, length(latentSpace_output[,1]), by=10),]
colors <- rainbow(length(unique(ytestSample)))
library(tsne)

## Plotting 8 dimensional Latent Space in 2 dimensions with TSNE algorithm. 
ecb = function(x,y){ plot(x,t='n'); text(x, col=colors[ytestSample])}
tsne_iris = tsne(latentSpace_outputSample[,1:6], epoch_callback = ecb, perplexity=75)




latentSpace_outputSample <- latentSpace_output[ seq(1, length(latentSpace_output[,1]), by=10),]
