set.seed(42)
library(RColorBrewer)
n <- 100; N <- 10; p <- 3
X <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
ind <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
Y <- X%*%rep(1,p) * (as.numeric(ind) <= N/2) + rnorm(n*N,0,1)

X_test <- matrix(runif(N*n*p),nrow = n*N, ncol = p)
ind_test <- as.factor(rep(1:N,n)[sample(1:(n*N),n*N)])
Y_test <- X_test%*%rep(1,p) * (as.numeric(ind_test) <= N/2) + rnorm(n*N,0,1)

library(plyr)
ind = revalue(ind, c("1"="one", "2"="two", "3"="three", "4"="four", "5"="five"))
ind_test = revalue(ind, c("1"="one", "2"="two", "3"="three", "4"="four", "5"="five"))
data <- data.frame(X,"y" = Y, "ind" = ind)
data_test <- data.frame(X_test,"y" = Y_test, "ind" = ind_test)

library(AdaptiveDiscreteSmoothing)
library(mlr3)
library(mlr3learners)
learner <- mlr_learners$get("regr.lm")
#learner <- mlr_learners$get("regr.ranger")
model <- ADS$new(data = data,
                 target = "y",
                 individ = "ind",
                 learner = learner,
                 iterations = 4)
model$fit(store_predictions = T)
model$heatmap()

private <- model$.__enclos_env__$private
df <- reshape2::melt(private$W_path_[,,iterations])
df$Var1 <- as.factor(df$Var1)
df$Var2 <- as.factor(df$Var2)
levels(df$Var1) <- levels(df$Var2) <- levels(private$ind_)
df$Var3 <- rep(iterations,each = nlevels(private$ind_)^2)
heatmap <- ggplot(df, aes(x = .data$Var1, y = .data$Var2, fill =.data$value)) +
  geom_tile() +
  xlab(label = "Individual") +
  ylab(label = "Individual") +
  scale_x_discrete(limits = private$level_ind_) +
  scale_y_discrete(limits = rev(private$level_ind_)) +
  facet_wrap(~ .data$Var3) +
  scale_fill_viridis(name="Weight",option ="C") +
  theme_bw() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Weight Matrix") +
  theme(legend.position="bottom")
heatmap


mse <- colMeans((data$y - model$predict(newdata = data))^2)
print(mse)

#plotting
library(ggplot2)
calc_mse <- function(model, newdata){
  pred <- data.frame(rep(0,dim(newdata)[1]), model$predict(newdata = newdata))
  colnames(pred) <- 0:model$.__enclos_env__$private$iterations_
  target <- newdata[model$.__enclos_env__$private$target_][,1]
  mse <- colMeans((target - pred)^2)

  #split the values for individuals
  ind <- model$.__enclos_env__$private$ind_
  target_list <- split(target, f = ind)
  pred_list <- split(pred,f = ind)
  mse_list <- lapply(names(pred_list), function(ind){
    colMeans((pred_list[ind][[1]]-target_list[ind][[1]])^2)
  })
  names(mse_list) <- names(target_list)

  res = list("mse" = mse, "ind_mse" = mse_list)
  return(res)
}



plot_mse <- function(model, newdata = NULL, individuals = NULL, iterations = NULL){
  if (is.null(individuals)){
    individuals <- levels(model$.__enclos_env__$private$ind_)
  }
  if (is.null(iterations)){
    iterations <- 0:model$.__enclos_env__$private$iterations_
  }
  mse_is <- calc_mse(model, newdata = model$data)
  df_ind <- reshape2::melt(do.call(rbind, mse_is$ind_mse))
  df_ind$Sample <- "In-Sample"
  df_all <- data.frame("Var1" = rep("All", length(iterations)),
                       "Var2" = iterations,
                       "value" = mse_is$mse,
                       "Sample" = "In-Sample")

  if (!is.null(newdata)){
    mse_oos <- calc_mse(model, newdata = newdata)
    df_ind_oos <- reshape2::melt(do.call(rbind, mse_oos$ind_mse))
    df_ind_oos$Sample <- "Out-of-Sample"
    df_all_oos <- data.frame("Var1" = rep("All", length(iterations)),
                             "Var2" = iterations,
                             "value" = mse_oos$mse,
                             "Sample" = "Out-of-Sample")
    df_ind <- rbind(df_ind, df_ind_oos)
    df_all <- rbind(df_all, df_all_oos)
  }
  #filter for desired individuals
  df_ind_filtered_1 <- df_ind[(vapply(df_ind$Var1, function(x) any(x == individuals), FUN.VALUE = FALSE)) & (vapply(df_ind$Var2, function(x) any(x == iterations), FUN.VALUE = FALSE)),]
  df_ind_filtered_2 <- df_ind[(vapply(df_ind$Var1, function(x) !any(x == individuals), FUN.VALUE = FALSE)) & (vapply(df_ind$Var2, function(x) any(x == iterations), FUN.VALUE = FALSE)),]
  df_all_filtered <- df_all[(vapply(df_all$Var2, function(x) any(x == iterations), FUN.VALUE = FALSE)),]

  cols <- c("black", scales::hue_pal()(length(individuals)))
  names(cols) <- c("All", individuals)
  plt <- ggplot(df_all_filtered, aes(x=Var2, y=value)) +
    geom_line(data=df_ind_filtered_2, aes(group = Var1), color = "grey75", alpha = 0.5) +
    geom_point(data=df_ind_filtered_2, aes(group = Var1), color ="grey75", alpha = 0.5) +
    geom_line(data=df_ind_filtered_1, aes(color=Var1), size = .9) +
    geom_point(data=df_ind_filtered_1, aes(color=Var1), size = 2) +
    geom_line(aes(color=Var1), size = .9) +
    geom_point(aes(color=Var1), size = 2) +
    labs(title = "Mean Squared Error over Iterations\n", x = "Iteration", y = "MSE", color = "Individual\n") +
    theme(legend.position="bottom") +
    scale_color_manual(values = cols)

  if (!is.null(newdata)){
    plt <- plt + facet_wrap(~ Sample)
  }
  return(plt)
}



plot_mse(model, newdata = data_test, individuals = list("one","10"))










