#' @title ADS class
#'
#' @description
#' Base class for Adaptive discrete smoothing.
#'
#'
#' @format [R6::R6Class] object.
#'
#' @family ADS
#'
#' @export
ADS <- R6Class("ADS",
               active = list(
               ),
               public = list(
                 #' @description
                 #' Initialize a ADS Class object.
                 #'
                 #' @param data (`data.frame()`) \cr
                 #' Data.frame containing the target variable, the covariates and a column with the corresponding individuals.
                 #'
                 #' @param target (`character(1)`) \cr
                 #' Name of the target variable.
                 #'
                 #' @param individ (`character(1)`) \cr
                 #' Name of the column with the individuals. Column has to be a factor.
                 #'
                 #' @param learner (named `list()`) \cr
                 #' The machine learners from the `mlr3`-package.
                 #'
                 #' @param delta (`numeric()`) \cr
                 #' Parameter for the loss function
                 #'
                 #' @param gamma (`gamma()`) \cr
                 #' Parameter for the loss function
                 #'
                 #' @param iterations (`integer(1)`) \cr
                 #' Number of iterations.
                 #'
                 #' @param W_start (`matrix()`) \cr
                 #' Starting weights. Default is the identity matrix.
                 #'
                 #' @param calc_dist (`function()`) \cr
                 #' A list. If not set to default a list with the following components:
                 #' * `fun` A function of the form `function(model_1,model_2,task_list,...)` to calculate the weights.
                 #' * `params` A list of the additional parameters used in `fun`.
                 #' @param calc_weight (`function()`) \cr
                 #' A list. If not set to default a list with the following components:
                 #' * `fun` A function of the form `function(dist, delta, gamma,...)` to calculate the weights.
                 #' * `params` A list of the additional parameters used in `fun`.
                 initialize = function(data,
                                       target,
                                       individ,
                                       learner,
                                       delta = 0.7,
                                       gamma = 1,
                                       iterations = 2,
                                       W_start = NULL,
                                       calc_dist = list("fun" = calc_dist_default),
                                       calc_weight = list("fun" = calc_weight_default,
                                                          "params" = list("kernel" = "gaussian"))){
                   # check data
                   assertDataFrame(data,any.missing = FALSE)
                   assertCharacter(target, min.len = 1,max.len = 1)
                   assertChoice(target,names(data))
                   assertCharacter(individ, min.len = 1,max.len = 1)
                   assertChoice(individ,names(data))
                   assertFactor(data[,individ])

                   private$data_ = data
                   private$target_ = target
                   private$ind_ = data[,individ]
                   private$covariates_ = names(data)[!(names(data) %in% c(target, individ))]
                   private$individ_ = individ

                   # check learner
                   assert(check_character(learner, max.len = 1),
                          check_class(learner, "Learner"))
                   assertTRUE(learner$task_type == "regr")

                   private$learner_ = learner$clone(deep = TRUE)

                   # check other parameters
                   assertIntegerish(iterations, lower = 1)
                   assertNumeric(delta,lower = 0)
                   if (length(delta) == 1){
                     delta <- rep(delta,iterations)
                   } else {
                     checkmate::assertTRUE(length(delta) == iterations)
                   }
                   assertNumeric(gamma,lower = 0)
                   if (length(gamma) == 1){
                     gamma <- rep(gamma,iterations)
                   } else {
                     checkmate::assertTRUE(length(gamma) == iterations)
                   }

                   private$iterations_ = iterations
                   private$delta_ = delta
                   private$gamma_ = gamma

                   #check W_start
                   if (!is.null(W_start)){
                     assertMatrix(W_start, nrows = nlevels(private$ind_), ncols = nlevels(private$ind_))
                   }

                   private$W_start_ = W_start

                   #check distance and weight funcitons
                   assertList(calc_dist)
                   assertFunction(calc_dist$fun, args = c("model_1","model_2","task_list"))
                   assertList(calc_dist$params, null.ok = TRUE)

                   assertList(calc_weight)
                   assertFunction(calc_weight$fun, args = c("dist", "delta", "gamma"))
                   assertList(calc_weight$params, null.ok = TRUE)

                   private$calc_dist_ = calc_dist
                   private$calc_weight_ = calc_weight
                 },
                 #' @description
                 #' Print ADS objects.
                 print = function(){
                   class_name = class(self)[1]
                   header = paste0(
                     "================= ", class_name,
                     " Object ==================\n")
                   data_info = paste0("Outcome variable: ", private$target_, "\n",
                                      "Individuals: ", private$individ_, "\n",
                                      "Covariates: ", paste0(private$covariates_, collapse = ","), "\n",
                                      "Learners:", paste0(private$learner$id)
                   )
                   cat(header, "\n",
                       data_info)

                   invisible(self)
                 },
                 #' @description
                 #' Estimate ADS models.
                 #'
                 #' @param store_predictions (`logical(1)`) \cr
                 #' Indicates whether the predictions for the nuisance functions should be
                 #' stored in field `predictions`. Default is `FALSE`.
                 #'
                 #' @return self
                 fit = function(store_predictions = FALSE){
                   level_vec = levels(private$ind_)
                   N = nlevels(private$ind_)
                   ind_index <- as.integer(1:N)[private$ind_]
                   n = length(private$ind_)

                   #construct a path for the weight matrix
                   W_path <- array(NaN, c(N,N,private$iterations_+1))
                   if (all(is.null(private$W_start_))) {
                     W_path[,,1] <- diag(N) #weight matrix for first stage
                   } else {
                     W_path[,,1] <- private$W_start_
                   }

                   #start iterations
                   for (it in 1:private$iterations_){
                     #list of tasks
                     task_list <- lapply(seq_len(N), function(i) {
                       data <- private$data_[,!(names(private$data_) == private$individ_)]
                       data$weight_ADS <- vapply(ind_index, function(j) W_path[i,j,it], FUN.VALUE = numeric(1))
                       task <- TaskRegr$new(id = level_vec[i], backend = data, target = private$target_)
                       #change role of weight
                       #task$set_col_role("weight_ADS","weight")
                       task$col_roles$feature <- task$col_roles$feature[-length(task$col_roles$feature)]
                       task$col_roles$weight <- "weight_ADS"
                       task
                     })
                     #list of learners (with training)
                     learner_list <- lapply(seq_len(N), function(i) {
                       temp_learner <- private$learner_$clone(deep = TRUE)
                       temp_learner$train(task_list[[i]])
                     })
                     #adjust weight matrix
                     W_path[,,it + 1] <- vapply(seq_len(N), function(i) {
                       vapply(seq_len(N),function(j) {
                         dist <- do.call(private$calc_dist_$fun,
                                         args = c(list(model_1 = learner_list[[i]],
                                                       model_2 = learner_list[[j]],
                                                       task_list = task_list),
                                                  private$calc_dist_$params))
                         assertNumber(dist)
                         weight <- do.call(private$calc_weight_$fun,
                                           args = c(list(dist = dist,
                                                         delta = private$delta_[it],
                                                         gamma = private$gamma_[it]),
                                                    private$calc_weight_$params))
                         assertNumber(weight)
                         weight
                       }, FUN.VALUE = numeric(1))
                     }, FUN.VALUE = numeric(N))
                     #set diag weight to 1
                     diag(W_path[,,it + 1]) <- 1
                   }
                   private$W_path_ <- W_path
                   private$learner_list_ <- learner_list
                   private$task_list_ <- task_list
                   private$level_ind_ <- level_vec

                   if (store_predictions){
                     #fitted values
                     fit_list <- lapply(seq_len(N), function(i) {
                       learner_list[[i]]$predict(task_list[[i]], row_ids = seq_len(n)[ind_index == i])
                     })
                     #vector of fitted values
                     private$predictions <- rep(NA,n)
                     invisible(lapply(seq_len(N), function(i) {private$predictions[ind_index == i] <<- fit_list[[i]]$response}))
                     names(private$predictions) <- seq_len(n)
                   }

                   invisible(self)
                 },
                 #' @description
                 #' Predict ADS models on new data.
                 #'
                 #' @param newdata (`data.frame()`) \cr
                 #' Predicts the model on new data. Has to be a data.frame with the same columns as in the trained model.
                 #'
                 #' @return A vector containig the predicted values.
                 predict = function(newdata){
                   assertDataFrame(newdata,any.missing = FALSE)
                   assertSubset(names(private$data_)[names(private$data_) != private$target_],names(newdata))
                   assertFactor(newdata[,private$individ_])

                   newdata$weight_ADS = 1
                   #split newdata into lists based on individual
                   newdata[,!(names(newdata) == private$individ_)]
                   newdata_list <-  split(newdata[,!(names(newdata) == private$individ_)],
                                          f = newdata[,private$individ_])
                   #predict values for each model
                   fit_list <- lapply(levels(newdata[,private$individ_]), function(level) {
                     i <- match(level,private$level_ind_)
                     j <- match(level,levels(newdata[,private$individ_]))
                     predict(object = private$learner_list_[[i]], newdata = newdata_list[[j]])
                   })
                   #reverse the split to make the vector compatible with newdata
                   fit <- unsplit(fit_list, f = newdata[,private$individ_])
                   names(fit) <- 1:dim(newdata)[1]
                   return(fit)
                 },
                 #' @description
                 #' Plot the used weights as a heatmap.
                 #'
                 #' @param iterations (`integer()`) \cr
                 #' Steps to plot the weights for. Defaults to all iterations.
                 #'
                 #' @return list
                 heatmap = function(iterations = NULL){
                   if (is.null(iterations)){
                     iterations <- seq_len(private$iterations_)
                   }
                   assertSubset(iterations,seq_len(private$iterations_))
                   df <- reshape2::melt(private$W_path_[,,iterations])
                   df$Var3 <- rep(iterations,each = nlevels(private$ind_)^2)
                   heatmap <- ggplot(df, aes(x = .data$Var1, y = .data$Var2, fill =.data$value)) +
                     geom_tile() +
                     xlab(label = "Individual") +
                     ylab(label = "Individual") +
                     scale_x_continuous(breaks = seq_len(length(private$level_ind_))) +
                     scale_y_continuous(breaks = seq_len(length(private$level_ind_))) +
                     facet_wrap(~ .data$Var3) +
                     scale_fill_gradient(name = "Weight",low = "#FFFFFF",high = "#012345") +
                     theme_bw() +
                     theme(strip.placement = "outside",
                                    plot.title = element_text(hjust = 0.5),
                                    strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
                     ggtitle(label = "Weight Matrix") +
                     theme(legend.position="bottom")
                   return(heatmap)

                 }
               ),
               private = list(
                 data_ = NULL,
                 target_ = NULL,
                 covariates_ = NULL,
                 ind_ = NULL,
                 individ_ = NULL,
                 W_start_ = NULL,
                 learner_ = NULL,
                 delta_ = NULL,
                 gamma_ = NULL,
                 iterations_ = NULL,
                 calc_dist_ = NULL,
                 calc_weight_ = NULL,
                 predictions = NULL,
                 W_path_ = NULL,
                 learner_list_ = NULL,
                 task_list_ = NULL,
                 level_ind_ = NULL
               )
)
