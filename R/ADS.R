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
ADS = R6Class("ADS",
               active = list(
                 #' @field data (`data.frame()`)\cr
                 #' Data frame.
                 data = function(value) {
                   if (missing(value)) return(private$data_)
                   else stop("can't set field data")
                 },
                 #' @field learner ([`LearnerRegr`][mlr3::LearnerRegr])\cr
                 #' A learner.
                 learner = function(value) {
                   if (missing(value)) return(private$learner_)
                   else stop("can't set field learner")
                 },
                 #' @field task_list (`list()`)\cr
                 #' A list of tasks.
                 task_list = function(value) {
                   if (missing(value)) return(private$task_list_)
                   else stop("can't set field task_list")
                 },
                 #' @field learner_list (`list()`)\cr
                 #' A list of learners.
                 learner_list = function(value) {
                   if (missing(value)) return(private$learner_list_)
                   else stop("can't set field learner_list")
                 },
                 #' @field delta (`numeric()`)\cr
                 #' A vector.
                 delta = function(value) {
                   if (missing(value)) return(private$delta_)
                   else stop("can't set field delta")
                 },
                 #' @field gamma (`numeric()`)\cr
                 #' A vector.
                 gamma = function(value) {
                 if (missing(value)) return(private$gamma_)
                 else stop("can't set field gamma")
                 },
                 #' @field weight_path (`array()`)\cr
                 #' An array.
                 weight_path = function(value) {
                   if (missing(value)) return(private$W_path_)
                   else stop("can't set field weight_path")
                 },
                 #' @field predictions (`array()`)\cr
                 #' An array.
                 predictions = function(value) {
                   if (missing(value)) return(private$predictions_)
                   else stop("can't set field predictions")
                 }
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
                 #' @param learner ([`LearnerRegr`][mlr3::LearnerRegr]) \cr
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
                   assertTRUE("weights" %in% learner$properties)

                   private$learner_ = learner$clone(deep = TRUE)

                   # check other parameters
                   assertIntegerish(iterations, lower = 1)
                   assertNumeric(delta,lower = 0)
                   if (length(delta) == 1){
                     delta = rep(delta,iterations)
                   } else {
                     checkmate::assertTRUE(length(delta) == iterations)
                   }
                   assertNumeric(gamma,lower = 0)
                   assertTRUE(all(gamma > 0))
                   if (length(gamma) == 1){
                     gamma = rep(gamma,iterations)
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
                                      "Learners:", paste0(class(private$learner_)[1])
                   )
                   cat(header, "\n",
                       data_info)

                   invisible(self)
                 },
                 #' @description
                 #' Estimate ADS models.
                 #'
                 #' @param store_predictions (`logical(1)`) \cr
                 #' Indicates whether the predictions should be
                 #' stored in field `predictions_`. Default is `FALSE`.
                 #'
                 #' @return self
                 fit = function(store_predictions = FALSE){
                   level_vec = levels(private$ind_)
                   N = nlevels(private$ind_)
                   ind_index = as.integer(1:N)[private$ind_]
                   n = length(private$ind_)

                   #construct a path for the weight matrix
                   W_path = array(NaN, c(N,N,private$iterations_+1))
                   if (all(is.null(private$W_start_))) {
                     W_path[,,1] = diag(N) #weight matrix for first stage
                   } else {
                     W_path[,,1] = private$W_start_
                   }

                   #initialize lists
                   task_list = learner_list = list()
                   if (store_predictions){
                     private$predictions_ = array(data = NA,
                                                   dim = c(n,private$iterations_),
                                                   dimnames = list(1:n, 1:private$iterations_))
                   }
                   #start iterations
                   for (it in 1:private$iterations_){
                     #list of tasks
                     task_list[[it]] = lapply(seq_len(N), function(i) {
                       data = private$data_[,!(names(private$data_) == private$individ_)]
                       data$weight_ADS = vapply(ind_index, function(j) W_path[i,j,it], FUN.VALUE = numeric(1))
                       task = TaskRegr$new(id = level_vec[i], backend = data, target = private$target_)
                       #change role of weight
                       #task$set_col_role("weight_ADS","weight")
                       task$col_roles$feature = task$col_roles$feature[-length(task$col_roles$feature)]
                       task$col_roles$weight = "weight_ADS"
                       task
                     })
                     #list of learners (with training)
                     learner_list[[it]] = lapply(seq_len(N), function(i) {
                       temp_learner = private$learner_$clone(deep = TRUE)
                       temp_learner$train(task_list[[it]][[i]])
                     })
                     #adjust weight matrix
                     W_path[,,it + 1] = vapply(seq_len(N), function(i) {
                       vapply(seq_len(N),function(j) {
                         dist = do.call(private$calc_dist_$fun,
                                         args = c(list(model_1 = learner_list[[it]][[i]],
                                                       model_2 = learner_list[[it]][[j]],
                                                       task_list = task_list[[it]]),
                                                  private$calc_dist_$params))
                         assertNumber(dist)
                         weight = do.call(private$calc_weight_$fun,
                                           args = c(list(dist = dist,
                                                         delta = private$delta_[it],
                                                         gamma = private$gamma_[it]),
                                                    private$calc_weight_$params))
                         assertNumber(weight)
                         weight
                       }, FUN.VALUE = numeric(1))
                     }, FUN.VALUE = numeric(N))
                     #set diag weight to 1
                     diag(W_path[,,it + 1]) = 1

                     if (store_predictions){
                       #fitted values
                       fit_list = lapply(seq_len(N), function(i) {
                         learner_list[[it]][[i]]$predict(task_list[[it]][[i]], row_ids = seq_len(n)[ind_index == i])
                       })
                       #vector of fitted values

                       invisible(lapply(seq_len(N), function(i) {private$predictions_[ind_index == i, it] <<- fit_list[[i]]$response}))
                     }
                   }
                   private$W_path_ = W_path
                   private$learner_list_ = learner_list
                   private$task_list_ = task_list
                   private$level_ind_ = level_vec



                   invisible(self)
                 },
                 #' @description
                 #' Predict ADS models on new data.
                 #'
                 #' @param newdata (`data.frame()`) \cr
                 #' Predicts the model on new data. Has to be a data.frame with the same columns as in the trained model.
                 #'
                 #' @param iterations (`vector()`) \cr
                 #' Specifies the iterations to predict. Defaults to all iterations.
                 #'
                 #' @return An array containing the predicted values over different iterations.
                 predict = function(newdata, iterations = NULL){
                   assertDataFrame(newdata,any.missing = FALSE)
                   assertSubset(names(private$data_)[names(private$data_) != private$target_],names(newdata))
                   assertFactor(newdata[,private$individ_])

                   newdata$weight_ADS = 1
                   #split newdata into lists based on individual
                   newdata[,!(names(newdata) == private$individ_)]
                   newdata_list = split(newdata[,!(names(newdata) == private$individ_)],
                                         f = newdata[,private$individ_])
                   if (is.null(iterations)) {
                     iterations = 1:private$iterations_
                   }

                   fit = array(data = NA,
                                dim = c(dim(newdata)[1],length(iterations)),
                                dimnames = list(1:dim(newdata)[1], 1:private$iterations_))
                   for (it in iterations){
                     #predict values for each model
                     fit_list_it = lapply(levels(newdata[,private$individ_]), function(level) {
                       i = match(level,private$level_ind_)
                       j = match(level,levels(newdata[,private$individ_]))
                       predict(object = private$learner_list_[[it]][[i]], newdata = newdata_list[[j]])
                     })
                     #reverse the split to make the vector compatible with newdata
                     fit[,it] = unsplit(fit_list_it, f = newdata[,private$individ_])
                   }
                   return(fit)
                 },
                 #' @description
                 #' Calculate the mean squared error for indiviuals over different iterations.
                 #'
                 #' @param newdata (`data.frame()`) \cr
                 #' Calculate the mean squared error on new data. Has to be a data.frame with the same columns as in the trained model.
                 #'
                 #' @return A list containing the mean squared error for indiviudals and the combined model.
                 calc_mse = function(newdata){
                   assertDataFrame(newdata,any.missing = FALSE)
                   assertFactor(newdata[,private$individ_])

                   pred = data.frame(rep(0,dim(newdata)[1]), self$predict(newdata = newdata))
                   colnames(pred) = 0:private$iterations_
                   target = newdata[private$target_][,1]
                   mse = colMeans((target - pred)^2)

                   #split the values for individuals
                   ind = private$ind_
                   target_list = split(target, f = ind)
                   pred_list = split(pred,f = ind)
                   mse_list = lapply(names(pred_list), function(ind){
                     colMeans((pred_list[ind][[1]]-target_list[ind][[1]])^2)
                   })
                   names(mse_list) = names(target_list)

                   res = list("mse" = mse, "ind_mse" = mse_list)
                   return(res)
                 },
                 #' @description
                 #' Plot the mean squeared error over the iterations. At iteration zero, all predictions are initialized to be zero.
                 #'
                 #' @param newdata (`data.frame()`) \cr
                 #' Plot the mean squared error out-of-sample. Has to be a data.frame with the same columns as in the trained model.
                 #'
                 #' @param individuals (`character()`) \cr
                 #' Individuals to plot the weights for. Defaults to all individuals.
                 #'
                 #' @param iterations (`integer()`) \cr
                 #' Steps to plot the weights for. Defaults to all iterations.
                 #'
                 #' @param interactive (`logical(1)`) \cr
                 #' Create an interactive plot with `plotly`.
                 #'
                 #' @return list
                 plot_mse = function(newdata = NULL,
                                      individuals = NULL,
                                      iterations = NULL,
                                      interactive = TRUE){
                   if (is.null(individuals)){
                     individuals = private$level_ind_
                   }
                   if (is.null(iterations)){
                     iterations = 0:private$iterations_
                   }

                   # check inputs
                   if (!is.null(newdata)){
                     assertDataFrame(newdata,any.missing = FALSE)
                     assertFactor(newdata[,private$individ_])
                   }
                   assertCharacter(individuals)
                   assertIntegerish(iterations)
                   assertLogical(interactive, len = 1)
                   assertSubset(iterations, seq(0, private$iterations_, by = 1))
                   assertSubset(individuals, private$level_ind_)

                   mse_is = self$calc_mse(newdata = self$data)
                   df_ind = reshape2::melt(do.call(rbind, mse_is$ind_mse))
                   df_ind$Var1 = as.character(df_ind$Var1)
                   df_ind$Sample = "In-Sample"
                   df_all = data.frame("Var1" = rep("All", length(iterations)),
                                        "Var2" = iterations,
                                        "value" = mse_is$mse,
                                        "Sample" = "In-Sample")

                   if (!is.null(newdata)){
                     mse_oos = self$calc_mse(newdata = self$data)
                     df_ind_oos = reshape2::melt(do.call(rbind, mse_oos$ind_mse))
                     df_ind_oos$Var1 = as.character(df_ind_oos$Var1)
                     df_ind_oos$Sample = "Out-of-Sample"
                     df_all_oos = data.frame("Var1" = rep("All", length(iterations)),
                                              "Var2" = iterations,
                                              "value" = mse_oos$mse,
                                              "Sample" = "Out-of-Sample")
                     df_ind = rbind(df_ind, df_ind_oos)
                     df_all = rbind(df_all, df_all_oos)
                   }

                   #filter for desired individuals
                   df_ind_filtered_1 = df_ind[(vapply(df_ind$Var1, function(x) any(x == individuals), FUN.VALUE = FALSE)) & (vapply(df_ind$Var2, function(x) any(x == iterations), FUN.VALUE = FALSE)),]
                   df_ind_filtered_2 = df_ind[(vapply(df_ind$Var1, function(x) !any(x == individuals), FUN.VALUE = FALSE)) & (vapply(df_ind$Var2, function(x) any(x == iterations), FUN.VALUE = FALSE)),]
                   df_all_filtered = df_all[(vapply(df_all$Var2, function(x) any(x == iterations), FUN.VALUE = FALSE)),]

                   if (interactive){
                     df_ind_filtered_1$text = paste0("Individual: ", df_ind_filtered_1$Var1, "\n","Iteration: ", df_ind_filtered_1$Var2, "\n", "MSE: ",round(df_ind_filtered_1$value,2))
                     if (dim(df_ind_filtered_2)[1] >0){
                       df_ind_filtered_2$text = paste0("Individual: ", df_ind_filtered_2$Var1, "\n","Iteration: ", df_ind_filtered_2$Var2, "\n", "MSE: ",round(df_ind_filtered_2$value,2))
                     }
                     df_all_filtered$text = paste0("Individual: ", df_all_filtered$Var1, "\n","Iteration: ", df_all_filtered$Var2, "\n", "MSE: ",round(df_all_filtered$value,2))
                   }

                   cols = c("black", scales::hue_pal()(length(individuals)))
                   names(cols) = c("All", individuals)
                   plt = ggplot(df_all_filtered, aes(x=Var2, y=value))

                   if (dim(df_ind_filtered_2)[1] > 0){
                     plt = plt + geom_line(data=df_ind_filtered_2, aes(group = Var1), color = "grey75", alpha = 0.5) +
                       suppressWarnings(geom_point(data=df_ind_filtered_2, aes(group = Var1, text = text), color ="grey75", alpha = 0.5))
                   }

                   plt = plt +
                     geom_line(data=df_ind_filtered_1, aes(color=Var1), size = .9) +
                     suppressWarnings(geom_point(data=df_ind_filtered_1, aes(color=Var1, text = text), size = 2)) +
                     geom_line(aes(color=Var1), size = .9) +
                     suppressWarnings(geom_point(aes(color=Var1, text = text), size = 2)) +
                     labs(title = "Mean Squared Error over Iterations\n", x = "Iteration", y = "MSE", color = "Individual\n") +
                     theme(legend.position="bottom") +
                     scale_color_manual(values = cols)

                   if (!is.null(newdata)){
                     plt = plt + facet_wrap(~ Sample)
                   }

                   if (interactive){
                     plt = plotly::ggplotly(plt, tooltip="text")
                   }
                   return(plt)
                 },
                 #' @description
                 #' Plot the used weights as a heatmap.
                 #'
                 #' @param individuals (`character()`) \cr
                 #' Individuals to plot the weights for. Defaults to all individuals.
                 #'
                 #' @param iterations (`integer()`) \cr
                 #' Steps to plot the weights for. Defaults to all iterations.
                 #'
                 #' @param interactive (`logical(1)`) \cr
                 #' Create an interactive plot with `plotly`.
                 #'
                 #' @param show_axis_text (`logical(1)`) \cr
                 #' Show axis tick text.
                 #'
                 #' @return list
                 heatmap = function(individuals = NULL,
                                    iterations = NULL,
                                    interactive = TRUE,
                                    show_axis_text = TRUE){
                   if (is.null(iterations)){
                     iterations = seq_len(private$iterations_)
                   }
                   if (is.null(individuals)){
                     individuals = private$level_ind_
                   }
                   # check inputs
                   assertCharacter(individuals)
                   assertIntegerish(iterations)
                   assertLogical(interactive, len = 1)
                   assertLogical(show_axis_text, len = 1)
                   assertSubset(iterations, seq_len(private$iterations_))
                   assertSubset(individuals, private$level_ind_)

                   #reshape data
                   df = reshape2::melt(private$W_path_[,,iterations])
                   df$Var1 = as.factor(df$Var1)
                   df$Var2 = as.factor(df$Var2)
                   levels(df$Var1) = levels(df$Var2) = levels(private$ind_)
                   df$Var3 = rep(iterations,each = nlevels(private$ind_)^2)
                   df$text =paste0("Individual i: ", df$Var2, "\n","Individual j: ", df$Var1, "\n", "Weight: ",round(df$value,2))
                   #filter data
                   df_filtered = df[(vapply(df$Var1, function(x) any(x == individuals), FUN.VALUE = FALSE)) & (vapply(df$Var2, function(x) any(x == individuals), FUN.VALUE = FALSE)),]

                   heatmap = ggplot(df_filtered, aes(x = .data$Var1, y = .data$Var2, fill =.data$value, text = .data$text)) +
                     geom_tile() +
                     xlab(label = "Individual j") +
                     ylab(label = "Individual i") +
                     scale_x_discrete(limits = unlist(individuals)) +
                     scale_y_discrete(limits = rev(unlist(individuals))) +
                     facet_wrap(~ .data$Var3) +
                     viridis::scale_fill_viridis(name="Weight",option ="C") +
                     theme_bw() +
                     theme(strip.placement = "outside",
                                    plot.title = element_text(hjust = 0.5),
                                    strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
                     ggtitle(label = "Weight Matrix") +
                     theme(legend.position="bottom")

                   if (!show_axis_text){
                     heatmap = heatmap +
                       theme(axis.text.x = element_blank(),
                             axis.text.y = element_blank())
                   }

                   if (interactive){
                     heatmap = plotly::ggplotly(heatmap, tooltip="text")
                   }
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
                 predictions_ = NULL,
                 W_path_ = NULL,
                 learner_list_ = NULL,
                 task_list_ = NULL,
                 level_ind_ = NULL
               )
)
