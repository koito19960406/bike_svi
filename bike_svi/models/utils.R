pacman::p_load(
    tidyverse, stats, plm, utils, pglm, progress, MatchIt, lmtest, sandwich,
    pscl, cobalt, grf, AER, DiagrammeRsvg, rsvg, stargazer, hrbrthemes, Hmisc,
    WeightIt, gbm, CBPS, caret, car, notifier, corrplot, randomForest, pdp, doMC,
    doParallel, caret
)

clean_var_name <- function(var_name){
    # Removing _binary, ss_, and _
    cleaned_var_name <- gsub("_binary", "", var_name)
    cleaned_var_name <- gsub("ss_", "", cleaned_var_name)
    cleaned_var_name <- gsub("_", "", cleaned_var_name) 
    return(cleaned_var_name)
}

remove_highly_correlated <- function(df, threshold = 0.7) {
    correlation_matrix <- cor(df, use = "complete.obs")
    # correlation_matrix[lower.tri(correlation_matrix)] <- 0
    highly_correlated <- findCorrelation(correlation_matrix, cutoff = threshold, verbose = TRUE)
    df <- df %>%
        dplyr::select(-all_of(highly_correlated))
    return(df)
}

# psm
run_psm <- function(data, dep_var_name, ind_var_name, covariates, model_dir, figure_dir) {
    models <- list()
    # check balance
    covariates_pasted <- paste(covariates, collapse = " + ")
    formula <- as.formula(paste(ind_var_name, " ~ ", covariates_pasted))
    match_result <- matchit(formula = formula, data = data, method = "nearest", distance = "glm", estimand = "ATE")
    summary_match_result <- summary(match_result)
    capture.output(summary_match_result, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_summary_balance.txt"))
    summary_match_model <- summary(match_result$model)
    capture.output(summary_match_model, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_first_stage_model.txt"))
    models$first_stage <- match_result$model
    print(match_result)
    # plot
    # pdf(paste0(figure_dir, "/", ind_var_name,"_match_result.pdf"), height = 2, width = 6)
    # plot(match_result, type = "density", interactive = FALSE)
    plot <- bal.plot(match_result, var.name = "distance", which = "both", lwd = 0.5) +
        scale_fill_manual(values = alpha(c("#7B52AE", "#74B652"), 0.5)) +
        labs(title = paste0("Distributional Balance for ", strsplit(ind_var_name, "_binary")[[1]][1])) +
        theme_ipsum()
    ggsave(paste0(figure_dir, "/", ind_var_name, "_match_result.png"), height = 4, width = 10)
    # print(plot)
    # dev.off()
    # model
    match_result_df <- match.data(match_result)
    # estimate ps and save data
    ps <- glm(formula, family = binomial(), data = data)
    ps_df <- data.frame(
        pr_score = predict(ps, type = "response"),
        count_point_id = data$count_point_id,
        treatment = data[[ind_var_name]]
    )
    write.csv(ps_df, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_propensity_score.csv"))
    match_result_df_cor <- match_result_df %>%
        dplyr::select(covariates) %>%
        dplyr::select(-c(year))
    corrmatrix <- cor(match_result_df_cor, use = "complete.obs")
    corrdf <- corrmatrix %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Var1") %>%
        gather("Var2", "value", -Var1)

    print(corrdf %>%
        filter((value >= 0.6 | value <= -0.6) & (Var1 != Var2)))
    right_side <- paste0(ind_var_name, " + ", covariates_pasted)
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
    zero_condition_covariates <- paste(covariates[str_detect(covariates, paste("od_", "pop_den", "poi", sep = "|"))],
        collapse = " + "
    )
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side, "|", zero_condition_covariates))
    print(formula)
    model_year_zero_inflated <- zeroinfl(formula, dist = "negbin", link = "logit", data = match_result_df, weights = weights) # check if it needs to be inverse
    model_year_zero_inflated_summary <- summary(model_year_zero_inflated, cluster = c("subclass"))
    capture.output(model_year_zero_inflated_summary, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_psm_year_fe_zinb.txt"))
    models$second_stage <- model_year_zero_inflated
    # return model object for stargazer later
    return(models)
}

run_psm_nb <- function(data, dep_var_name, ind_var_name, covariates, model_dir, figure_dir) {
    require(MASS) # Ensure the MASS package is loaded

    models <- list()
    # check balance
    covariates_pasted <- paste(covariates, collapse = " + ")
    formula <- as.formula(paste(ind_var_name, " ~ ", covariates_pasted))
    match_result <- matchit(formula = formula, data = data, method = "full", distance = "glm", estimand = "ATE")
    summary_match_result <- summary(match_result)
    capture.output(summary_match_result, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_summary_balance.txt"))
    summary_match_model <- summary(match_result$model)
    capture.output(summary_match_model, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_first_stage_model.txt"))
    models$first_stage <- match_result$model
    print(match_result)

    # plot
    # pdf(paste0(figure_dir, "/", ind_var_name,"_match_result.pdf"), height = 2, width = 6)
    # plot(match_result, type = "density", interactive = FALSE)
    # create a histogram in ggplot2
    match_df <- as_tibble(match.data(match_result)) %>% 
        # conevrt ind_var_name to factor
        mutate(treatment := factor(!!sym(ind_var_name), levels = c(0, 1)))
    plot <- ggplot(match_df) +
        geom_histogram(aes(x = distance, fill = treatment, weight = weights), alpha = 0.7, binwidth = 0.01, colour = "black") +
        scale_fill_manual(values = c("#7d7d7d", "#74B652")) +
        labs(title = paste0("Distributional Balance for ", clean_var_name(ind_var_name))) +
        theme_ipsum()
    ggsave(paste0(figure_dir, "/", ind_var_name, "_match_result.png"), height = 4, width = 10)
    # plot <- bal.plot(match_result, var.name = "distance", which = "adjusted", lwd = 1, type = "histogram", bins = 40) +
    #     scale_fill_manual(values = alpha(c("#7d7d7d", "#74B652"), 1)) +
    #     labs(title = paste0("Distributional Balance for ", clean_var_name(ind_var_name))) +
    #     theme_ipsum()
    # ggsave(paste0(figure_dir, "/", ind_var_name, "_match_result.png"), height = 4, width = 10)

    # model
    match_result_df <- match.data(match_result)

    # estimate ps and save data
    ps <- glm(formula, family = binomial(), data = data)
    ps_df <- data.frame(
        pr_score = predict(ps, type = "response"),
        count_point_id = data$count_point_id,
        treatment = data[[ind_var_name]]
    )
    write.csv(ps_df, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_propensity_score.csv"))

    match_result_df_cor <- match_result_df %>%
        dplyr::select(covariates) %>%
        dplyr::select(-c(year, month))
    corrmatrix <- cor(match_result_df_cor, use = "complete.obs")
    corrdf <- corrmatrix %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Var1") %>%
        gather("Var2", "value", -Var1)

    print(corrdf %>%
        filter((value >= 0.6 | value <= -0.6) & (Var1 != Var2)))
    right_side <- paste0(ind_var_name, " + ", covariates_pasted)
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
    print(formula)
    model_year_negative_binomial <- glm.nb(formula, data = match_result_df) # No need for weights argument here unless you want to include it
    model_year_negative_binomial_summary <- summary(model_year_negative_binomial) # cluster argument removed as it's not applicable to glm.nb
    capture.output(model_year_negative_binomial_summary, file = paste0(model_dir, "/", ind_var_name, "/", ind_var_name, "_psm_year_fe_nb.txt"))
    models$second_stage <- model_year_negative_binomial
    # return model object for stargazer later
    return(models)
}


# ipw
run_ipw <- function(data, dep_var_name, ind_var_name, covariates, model_dir, figure_dir) {
    # check balance
    covariates_pasted <- paste(covariates, collapse = " + ")
    formula <- as.formula(paste(ind_var_name, " ~ ", covariates_pasted))
    weight_result <- weightit(formula = formula, data = data, estimand = "ATT", method = "ps")
    weight_result <- trim(weight_result, at = 0.95)
    summary_weight_result <- bal.tab(weight_result, stats = c("c", "m"), un = TRUE, thresholds = c(cor = .1))
    capture.output(summary_weight_result, file = paste0(model_dir, "/", gind_var_name, "/", ind_var_name, "_ipw_summary_balance.txt"))

    # plot
    love_plot <- love.plot(weight_result,
        stats = c("c"),
        thresholds = c(cor = .1),
        abs = TRUE, wrap = 20,
        var.order = "unadjusted", line = TRUE
    ) +
        scale_colour_manual(values = alpha(c("#7B52AE", "#74B652"), 0.5)) +
        theme_ipsum()
    ggsave(paste0(figure_dir, "/", ind_var_name, "_weight_result.png"), love_plot, height = 4, width = 10)

    # model
    right_side <- paste0(ind_var_name, " + ", covariates_pasted)
    zero_condition_covariates <- paste(covariates[str_detect(covariates, paste("od_", "pop_den", "poi", sep = "|"))],
        collapse = " + "
    )
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side, "|", zero_condition_covariates))
    model_year_zero_inflated <- zeroinfl(formula, dist = "negbin", link = "logit", data = data, weights = weight_result$weights)
    model_year_zero_inflated_summary <- summary(model_year_zero_inflated, cluster = c("subclass"))
    capture.output(model_year_zero_inflated_summary, file = paste0(model_dir, "/", gind_var_name, "/", ind_var_name, "_ipw_year_fe_zinb.txt"))

    # return model object for stargazer later
    return(model_year_zero_inflated)
}


# causal forest
run_causal_forest <- function(data, dep_var_name, ind_var_name, covariates_names, treatment) {
    print(ind_var_name)
    X <- data %>%
        dplyr::select(covariates_names) %>%
        as.matrix() %>%
        unlist()
    Y <- data %>%
        dplyr::select(all_of(dep_var_name)) %>%
        dplyr::pull()
    W <- data %>%
        dplyr::select(all_of(ind_var_name)) %>%
        dplyr::pull()
    tau.forest <- grf::causal_forest(X, Y, W, seed = 1234)
    # Estimate treatment effects for the training data using out-of-bag prediction.
    # tau.hat.oob <- predict(tau.forest)
    # pdf(paste0(figure_dir, "/", ind_var_name,"_causal_forest.pdf"), height = 7, width = 7)
    # plot <- hist(tau.hat.oob$predictions)
    # print(plot)
    # dev.off()

    # variable importance
    forest.Y.varimp <- variable_importance(tau.forest)
    df <- data.frame(var_name = covariates_names, variable_importance = forest.Y.varimp)
    # save the result
    df %>%
        write.csv(paste0(model_dir, "/", ind_var_name, "/", treatment, "_", "causal_forest_var_imp.csv"), row.names = F)

    # conditional average treatment effect
    cate <- average_treatment_effect(tau.forest, target.sample = "all")
    t_score <- cate["estimate"] / cate["std.err"]
    cate["p_value"] <- 2 * pt(q = abs(t_score), df = length(Y) - 1, lower.tail = FALSE)
    capture.output(cate, file = paste0(model_dir, "/", ind_var_name, "/", treatment, "_", "causal_forest_cate.txt"))

    # Extract the first tree from the fitted forest.
    tau.forest.2 <- grf::causal_forest(X, Y, W, seed = 1234, min.node.size = 10)
    tree <- get_tree(tau.forest.2, 1)
    # Print the first tree.
    print(tree)
    capture.output(tree, file = paste0(model_dir, "/", ind_var_name, "/", treatment, "_", "causal_tree.txt"))
    # Plot the first tree.
    tree_plot <- plot(tree)
    tree_plot <- DiagrammeRsvg::export_svg(tree_plot)
    tree_plot <- charToRaw(tree_plot) # flatten
    rsvg::rsvg_pdf(tree_plot, paste0(figure_dir, "/", treatment, "_", ind_var_name, "_causal_tree.pdf")) # saved graph as png

    # plot HTE by ranking
    cf_preds <- predict(tau.forest, estimate.variance = TRUE)

    # save as csv
    cf_preds %>%
        new_tibble(cf_preds) %>%
        write.csv(paste0(model_dir, "/", ind_var_name, "/", treatment, "_predictions.csv"), row.names = F)
}

# compute rate
compute_rate <- function(data, dep_var_name, ind_var_name, covariates_names, treatment, model_dir, figure_dir) {
    print(ind_var_name)
    X <- data %>%
        dplyr::select(covariates_names) %>%
        as.matrix() %>%
        unlist()
    Y <- data %>%
        dplyr::select(all_of(dep_var_name)) %>%
        dplyr::pull()
    W <- data %>%
        dplyr::select(all_of(ind_var_name)) %>%
        dplyr::pull()
    # plot rate
    train <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.5, 0.5))
    train.forest <- causal_forest(X[train, ], Y[train], W[train])
    eval.forest <- causal_forest(X[-train, ], Y[-train], W[-train])
    rate <- rank_average_treatment_effect(
        eval.forest,
        predict(train.forest, X[-train, ])$predictions
    )
    pdf(paste0(figure_dir, "/", treatment, "_", ind_var_name, "_rate.pdf"), height = 7, width = 7)
    plot(rate)
    dev.off()
    autoc <- paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))
    capture.output(autoc, file = paste0(model_dir, "/", ind_var_name, "/", treatment, "_", "autoc.txt"))
}

# compute cate ranking
compute_cate_ranking <- function(data, dep_var_name, ind_var_name, covariates_names) {
    # initialize list
    df_list <- list()
    # Valid randomized data and observational data with unconfoundedness+overlap.
    # Note: read the comments below carefully.
    # In randomized settings, do not estimate forest.e and e.hat; use known assignment probs.

    # Prepare dataset
    fmla <- formula(paste0("~ 0 + ", paste0(covariates_names, collapse = "+")))
    X <- model.matrix(fmla, data)
    W <- data[, ind_var_name] %>% 
        dplyr::pull()
    Y <- data[, dep_var_name] %>% 
        dplyr::pull()

    # Number of rankings that the predictions will be ranking on
    # (e.g., 2 for above/below median estimated CATE, 5 for estimated CATE quintiles, etc.)
    num.rankings <- 4

    # Prepare for data.splitting
    # Assign a fold number to each observation.
    # The argument 'clusters' in the next step will mimick K-fold cross-fitting.
    num.folds <- 10
    folds <- sort(seq(nrow(data)) %% num.folds) + 1

    # Comment or uncomment depending on your setting.
    # Observational setting with unconfoundedness+overlap (unknown assignment probs):
    forest <- causal_forest(X, Y, W, clusters = folds, seed = 1234)
    # Randomized settings with fixed and known probabilities (here: 0.5).
    # forest <- causal_forest(X, Y, W, W.hat=.3, clusters = folds, seed=1234)

    # Retrieve out-of-bag predictions.
    # Predictions for observation in fold k will be computed using
    # trees that were not trained using observations for that fold.
    tau.hat <- predict(forest)$predictions

    # Rank observations *within each fold* into quintiles according to their CATE predictions.
    ranking <- rep(NA, nrow(data))
    for (fold in seq(num.folds)) {
        tau.hat.quantiles <- quantile(tau.hat[folds == fold], probs = seq(0, 1, by = 1 / num.rankings))
        ranking[folds == fold] <- cut(tau.hat[folds == fold], tau.hat.quantiles, include.lowest = TRUE, labels = seq(num.rankings))
    }

    # Computing AIPW scores.
    tau.hat <- predict(forest)$predictions
    e.hat <- forest$W.hat # P[W=1|X]
    m.hat <- forest$Y.hat # E[Y|X]

    # Estimating mu.hat(X, 1) and mu.hat(X, 0) for obs in held-out sample
    # Note: to understand this, read equations 6-8 in this vignette:
    # https://grf-labs.github.io/grf/articles/muhats.html
    mu.hat.0 <- m.hat - e.hat * tau.hat # E[Y|X,W=0] = E[Y|X] - e(X)*tau(X)
    mu.hat.1 <- m.hat + (1 - e.hat) * tau.hat # E[Y|X,W=1] = E[Y|X] + (1 - e(X))*tau(X)

    # AIPW scores
    aipw.scores <- tau.hat + W / e.hat * (Y - mu.hat.1) - (1 - W) / (1 - e.hat) * (Y - mu.hat.0)
    ols <- lm(aipw.scores ~ 0 + factor(ranking))
    forest.ate <- data.frame("aipw", paste0("Q", seq(num.rankings)), coeftest(ols, vcov = vcovHC(ols, "HC2"))[, 1:2])
    colnames(forest.ate) <- c("method", "ranking", "estimate", "std.err")
    rownames(forest.ate) <- NULL # just for display
    df_list$forest.ate <- forest.ate

    df <- mapply(function(covariate) {
        # Looping over covariate names
        # Compute average covariate value per ranking (with correct standard errors)
        fmla <- formula(paste0(covariate, "~ 0 + ranking"))
        ols <- lm(fmla, data = transform(data, ranking = factor(ranking)))
        ols.res <- coeftest(ols, vcov = vcovHC(ols, "HC2"))

        # Retrieve results
        avg <- ols.res[, 1]
        stderr <- ols.res[, 2]

        # Tally up results
        data.frame(covariate, avg, stderr,
            ranking = paste0("Q", seq(num.rankings)),
            # Used for coloring
            scaling = pnorm((avg - mean(avg)) / sd(avg)),
            # We will order based on how much variation is 'explain' by the averages
            # relative to the total variation of the covariate in the data
            variation = sd(avg) / sd(pull(data[, covariate])),
            # String to print in each cell in heatmap below
            labels = paste0(signif(avg, 3), "\n", "(", signif(stderr, 3), ")")
        )
    }, covariates_names, SIMPLIFY = FALSE)
    df <- do.call(rbind, df)

    # a small optional trick to ensure heatmap will be in decreasing order of 'variation'
    df$covariate <- reorder(df$covariate, order(df$variation))

    df_list$df <- df
    return(df_list)
}

# compute hte by covariate
compute_hte_subgroups <- function(data, dep_var_name, ind_var_name, covariates_names) {
    # define a function for a simple cf model
    cf_simple <- function(data, dep_var_name, ind_var_name, covariates_names) {
        # run causal forest
        X <- data %>%
            dplyr::select(any_of(covariates_names)) %>%
            as.matrix() %>%
            unlist()
        Y <- data %>%
            dplyr::select(dep_var_name) %>%
            as.vector() %>%
            unlist()
        W <- data %>%
            dplyr::select(ind_var_name) %>%
            as.vector() %>%
            unlist()
        tau.forest <- causal_forest(X, Y, W, seed = 1234)

        # get CATE and SE
        cate <- average_treatment_effect(tau.forest, target.sample = "treated")
        return(cate)
    }
    # create a dataframe to store the results
    hte_df <- data.frame(matrix(ncol = 4, nrow = 0))
    col_names <- c("covariate", "category", "estimate", "std.err")
    colnames(hte_df) <- col_names
    for (selected_covariate in covariates_names) {
        print(selected_covariate)

         # Split data into three groups based on the tertiles
        q33 <- quantile(data[[selected_covariate]], 1/3)
        q66 <- quantile(data[[selected_covariate]], 2/3)

        data_group1 <- data[data[[selected_covariate]] <= q33, ]
        data_group2 <- data[data[[selected_covariate]] > q33 & data[[selected_covariate]] <= q66, ]
        data_group3 <- data[data[[selected_covariate]] > q66, ]
        # Check if any group is empty
        if (nrow(data_group1) == 0 | nrow(data_group2) == 0 | nrow(data_group3) == 0) {
            print(paste("Skipping covariate", selected_covariate, "due to insufficient data for grouping"))
            next
        }
        # get CATE and SE for each group
        cate_group1 <- cf_simple(data_group1, dep_var_name, ind_var_name, covariates_names)
        cate_group2 <- cf_simple(data_group2, dep_var_name, ind_var_name, covariates_names)
        cate_group3 <- cf_simple(data_group3, dep_var_name, ind_var_name, covariates_names)

        # complete the vectors
        cate_group1["covariate"] <- selected_covariate
        cate_group2["covariate"] <- selected_covariate
        cate_group3["covariate"] <- selected_covariate

        cate_group1["category"] <- "0-33%"
        cate_group2["category"] <- "33-66%"
        cate_group3["category"] <- "66-100%"

        # rbind to hte_df
        hte_df <- rbind(hte_df, data.frame(as.list(cate_group1)))
        hte_df <- rbind(hte_df, data.frame(as.list(cate_group2)))
        hte_df <- rbind(hte_df, data.frame(as.list(cate_group3)))
    }
    return(hte_df)
}

# compute overall variable importance
compute_overall_var_imp <- function(data, treatment, model_dir) {
    # run a normal causal forest just to check feature importance
    X <- data %>%
        dplyr::select(-contains(c("count", "binary"))) %>%
        as.matrix() %>%
        unlist()

    Y <- data %>%
        dplyr::select("count_log") %>%
        as.vector() %>%
        unlist()

    regression_forest <- regression_forest(X, Y)
    varimp <- variable_importance(regression_forest)
    var_name <- data %>%
        dplyr::select(-contains(c("count", "binary"))) %>%
        names()
    df <- data.frame(var_name = var_name, variable_importance = varimp)
    df %>%
        write.csv(paste0(model_dir, "/", treatment, "_", "causal_forest_var_imp.csv"), row.names = F)
}

# TODO
run_random_forest <- function(data, dep_var_name, ind_var_name, covariates_names, model_dir) {
    data <- data %>%
        dplyr::select(all_of(c(dep_var_name, ind_var_name, covariates_names)))
    covariates_pasted <- paste(c(ind_var_name, covariates_names), collapse = " + ")
    formula <- as.formula(paste(dep_var_name, " ~ ", covariates_pasted))
    # # Set up a parallel backend with 4 cores
    # cl <- makeCluster(4)
    # registerDoParallel(cl)
    # model <- foreach(ntree=500, .packages='randomForest') %dopar% {
    #                 randomForest(formula, data = data, ntree=ntree)
    #               }
    # # Stop the parallel backend
    # stopCluster(cl)
    model <- randomForest(formula, data = data, ntree = 500)
    # plot
    partial_df <- pdp::partial(model, pred.var = ind_var_name, train = data)
    partial_df %>%
        autoplot(rug = TRUE, train = data) +
        geom_line(color = "#7B52AE") +
        # geom_point(color = "red", size = 2) +
        labs(x = ind_var_name, y = "Predicted counts") +
        theme_ipsum()
    # save
    ggsave(paste0(figure_dir, "/", ind_var_name, "_pdp.png"), width = 10, height = 10)
    partial_df %>% write.csv(paste0(model_dir, "/", ind_var_name, "/pdp.csv"))
}

run_fe_poisson <- function(data, dep_var_name, ind_var_name, model_dir) {
    # create dir
    if (!dir.exists(paste0(model_dir, "/", ind_var_name))) {
        dir.create(paste0(model_dir, "/", ind_var_name))
    } else {
        print("Dir already exists!")
    }
    # run base model
    formula <- as.formula(paste(dep_var_name, " ~ ", ind_var_name))
    print(formula)
    model_year_fe_poisson <- model_year_fe_poisson <- glm(formula, family = poisson(), data = data)
    model_year_fe_poisson_summary <- summary(model_year_fe_poisson)
    capture.output(model_year_fe_poisson_summary, file = paste0(model_dir, "/", ind_var_name, "/", "year_fe_poisson_base.txt"))
    # add other covariates one by one
    data_covar <- data %>%
        select(-c(ind_var_name, dep_var_name)) %>%
        select(1:ncol(.))
    cov_names <- names(data_covar)
    names_agg <- c(ind_var_name)
    pb <- progress_bar$new(total = length(cov_names))
    for (cov_name in cov_names) {
        pb$tick()
        names_agg <- c(names_agg, cov_name)
        right_side <- paste(names_agg, collapse = " + ")
        formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
        model_year_fe_poisson <- glm(formula, family = poisson(), data = data)
        model_year_fe_poisson_summary <- summary(model_year_fe_poisson)
        capture.output(model_year_fe_poisson_summary, file = paste0(model_dir, "/", ind_var_name, "/", "year_fe_poisson", as.character(length(names_agg)), ".txt"))
    }
}

run_zero_inflated <- function(data, dep_var_name, ind_var_name, control_var_vec, count_model, model_dir) {
    # list to store result
    name_list <- list()
    estimate_list <- list()
    p_val_list <- list()
    # create dir
    # create dir
    if (!dir.exists(paste0(model_dir, "/", ind_var_name))) {
        dir.create(paste0(model_dir, "/", ind_var_name))
    } else {
        print("Dir already exists!")
    }
    # run base model
    right_side <- paste(c(ind_var_name, control_var_vec), collapse = " + ")
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
    print(formula)
    model_year_zero_inflated <- zeroinfl(formula, dist = count_model, link = "logit", data = data)
    model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
    capture.output(model_year_zero_inflated_summary, file = paste0(model_dir, "/", ind_var_name, "/", "year_zero_inflated_", count_model, "_base.txt"))
    # add other covariates one by one
    data_covar <- data %>%
        dplyr::select(-c(ind_var_name, dep_var_name, control_var_vec)) %>%
        dplyr::select(1:ncol(.)) %>%
        dplyr::select(-contains("binary"), -contains("count_point_id"))
    cov_names <- names(data_covar)
    names_agg <- c(ind_var_name, control_var_vec)
    pb <- progress_bar$new(total = length(cov_names))
    counter <- 1
    for (cov_name in cov_names) {
        print(cov_name)
        pb$tick()
        right_side <- paste(c(names_agg, cov_name), collapse = " + ")
        formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
        print(formula)
        model_year_zero_inflated <- zeroinfl(formula, dist = count_model, link = "logit", data = data)
        model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
        print(model_year_zero_inflated_summary)
        # append the model result
        # add covariate's name, point estimate, and p-value
        point_est <- model_year_zero_inflated_summary[["coefficients"]][["count"]][2, 1]
        p_val <- model_year_zero_inflated_summary[["coefficients"]][["count"]][2, 4]
        name_list <- append(name_list, cov_name)
        estimate_list <- append(estimate_list, point_est)
        p_val_list <- append(p_val_list, p_val)
        # save to txt file
        capture.output(model_year_zero_inflated_summary, file = paste0(model_dir, "/", ind_var_name, "/", "year_zero_inflated", as.character(counter), ".txt"))
        counter <- counter + 1
    }
    # convert model_result_list to df and save as csv
    model_result_list <- list(
        variable = name_list,
        point_estimate = estimate_list,
        p_value = p_val_list
    )
    df <- as.data.frame(do.call(cbind, model_result_list))
    print(df)
    df %>%
        as.matrix() %>%
        write.csv(paste0(model_dir, "/", ind_var_name, "/", "year_zero_inflated_", count_model, "_result.csv"), row.names = F)
}

run_negative_binomial <- function(data, dep_var_name, ind_var_name, control_var_vec, model_dir) {
    # Ensure the MASS package is loaded
    require(MASS)

    # list to store result
    name_list <- list()
    estimate_list <- list()
    p_val_list <- list()
    # create dir
    if (!dir.exists(paste0(model_dir, "/", ind_var_name))) {
        dir.create(paste0(model_dir, "/", ind_var_name))
    } else {
        print("Dir already exists!")
    }
    # run base model
    right_side <- paste(c(ind_var_name, control_var_vec), collapse = " + ")
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
    print(formula)
    model_year_negative_binomial <- glm.nb(formula, data = data)
    model_year_negative_binomial_summary <- summary(model_year_negative_binomial)
    capture.output(model_year_negative_binomial_summary, file = paste0(model_dir, "/", ind_var_name, "/", "year_negative_binomial_base.txt"))
    # add other covariates one by one
    data_covar <- data %>%
        dplyr::select(-c(ind_var_name, dep_var_name, control_var_vec)) %>%
        dplyr::select(1:ncol(.)) %>%
        dplyr::select(-contains("binary"), -contains("count_point_id"))
    cov_names <- names(data_covar)
    names_agg <- c(ind_var_name, control_var_vec)
    pb <- progress_bar$new(total = length(cov_names))
    counter <- 1
    for (cov_name in cov_names) {
        print(cov_name)
        pb$tick()
        right_side <- paste(c(names_agg, cov_name), collapse = " + ")
        formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
        print(formula)
        model_year_negative_binomial <- glm.nb(formula, data = data)
        model_year_negative_binomial_summary <- summary(model_year_negative_binomial)
        print(model_year_negative_binomial_summary)
        # append the model result
        # add covariate's name, point estimate, and p-value
        point_est <- model_year_negative_binomial_summary$coefficients[2, 1]
        p_val <- model_year_negative_binomial_summary$coefficients[2, 4]
        name_list <- append(name_list, cov_name)
        estimate_list <- append(estimate_list, point_est)
        p_val_list <- append(p_val_list, p_val)
        # save to txt file
        capture.output(model_year_negative_binomial_summary, file = paste0(model_dir, "/", ind_var_name, "/", "year_negative_binomial", as.character(counter), ".txt"))
        counter <- counter + 1
    }
    # convert model_result_list to df and save as csv
    model_result_list <- list(
        variable = name_list,
        point_estimate = estimate_list,
        p_value = p_val_list
    )
    df <- as.data.frame(do.call(cbind, model_result_list))
    print(df)
    df %>%
        as.matrix() %>%
        write.csv(paste0(model_dir, "/", ind_var_name, "/", "year_negative_binomial_result.csv"), row.names = F)
}
