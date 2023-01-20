
# data generation --------------------------------------------------------------

generate_data <- function(inn, test = FALSE){
  
  npred   = inn[[1]]
  ev      = inn[[2]]
  n.level = inn[[3]]
  n       = inn[[4]]
  mu0     = inn[[5]]
  mu1     = inn[[6]]
  sigma0  = inn[[7]]
  sigma1  = inn[[8]]
  
  # test set
  if (test == TRUE){n  = n*10}
  
  # positive class
  n1      <- rbinom(1, n, ev)
  class_1 <- mvrnorm(n1, mu1, sigma1)
  
  # negative class
  n0      <- n - n1
  class_0 <- mvrnorm(n0, mu0, sigma0)
  
  outcome <- c(rep(1, n1), rep(0, n0))
  
  # format data frame
  df <- cbind(rbind(class_1, class_0), outcome) %>% 
    as.data.frame() %>% 
    mutate(outcome = as.factor(outcome))
  
  return(df)
}


# model implementation ---------------------------------------------------------

lrg <- function(df, test){
  mod  <- glm(outcome ~ ., family = "binomial", data = df)
  pred <- predict(mod,  newdata = test, type = "response")
  return(pred)
}


svc <- function(df, test){
  mod  <- svm(x = subset(df, select = -outcome), y = df$outcome, probability = T)
  pred <- predict(mod, newdata = subset(test, select = -outcome), probability = T)
  pred <- attr(pred, "probabilities")[,1]
  return(pred)
}


rnf <- function(df, test){
  mod  <- randomForest(outcome~., data = df) 
  pred <- predict(mod,  newdata = test, type = "prob")[,2]
  return(pred)
}


xgb <- function(df, test){
  train_x <- model.matrix(outcome ~ ., df)[,-1]
  train_y <- as.numeric(df$outcome) - 1
  xgb     <- xgboost(data = train_x,
                     label = train_y, 
                     max.depth = 10,
                     eta = 1,
                     nthread = 4,
                     nrounds = 4,
                     objective = "binary:logistic",
                     verbose = 2)
  pred <- predict(xgb, newdata = model.matrix(outcome~., test)[,-1])
  return(pred)
}


rub <- function(df, test){
  mod  <- rus(outcome ~., size = 10, alg = "c50", data = df)
  pred <- predict(mod, newdata = test)
  return(pred)
}


eas <- function(df, test){
  train_x <- df %>% dplyr::select(-outcome)
  train_y <- df %>% dplyr::pull(outcome)
  test_x  <- test %>% dplyr::select(-outcome)
  
  mod  <- EasyEnsemble(train_x, train_y)
  pred <- predict(mod, test_x, type = "probability")$X1
  return(pred)
}

pred_probs <- function(df, test){
  tryCatch.W.E({
    a <- tibble(
      class = test$outcome, 
      lrg   = lrg(df, test), 
      svm   = svc(df, test), 
      rnf   = rnf(df, test), 
      xgb   = xgb(df, test), 
      rub   = rub(df, test), 
      zee   = eas(df, test),
    )
  })
  return(a)
}


# performance metrics ----------------------------------------------------------

brier_score <- function(probs, outcome){
  outcome <- as.numeric(outcome) - 1
  mean((probs - outcome)^2)
}


calibration_intercept <- function(probs, outcome){
  if(sum(probs == 1) != 0){
    probs[probs == 1] <- 0.999
  }
  if(sum(probs == 0) != 0){
    probs[probs == 0] <- 0.001
  }
  mod <- glm(outcome ~ 1, offset = log(probs/(1-probs)), family = "binomial")
  int <- coef(mod)[1]
  return(int)
}

calibration_slope <- function(probs, outcome){
  if(sum(probs == 1) != 0){
    probs[probs == 1] <- 0.999
  }
  if(sum(probs == 0) != 0){
    probs[probs == 0] <- 0.001
  }
  mod <- glm(outcome ~ log(probs/(1-probs)), family = "binomial")
  slp <- coef(mod)[2]
  return(slp)
}


# apply performance metrics ----------------------------------------------------

get_brier_score <- function(tibble){
  vec = apply(tibble[-1], 2, brier_score, outcome = tibble$class)
  return(vec)
}


get_auc <- function(tibble){
  vec = apply(tibble[-1], 2, pROC::auc, response = tibble$class)
  return(vec)
}


get_int <- function(tibble){
  vec = apply(tibble[-1], 2, calibration_intercept, outcome = tibble$class)
  return(vec)
}


get_slp <- function(tibble){
  vec = apply(tibble[-1], 2, calibration_slope, outcome = tibble$class)
  return(vec)
}


get_stats <- function(auc, bri, int, slp){
  
  out <- 
    rbind(
      auc_med  = apply(auc, 2, mean),
      auc_iqr  = apply(auc, 2, sd),
      bri_med  = apply(bri, 2, mean),
      bri_iqr  = apply(bri, 2, sd), 
      int_med  = apply(int, 2, mean),
      int_iqr  = apply(int, 2, sd),
      slp_med  = apply(slp, 2, mean),
      slp_iqr  = apply(slp, 2, sd)
    )
  
  colnames(out) <- c("lrg", "svc", "rnf", "xgb", "rub", "zee")
  return(out)
}

# pretty plot ----------------------------------------------------

ready2plot <- function(pp){
  ppl <- 
    pp %>% 
    rename("Logistic Regression" = "lrg",
           "Support Vector Machine" = "svm",
           "Random Forest" = "rnf",
           "XGBoost" = "xgb",
           "RUSBoost" = "rub",
           "EasyEnsemble" = "zee")%>%
    pivot_longer(-class, names_to = "Method")%>%
    mutate(Method = fct_relevel(Method,
                                "Logistic Regression",
                                "Support Vector Machine",
                                "Random Forest",
                                "XGBoost",
                                "RUSBoost",
                                "EasyEnsemble"))
  return(ppl)
}
