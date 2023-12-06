
### load the libraries
library(shiny)
library(tidyverse)
library(psych)
library(readxl)
library(readr)
library(caret)
library(pROC)
library(broom)
library(rsconnect)

### download the data
urlfile <- "https://raw.githubusercontent.com/abeuoy/shiny-for-DATA-824/main/wine_df.csv"
wine_df <- read_csv(url(urlfile))

### import the data set and drop wine quality
wine_df <- read_csv("wine_df.csv") %>%
  dplyr::select(- quality)

### recode wine_type to numeric 
wine_df <- wine_df %>%
  mutate(
    wine_recode = case_when(
      wine_type == "red" ~ 0,
      TRUE ~ 1
    )
  )

## convert recoded variable to factor
wine_df$wine_recode <- as.factor(wine_df$wine_recode) 
wine_df <- wine_df %>%
  dplyr::select(- wine_type)

### describe the data set and calculate freq table for wine type
## numeric variables
describe(wine_df[, 1:11])

## wine type freq table
table(wine_df$wine_recode)


### create a training and test set
## create object with test and training set. Training set will contain 70% of
## the data
index <- createDataPartition(wine_df$wine_recode, p = .70, list = FALSE)

## create training set
train <- wine_df[index, ]

## create test set
test <- wine_df[- index, ]

### describe the test and training set
## put origin variable on different data sets
train_for_describe <- train
train_for_describe$origin <- "training set"
test_for_describe <- test
test_for_describe$origin <- "test set"

## combine both data sets with origin variables
datasets <- rbind(train_for_describe, test_for_describe)

## describe the datasets by wine type and origin
describe_datasets <- describeBy(datasets[, 1:11], group = list(datasets$origin),
                                mat = TRUE, digits = 2) %>%
  rownames_to_column(var = "variable") %>%
  dplyr::select(- c(item, vars)) %>%
  rename("origin" = group1)

### train the model
training_model <- glm(wine_recode ~ ., family = binomial(), data = train)
## model summary
summary(training_model)

### tidy output for training model
## estimates
tidy_model <- as.data.frame(broom::tidy(training_model))
tidy_model <- format(tidy_model, digits = 3)

# model
tidy_glance <- as.data.frame(glance(training_model))

### Predicting the test dataset
pred_prob <- predict(training_model, test, type = "response")

### convert probability to class values
## training set
# Converting from probability to actual output
train$pred_type <- ifelse(training_model$fitted.values >= 0.5, "white_wine", "red_wine")

# Generating the classification table
ctab_train <- table(train$wine_recode, train$pred_type)
ctab_train

## testing set
# Converting from probability to actual output
test$pred_type <- ifelse(pred_prob >= 0.5, "white_wine", "red_wine")

# Generating the classification table
ctab_test <- table(test$wine_recode, test$pred_type)
ctab_test

### calculate the accuracy
## Accuracy in Training dataset
accuracy_train <- sum(diag(ctab_train))/sum(ctab_train)*100
accuracy_train

## Accuracy in Test dataset
accuracy_test <- sum(diag(ctab_test))/sum(ctab_test)*100
accuracy_test

## Area under the curve
# create curve
roc <- roc(train$wine_recode, training_model$fitted.values)
auc <- as.data.frame(roc[["auc"]])
colnames(auc) <- "ROC"

# create the plot
roc_plot <- plot(roc, main = "ROC Curve for Logistic Regression Model")


ui <- fluidPage(
  ### Main title
  titlePanel("Binary Logistic Regression Predicting Type of Wine"),
  
  ### create side bar layout with input and output definitions
  sidebarLayout(
    
    ## sidebar panel for input
    sidebarPanel(
      
      # inputs to chose from
      selectInput("Output", "Choose an output:",
                  choices = c("Test and training descriptives", "Training Model Estimates",
                              "Training Model fit", "Cross Tabs for Training Set", 
                              "Cross Tabs for Test Set", "Accuracy for Training Set",
                              "Accuracy for Test Set", "ROC")),
    ),
    
    
    # Main panel for for displaying outputs
    mainPanel(
      tableOutput("table"))
  )
)

### Define server logic ----
server <- function(input, output) {
  
  ### Reactive value for selected data set
  table_input <- reactive({
    switch(input$Output,
           "Test and training descriptives" = describe_datasets, 
           "Training Model Estimates" = tidy_model,
           "Training Model fit" = tidy_glance, 
           "Cross Tabs for Training Set" = ctab_train, 
           "Cross Tabs for Test Set" = ctab_test, 
           "Accuracy for Training Set" = accuracy_train,
           "Accuracy for Test Set"= accuracy_test,
           "ROC" = auc)
    
  })
  
  ### Table of selected output
  output$table <- renderTable({
    table_input()
  })
  ### ROC plot
  output$plot <- renderPlot(
    roc_plot
  )
}

### Run the app ----
shinyApp(ui = ui, server = server)
