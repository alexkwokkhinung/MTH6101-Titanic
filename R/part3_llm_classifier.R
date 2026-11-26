source("R/00_setup.R")
source("R/part1_cleaning_eda_full_model.R")
#add code below
raw <- read.csv("data/train.csv", na.strings = c("", "NA"), stringsAsFactors = FALSE)
fields <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
passengers <- raw[complete.cases(raw[fields]), ]
survivors <- which(passengers$Survived == 1)
deaths <- which(passengers$Survived == 0)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(1)
training <- c(sample(survivors, round(0.8 * length(survivors))),
              sample(deaths, round(0.8 * length(deaths))))
test <- setdiff(seq_len(nrow(passengers)), training)

full <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
            data = passengers[training, ], family = binomial())
reduced <- update(full, . ~ . - Parch - Embarked)
print(AIC(full, reduced))
stopifnot(AIC(reduced) < AIC(full))

set.seed(6103)
chosen <- c(sample(intersect(test, survivors), 12),
            sample(intersect(test, deaths), 18))
chosen <- sample(chosen)
cases <- passengers[chosen, ]
case_id <- sprintf("C%02d", seq_len(nrow(cases)))

results <- "part3_results"
template <- paste(readLines(file.path(results, "prompt_template.txt")), collapse = "\n")
predictors <- c("Pclass", "Sex", "Age", "SibSp", "Fare")
make_prompt <- function(i) {
  prompt <- template
  for (field in predictors) {
    value <- cases[[field]][i]
    if (is.numeric(value)) value <- sprintf("%.12g", value)
    prompt <- gsub(paste0("{", field, "}"), value, prompt, fixed = TRUE)
  }
  prompt
}
prompts <- vapply(seq_len(nrow(cases)), make_prompt, character(1))

read_result <- function(name) read.csv(file.path(results, name), stringsAsFactors = FALSE)
logged <- read_result("prompts.csv")
key <- read_result("evaluation_key.csv")
responses <- read_result("llm_predictions.csv")
stopifnot(identical(case_id, logged$case), identical(prompts, logged$prompt),
          identical(case_id, key$case), identical(key$PassengerId, cases$PassengerId),
          identical(key$Survived, cases$Survived), all(chosen %in% test),
          identical(case_id, responses$case), all(responses$llm_prediction %in% 0:1),
          all(nzchar(responses$explanation)))
print(table(LLM_prediction = responses$llm_prediction))
