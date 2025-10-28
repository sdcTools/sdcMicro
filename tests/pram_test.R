# Debugging the pram() function
################################################################################

# load functions
devtools::load_all()

# create toy data for testing ----

set.seed(123)

n <- 100  # number of records

# generate ages
age <- sample(18:70, n, replace = TRUE)

# age categories (separate groups for students <25 and retired >60)
age_category <- cut(
  age,
  breaks = c(17, 24, 59, 70),
  labels = c("18-24", "25-59", "60-70")
)

# gender
gender <- sample(c("Male", "Female", "Other"), n, replace = TRUE, prob = c(0.48, 0.48, 0.04))

# income (different ranges depending on age)
income <- ifelse(age < 25, round(rnorm(n, 15000, 5000), -2),
                 ifelse(age >= 60, round(rnorm(n, 30000, 8000), -2),
                        round(rnorm(n, 50000, 15000), -2)))

# occupation (depends on age)
occupation <- ifelse(age < 25, "Student",
                     ifelse(age >= 60, "Retired",
                            sample(c("Teacher", "Engineer", "Doctor", "Artist", "Clerk"),
                                   n, replace = TRUE)))

# combine
df <- data.frame(
  age, age_category, gender, income, occupation
)

df$gender <- as.factor(df$gender)
df$occupation <- as.factor(df$occupation)

# create sdcObject VERSION 1 ----
sdcV1 <- createSdcObj(dat = df,
                      keyVars = c("age", "gender", "occupation"),
                      sensibleVar = "income")

sdcV1 <- pram(sdcV1,
              variables = c("gender", "occupation"),
              strata_variables = "age_category")
print(sdcV1, type = "pram")

# extract dataset
df1_pram <- extractManipData(sdcV1)

# check whether pram was applied within strata
table(df1_pram$age_category, df1_pram$occupation)

# create sdcObject VERSION 2----
sdcV2 <- createSdcObj(dat = df,
                      keyVars = "age",
                      pramVars = c("gender", "occupation"),
                      sensibleVar = "income",
                      strataVar = "age_category")
sdcV2 <- pram(sdcV2)

print(sdcV2, type = "pram")

# extract dataset
df2_pram <- extractManipData(sdcV2)

# check whether pram was applied within strata
table(df2_pram$age_category, df2_pram$occupation)


# Test with non default pd ----
pd_scal <- 0.4
pd_vec <- c(0.4, 0.6)
pd_matrix <- `Rs_18-24`
pd_matrix[6, 6] <- 0
pd_matrix[5, 5] <- 0
pd_matrix[6, 4] <- 1
pd_matrix[5, 4] <- 1

sdcV1 <- createSdcObj(dat = df,
                      keyVars = c("age", "gender", "occupation"),
                      sensibleVar = "income")

sdcV1 <- pram(sdcV1,
              pd = c(0.5, pd_matrix),
              variables = c("gender", "occupation"),
              strata_variables = "age_category")

# extract dataset
df1_pram <- extractManipData(sdcV1)

# check whether pram was applied within strata
table(df1_pram$age_category, df1_pram$occupation)
