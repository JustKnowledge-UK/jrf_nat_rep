rm(list = ls())

library(haven)
library(poLCA)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read_sav("./Data/uncleaned_full_data.sav")

# LCA
# prepare LCA

f1 <- as.formula(cbind(Q1_1,
                       Q1_2,
                       Q1_3,
                       Q1_4,
                       Q1_5,
                       Q1_6) ~ 1)

# 1 class
LCA_1 <- poLCA(f1, data, nclass = 1)

# 2 classes
LCA_2 <- poLCA(f1, data, nclass = 2)
plot(LCA_2)

# save 2 class to use later:
# saveRDS(LCA_2, file = "./outputs/Q3_LCA_2.Rds")

# 3 classes
LCA_3 <- poLCA(f1, data, nclass = 3)
plot(LCA_3)

# 4 classes
LCA_4 <- poLCA(f1, data, nclass = 4)
plot(LCA_3)

ics <- data.frame("classes" = seq(1,4,1),
                  "ic" = c(rep("aic", 4),
                           rep("bic", 4)),
                  "value" = c(LCA_1[["aic"]],
                              LCA_2[["aic"]],
                              LCA_3[["aic"]],
                              LCA_4[["aic"]],
                              LCA_1[["bic"]],
                              LCA_2[["bic"]],
                              LCA_3[["bic"]],
                              LCA_4[["bic"]])
)

# 'elbow' at 2 classes suggests additional classes beyond 2 don't explain much
ggplot(ics, aes(classes, value, colour = ic, group = ic)) +
  geom_point() +
  geom_line()

# change from scientific notation
options(scipen = 999)

# get the class probabilities
lca2_class_probs <- as.data.frame(t(as.data.frame(LCA_2[["probs"]])[,c(TRUE, FALSE)])) %>%
  mutate_all(~round(., 4))

lca3_class_probs <- as.data.frame(t(as.data.frame(LCA_3[["probs"]])[,c(TRUE, FALSE)])) %>%
  mutate_all(~round(., 4))

# add predicted class variable from LCA
data <- data %>%
  mutate(
    predicted_class_2 = LCA_2[["predclass"]],
    predicted_class_3 = LCA_3[["predclass"]]
  )

# quick summary of predicted memberships
data %>%
  group_by(predicted_class_2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = 100 * (n / sum(n)),
  )  %>%
  ggplot(., aes(predicted_class_2, perc)) +
  geom_col(position = "dodge", colour = "black") +
  scale_y_continuous(breaks = seq(0,60,10)) +
  geom_label(aes(label = round(perc,2)))

# 2 classes come out overall - one with high loadings on all, and one with low loadings
# but relatively high on 3 and 6. suggests these indicators are more broadly applciable
# beyond outsrouced work7

# should do an lca with q3v3a added as a covariate, to see how gruopigns cahnge depending
# on what people reported fro this question. or will this just tell us the same as before?

