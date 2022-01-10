library(tidyverse)
require(rpart)
require(partykit)
require(mvtnorm)
require(tree)
require(caret)

#EDA (Code from online)
mushroom <- read.delim(file.choose(), header = T, sep = ",")

colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")

mushroom <- mushroom %>% map_df(function(.x) as.factor(.x))

levels(mushroom$edibility) <- c("edible", "poisonous")
levels(mushroom$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroom$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushroom$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushroom$bruises) <- c("no", "yes")
levels(mushroom$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroom$gill_attachement) <- c("attached", "free")
levels(mushroom$gill_spacing) <- c("close", "crowded")
levels(mushroom$gill_size) <- c("broad", "narrow")
levels(mushroom$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushroom$stalk_shape) <- c("enlarging", "tapering")
levels(mushroom$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushroom$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$veil_type) <- "partial"
levels(mushroom$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushroom$ring_number) <- c("none", "one", "two")
levels(mushroom$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroom$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushroom$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroom$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

mushroom <- mushroom %>% select(- veil_type)

set.seed(1810)
split <- 0.7
inTrain   <- sample(1:nrow(mushroom), split*nrow(mushroom))
train.set <- mushroom[inTrain,]
test.set  <- mushroom[-inTrain,]

#Pruning Before Modeling
tree.model <- tree(edibility ~ ., data=mushroom); summary(tree.model)
cv.model <- cv.tree(tree.model); plot(cv.model)
cv.model$dev
best.size <- cv.model$size[which(cv.model$dev==min(cv.model$dev))]
best.size
cv.model.pruned <- prune.misclass(tree.model, best=best.size)
summary(cv.model.pruned)
plot(cv.model.pruned); text(cv.model.pruned)

#Model Fitting with cross validation after pruning
model <- tree(edibility ~ ., data=train.set); model
summary(model)
plot(model); text(model)
pred <- predict(model, test.set, type="class")
predict.table = table(test.set$edibility, pred); predict.table
confusionMatrix(predict.table, positive="edible", mode="prec_recall")

pruned.tree <- prune.tree(model, best=5)
plot(pruned.tree); text(pruned.tree)
pruned.pred <- predict(pruned.tree, test.set, type="class")
pruned.table <- table(pruned.pred, test.set$edibility)
confusionMatrix(pruned.table, positive="edible", mode="prec_recall")
