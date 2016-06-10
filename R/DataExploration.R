#using the mtcars dataset 

mtcars = transform(mtcars,
                   am = factor(am, levels = c(0, 1), labels = c("automatic", "manual")),
                   vs = factor(vs, labels = c("V", "straight"))
)

summary(mtcars)
View(mtcars)

aggregate(mtcars$wt, by = mtcars[, "am", drop = FALSE], length) # like table 
aggregate(mtcars[, "wt"], by = mtcars[, "am", drop = FALSE], mean) # actually displays wt 
aggregate(mtcars[, c("mpg", "wt")], by = mtcars[, c("vs", "am")], median) # more than one factor! 

(mtcars.table <- table(mtcars[, c("vs", "am")])) # two categorical variables! 

plot(mtcars.table, main = "Contingency Table for am versus vs") # mosiac! 

