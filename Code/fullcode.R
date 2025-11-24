setwd("C:\\Users\\chris\\Documents\\MAT 261A\\Project 2\\Data")

df = read.csv("Trans Athletes DATA.csv")
rp = subset(df, republican == 1)

dm = subset(df, democrat == 1)

hist(rp$margin_of_victory)
hist(dm$margin_of_victory)

# both are right skewed

