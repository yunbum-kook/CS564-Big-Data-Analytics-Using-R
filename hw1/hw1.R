### HW1 ###
###########
# Problem 1
x=c(1,2,3,4,5,2,4,3,5,1,2,3,4,5,1,2)
y=c("Red","Green","Blue","Magenta")
y[x]

# Problem 2
A <- matrix(c(1,2,3,0,1,4,5,2,4), nrow=3, byrow=TRUE)
B <- matrix(c(2,3,0,-1,2,5,3,9,2), nrow=3, byrow=TRUE)
A
B
C <- A %*% B
C

# Problem 3
# (1)
df <- as.data.frame(state.x77)
str(df)
class(df)

# (2)
income4000 <- df$Income[df$Income < 4000]
length(income4000)

# (3)
index_of_highest_income <- which(rank(-df$Income) == 1)
index_of_highest_income
rownames(df)[index_of_highest_income]