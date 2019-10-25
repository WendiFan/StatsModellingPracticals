#Ex1
Z <- rnorm(1000000)
mean(Z[Z >= 1])
mean(Z^6)

#Ex2
qchisq(0.05, 6, lower.tail = FALSE)

#Ex3
A <- matrix(c(3, 4, -2, 1, 2, -1, 7, -2, 6, 2, -1, 1, 1, 6, -2, 5), nrow = 4, ncol = 4, byrow = TRUE)
solve(A , c(9, 13, 11, 27) )

#Ex4
grades_1 <- c(10,11,14.5,15,15,18,12,19,18.5,19,20,13)
grades_2 <- c(12,11,14.5,13,12,11,12,14.5,20,17)
mean(grades_1)
mean(grades_2)
tstat <- mean(grades_1)-mean(grades_2)
all_grades <- c(grades_1,grades_2)
edtstat = rep(0, 10000)
for (i in 1:10000) {
 perm <- sample(all_grades)
 edtstat[i] <- mean(perm[1:length(grades_1)])-mean(perm[-(1:length(grades_1))])
 }
p_value <- mean(abs(tstat) <= abs(edtstat))
p_value

