########################################################################
##                       Problem_01
########################################################################

library(readxl)
library(writexl)

Data_exm <-  read_xlsx("LRA441_221281.xlsx")

print(Data_exm$Y)
print(Data_exm$X1)
print(Data_exm$X2)

########################################################################
##                       Problem_02
########################################################################

Ys <- Data_exm$Y^2
X1s <- Data_exm$X1^2
X2s <- Data_exm$X2^2

E_data_1 <- matrix(data = c(Ys, X1s, X2s), ncol = 3)
colnames(E_data_1) <- c("Ys", "X1s", "X2s")

write_xlsx(as.data.frame(E_data_1), "Lab1_file1_export.xlsx", col_names = T)


########################################################################
##                       Problem_03
########################################################################

Y <- Data_exm$Y
X <- matrix(data = c(Data_exm$X1, Data_exm$X2), ncol = 2)

(ls.est <- solve(t(X) %*% X) %*% t(X) %*% Y)


########################################################################
##                       Problem_04
########################################################################

df <- data.frame(Data_exm$Y, Data_exm$X1, Data_exm$X2)
colnames(df) <- c("Y", "X1", "X2")


model <- lm(Y ~ 0 +  X1 + X2, data = df)
model$coefficients

