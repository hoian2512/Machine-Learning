library(glmnet)
# Đọc dữ liệu từ file csv
data <- read.csv("Hitters1.csv")
View(data)
x <- model.matrix(Salary ~ ., data = data)[, -1]
y <- data$Salary

# Phân chia dữ liệu thành tập huấn luyện và tập kiểm tra
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE)
test <- !train

# Tạo ma trận thiết kế cho tập huấn luyện
x.train = x[train,]
y.train = y[train]

# Thiết lập các giá trị lambda để sử dụng trong Lasso regression
grid <- 10^seq(2, -2, length = 100)

# Thiết lập số fold cho Cross-Validation
k <- 5

# Huấn luyện mô hình Lasso Regression và lựa chọn giá trị lambda tốt nhất bằng phương pháp 5-fold Cross-Validation
lasso.fit = cv.glmnet(x.train, y.train, alpha = 1, lambda = grid, nfolds = k)
plot(lasso.fit)
# Lấy giá trị lambda tốt nhất
best_lambda = lasso.fit$lambda.min
best_lambda
# Huấn luyện mô hình Lasso Regression trên toàn bộ dữ liệu với giá trị lambda tốt nhất
lasso_model = glmnet(x.train, y.train, alpha = 1, lambda =grid)
plot(lasso_model)
cv.out = cv.glmnet (x.train, y.train, alpha = 1)

plot ( cv.out )
# Đánh giá mô hình trên tập kiểm tra
lasso_pred <- predict(lasso_model, newx = x[!train, ])

y_test <- y[!train]
mean((lasso_pred - y_test)^2)
out = glmnet (x , y , alpha = 1, lambda = best_lambda )
lasso.coef <- predict(out, type = "coefficients", s = best_lambda)
lasso.coef
lasso.coef[lasso.coef != 0]

x <- model.matrix(Salary ~ ., data = data)[, -1]
y <- nhu1
# Phân chia dữ liệu thành tập huấn luyện và tập kiểm tra
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE)

test <- !train


# Tạo ma trận thiết kế cho tập huấn luyện
x.train = x[train,]
y.train = y[train]

# Thiết lập các giá trị lambda để sử dụng trong Lasso regression
grid <- 10^seq(10, -2, length = 100)

# Thiết lập số fold cho Cross-Validation
k <- 5

# Huấn luyện mô hình Lasso Regression và lựa chọn giá trị lambda tốt nhất bằng phương pháp 5-fold Cross-Validation
lasso.fit = cv.glmnet(x.train, y.train, alpha = 1, lambda = grid, nfolds = k)
plot(lasso.fit)
# Lấy giá trị lambda tốt nhất
best_lambda = lasso.fit$lambda.min
best_lambda
# Huấn luyện mô hình Lasso Regression trên toàn bộ dữ liệu với giá trị lambda tốt nhất
lasso_model = glmnet(x.train, y.train, alpha = 1, lambda = best_lambda)
plot(lasso_model)
cv.out = cv.glmnet (x.train, y.train, alpha = 1)
plot ( cv.out )

# Đánh giá mô hình trên tập kiểm tra
lasso_pred <- predict(lasso_model, newx = x[!train, ])

y_test <- y[!train]
mean((lasso_pred - y_test)^2)
out = glmnet (x , y , alpha = 1, lambda = best_lambda )
lasso.coef <- predict(out, type = "coefficients", s = best_lambda)
lasso.coef
lasso.coef[lasso.coef != 0]
