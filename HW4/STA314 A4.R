#Q1 (iii)
p <- 1:100/100 #grid search sequence 0.01,0.02,...,1
log_likelihood <- 3*log(p) +(5-3)*log(1-p) #3 successes
plot(p, log_likelihood, type = "l", main = "log_likelihood")
p_hat<-p[which.max(log_likelihood)]
p_hat
abline(v = p_hat, col= "red")


#Q2 
#(ii)
ll <- function(beta, x, y){
  beta0 <- beta[1]
  beta1 <- beta[2]
  sum(y*(beta0+beta1*x))-sum(log(1+exp(beta0+beta1*x)))
}

#(iii)
library(ISLR)
fit1 <- glm(default ~ balance, family = "binomial", data = Default)
summary(fit1)

###(iv)
default <- ifelse(Default$default == "Yes", 1, 0) #set "Yes"=1, "No"=0
res <- optim(c(0, 0), ll, x = Default$balance, y = default, control = list(fnscale = -1), hessian = TRUE)

###(v)
res$par
# same as part (ii)

###(vi)
sqrt(diag(solve(res$hessian)))


###(vii) different se



# Question3

set.seed(100)

#(i)
fit2 <- glm(default ~ income + balance, family = "binomial", data = Default)
summary(fit2)

#(ii)
boot.fn <- function(data = Default, index){
  coef(glm(default ~ income + balance, family = "binomial", data = data, subset = index))
}

#(iii)
library(boot)
boot(Default, boot.fn, 100) # can be more than 100

#(iv) very close

# se(beto0 hat): 4.348e-01
# se(beta1 hat): 4.985e-06
# se(beta2 hat): 2.274e-04


# 4.639390e-01
# 4.530553e-06
# 2.398827e-04




