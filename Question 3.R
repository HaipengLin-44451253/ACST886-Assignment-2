## Question 3
### Tutorial 5.7
Firstly, we import the data needed.
Age $x$
  ```{r}
x<-c(70:75)
x
```
Central Exposed to Risk $E^C_x$
  ```{r}
ECx<-c(1000,1005,1010,1008,1006,998)
ECx
```
Observed Number of Death $d_x$
  ```{r}
dx<-c(80,90,95,105,115,125)
dx
```
Crude $\mu_x$
  ```{r}
mu_x<-dx/ECx
mu_x
```
In order to obtain the graduated estimates of force of mortality $\dot{\mu}$ for the above data, we need to use the poisson model to find its maximum likelihood estimate. Since the age label of the data is $\prime$aged $x$ nearest birthday$\prime$, the $\dot\mu$-estimate applies to age $x$.  $$P_r(D_x=d_x)=\frac{e^{-E^C_x\mu_x}(E^C_x\mu_x)^{d_x}}{d_x!}$$
  Its log likelihood functioin is $$l(\mu)=-\sum_{x}E^C_x\mu_x+\sum_{x}d_xln(E^C_x\mu_x)$$
  According to Gompertz$\prime$ Law, $\mu_x=Bc^x$.
$$l(\mu)=-\sum_{x}E^C_xBc^x+\sum_{x}d_xln(E^C_xBc^x)$$
  Then $$\frac{\partial{l(\mu)}}{\partial{B}}=-\sum_{x}E^C_xc^x+\sum_{x}\frac{d_x}{B}$$
  $$\frac{\partial{l(\mu)}}{\partial{c}}=-\sum_{x}E^C_xBxc^{x-1}+\sum_{x}d_x\frac{x}{c}$$
  Let these two equations be 0, and then we can get the MLE of parameter $B$ and $c$, which can be calculated by the R function for non-linear minimisation function, $nlm()$.
In order to achieve better estimates of $B$ and $c$, we can construct a linear regression model to find the initial starting point.$$ln(\hat{\mu_x})=lnB+xlnc$$
  We can use the crude $\hat{\mu_x}$ to help construct the linear regression model.
```{r}
y<-log(mu_x)
y
```

```{r}
mod_1<-lm(y~x)
mod_1
```
Therefore, $lnB$ and $lnc$ are
```{r}
lnB<-mod_1$coefficients[1]
lnB
```
```{r}
lnc<-mod_1$coefficients[2]
lnc
```
The initial points of $B$ and $c$ are
```{r}
B_in<-exp(lnB)
B_in
```
```{r}
c_in<-exp(lnc)
c_in
```
We can use these two initial values and $nlm()$ functioin to find the exact value of $B$ and $c$.
```{r}
f_1<-function(p){abs(sum(-ECx*p[1]*p[2]^x+dx*log(ECx*p[1]*p[2]^x,base=exp(1))))}
iv<-c(B_in,c_in)
para<-nlm(f_1,iv)
para
```
The MLE of $B$ and $c$ are
```{r}
B_e<-para$estimate[1]
B_e
```

```{r}
c_e<-para$estimate[2]
c_e
```
The graduated $\mu_x$ are
```{r}
mu_grad<-B_e*c_e^x
mu_grad
```

### Tutorial 5.8
Firstly, we import the data needed.
Age $x$
  ```{r}
x<-c(30:49)
x
```
Initial Exposed to Risk $E_x$
  ```{r}
Ex<-c(70000,66672,68375,65420,61779,66091,68514,69560,65000,66279,67300,65368,65391,62917,66537,62302,62145,63856,61097,61110)
Ex
```
Observed Number of Death $d_x$
  ```{r}
dx<-c(39,43,34,31,23,50,48,43,48,47,62,63,84,86,120,121,122,162,151,184)
dx
```
Crude $q_x$
  ```{r}
q_x<-dx/Ex
q_x
```
In order to obtain the graduated estimates of mortality $\dot{q}$ for the above data, we use the model provided and weighted least squares. Since the age label of the data is $\prime$aged $x$ last birthday$\prime$, the $\dot\q$-estimate applies to age $x$. The weighted least squares is $$\sum_xW_x(\hat{q_x}-\dot{q_x})^2$$
  $$W_x=E_x$$.

The provided model is $$ln(\frac{\dot{q_x}}{1-\dot{q_X}})=a+bx$$.

Therefore, in order to achieve the better graduated estimates, we can use crude $\hat{q_x}$ and the above model to find the initial values of $a$ and $b$ for $nlm()$ function.
$$ln(\frac{\hat{q_x}}{1-\hat{q_X}})=a+bx$$
  ```{r}
y<-log(q_x/(1-q_x))
y
```

```{r}
mod_2<-lm(y~x)
mod_2
```
Therefore, $a$ and $b$ are
```{r}
a_in<-mod_2$coefficients[1]
a_in
```
```{r}
b_in<-mod_2$coefficients[2]
b_in
```
We can use these two initial values and $nlm()$ functioin to find the exact value of $a$ and $b$.
```{r}
f_2<-function(p){abs(sum(Ex*(log(q_x/(1-q_x))-(p[1]+p[2]*x))^2))}
iv<-c(a_in,b_in)
para_2<-nlm(f_2,iv)
para_2
```
The MLE of $a$ and $b$ are
```{r}
a_e<-para_2$estimate[1]
a_e
```

```{r}
b_e<-para_2$estimate[2]
b_e
```
$$ln(\frac{\dot{q_x}}{1-\dot{q_X}})=\hat{a}+\hat{b}x$$
  The graduated $\dot{q}_x$ are
```{r}
qx_grad<-1-1/(1+exp(a_e+b_e*x))
qx_grad
```

### Tutorial 5.9
Firstly, we import the data needed.
Age $x$
  ```{r}
x<-c(47:67)
x
```
Initial Exposed to Risk $E_x$
  ```{r}
Ex<-c(166,187,218,243,276,302,347,390,430,494,558,628,701,813,917,1040,1182,1299,1432,1596,1752)
Ex
```
Observed Number of Death $d_x$
  ```{r}
dx<-c(2,2,4,6,2,4,7,3,9,9,8,11,14,18,18,24,30,43,41,54,64)
dx
```
Standard Table Figure $q^S_x$
  ```{r}
qsx<-c(0.00505,0.00570,0.00644,0.00728,0.00826,0.00930,0.01051,0.01184,0.01331,0.01492,0.01668,0.01859,0.02065,0.02287,0.02525,0.02778,0.03049,0.03339,0.03648,0.03978,0.04332)
qsx
```
Crude $q_x$
  ```{r}
q_x<-dx/Ex
q_x
```
In order to obtain the graduated estimates of mortality $\dot{q}$ for the above data, we use the model provided and weighted least squares. Since the age label of the data is $\prime$aged $x$ last birthday$\prime$, the $\dot\q$-estimate applies to age $x$. The weighted least squares is $$\sum_xW_x(\hat{q_x}-\dot{q_x})^2$$
  $$W_x=E_x$$.

The provided model is $$\dot{q_x}=a+bq^S_x$$.

Therefore, in order to achieve the better graduated estimates, we can use crude $\hat{q_x}$ and the above model to find the initial values of $a$ and $b$ for $nlm()$ function.
$$\hat{q_x}=a+bq^S_x$$
  
  ```{r}
y<-q_x
y
```

```{r}
mod_3<-lm(y~qsx)
mod_3
```
Therefore, $a$ and $b$ are
```{r}
a_in<-mod_3$coefficients[1]
a_in
```
```{r}
b_in<-mod_3$coefficients[2]
b_in
```
We can use these two initial values and $nlm()$ functioin to find the exact value of $a$ and $b$.
```{r}
f_2<-function(p){abs(sum(Ex*(q_x-(p[1]+p[2]*qsx))^2))}
iv<-c(a_in,b_in)
para_3<-nlm(f_2,iv)
para_3
```
The MLE of $a$ and $b$ are
```{r}
a_e<-para_3$estimate[1]
a_e
```

```{r}
b_e<-para_3$estimate[2]
b_e
```
$$\dot{q_x}=a+bq^S_x$$.
The graduated $\dot{q}_x$ are
```{r}
qx_grad<-a_e+b_e*qsx
qx_grad

```

