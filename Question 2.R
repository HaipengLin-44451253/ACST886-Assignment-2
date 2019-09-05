
## Question 2


x<-c("40–44","45–49","50–54","55–59","60–64","65–69","70–74","75–79","80–84","85–89","90–94")
x



Ex<-c(15518,19428,21594,21890,19174,15775,11414,6993,3276,1096,201)
Ex

dx<-c(65,144,219,378,465,557,685,644,471,217,67)
dx


Edx<-c(73.9,134.6,223.9,346.3,468.1,600.2,675.5,637.4,458.7,240.6,61.4)
Edx

qx<-Edx/Ex
qx

VDeaths<-Edx*(1-qx)
VDeaths

Zx<-(dx-Edx)/sqrt(Edx*(1-qx))
Zx
```

### Statistical Test

### Chi Square Test

chi_2<-sum(Zx^2)

qchisq(0.95,11)

### Indibidual Standardised Deviations Test

r<-c()
r[1]<-pnorm(-1)
r[2]<-pnorm(0)-pnorm(-1)
r[3]<-pnorm(1)-pnorm(0)
r[4]<-1-pnorm(1)
r


Er<-11*r
Er


Ar<-c()
Ar[1]<-sum(Zx<(-1))
Ar[2]<-sum((Zx>-1)*Zx<(0))
Ar[3]<-sum((Zx>0)*(Zx<(1)))
Ar[4]<-sum(Zx>1)
Ar

sum((Zx>(-2/3))*(Zx<(2/3)))

ISD<-sum((Ar-Er)^2/Er)
ISD

qchisq(0.95,3)

### Cumulative Deviation Test

CDT<-sum(dx-Edx)/sqrt(sum(Edx*(1-qx)))
CDT

CDT<-abs(CDT)
CDT

qnorm(0.975)
```

### Sign Test

sum((Zx>0))

pbinom(6,11,0.5)


2*pbinom(6,11,0.5)


### Grouping of Signs Test \frac{1}{k}

Zx
n1<-6;n2<-5
j<-0
repeat{j<-j+1
k<-c(1:j)
if((sum(factorial(n1-1)/factorial(k-1)/factorial(n1-k)*factorial(n2+1)/factorial(k)/factorial(n2+1-k)))/(factorial(n1+n2)/factorial(n1)/factorial(n2))>=0.05){print(j)
  break}
]
### Serial Correlations Test

zbar_1=Zx[1:10]
zbar_2=Zx[2:11]
SCT<-sum((zbar_1-mean(zbar_1))*(zbar_2-mean(zbar_2)))/sqrt(sum((zbar_1-mean(zbar_1))^2)*sum((zbar_2-mean(zbar_2))^2))*sqrt(10)
SCT

qnorm(0.95)
