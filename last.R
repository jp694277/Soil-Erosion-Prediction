
## Logistic GAM 模型

- 下圖為Plot of GAM
```{r GAM yield3 tillage, fig.align='center', fig.show='hold' ,echo=F, fig.width= 4,fig.height= 4,warning=FALSE, message = F}
library(gam);
gam.fit3 <- readRDS("RDS files/gam_fit.rds")
soil_train <- readRDS("RDS files/soil_train.rds")
plot.Gam(gam.fit3, terms = "s(yield3, df = 3)", se = T, col = "orange", lwd = 3)
plot.Gam(gam.fit3, terms = "tillage", se = T, col = "orange", lwd = 3)
```  

--- .class #id 

## Logistic GAM 模型

- Accuracy = 91.4%

```{r GAM ACC, echo=FALSE}
Accuracy <- c("0.912", "0.914", "0.914","0.914" , "0.914", "0.914")
data <- t(data.frame(Accuracy))
colnames(data) <- c("test1","test2","test3","test4","test5", "test_average")
kable(data) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  column_spec(7, bold = T, color = "grey", background = "skyblue")
```  

--- .class #id 

## Neural Network

- 下圖為Plot of Tune Model
- 第一隱藏層有一個節點，第二隱藏層為第二個節點

```{r neural tune plot, echo = F, fig.align = 'center',warning=FALSE, message = F}

nnt.tune <- read_rds("RDS files/nnt_tune_real.rds")

plot(nnt.tune)# no obvious results

nnt.tune$results %>% kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  row_spec(2, bold = T, color = "grey", background = "skyblue")
```  

--- .class #id 

## Neural Network

- 下圖為Plot of Neural Network

```{r nntmodel plot, echo = F, fig.align = 'center',warning=FALSE, message = F}

knitr::include_graphics("RDS files/neuralnetPlot.jpeg")
```  

--- .class #id 

## Neural Network

- Accuracy = 69.7%

```{r nn Acc, echo = F}
Accuracy <- c("0.697", "0.697", "0.697", "0.697", "0.697", "0.697")
data <- t(data.frame(Accuracy))
colnames(data) <- c("test1","test2","test3","test4","test5", "test_average")
kable(data) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  column_spec(7, bold = T, color = "grey", background = "skyblue")
```  

--- .class #id 

## Neural Network

- 預測不好的原因

```{r prednn show, echo=F, fig.align = 'center',warning=FALSE, message = F}

prednn_show <- read_rds("RDS files/prednn.rds")

head(prednn_show) %>% kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  row_spec(4, bold = T, color = "grey", background = "skyblue")
```  
