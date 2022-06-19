library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(igraph)

#导入数据
data <- mpg
head(mpg)
str(mpg)
names(mpg)
summary(mpg)

#选取出每类汽车中效率最高的型号并创建数据集best_in_class
best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

#绘制图形
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) + #在图形的标记中只对best_in_class打标签
  ggrepel::geom_label_repel( #使用 ggrepel对标签位置进行自动调整
    aes(label = model),
    data = best_in_class
  )

