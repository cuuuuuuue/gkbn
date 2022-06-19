library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(igraph)

#导入数据
data <- diamonds
head(diamonds)
str(diamonds)
summary(diamonds)

#创建一个新的数据smaller,里面只包含carat小于3的钻石
smaller <- diamonds %>%
  filter(carat < 3)

#绘制第一个箱线图 不做任何处理的箱线图
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group=carat))

#绘制第二个箱线图 使用cut_number()近似显示每个箱线图的数据量，题目中的第一个图
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

#绘制第三个箱线图 使用cut_width() 将 x 变量分成宽度为 width 的分箱，题目中的第二个图
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
