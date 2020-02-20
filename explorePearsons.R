## The point of this script is to explore, at what point these two values become significant
## given the size of the sample b being 132 using equation 28.22 of Handbook of Parametric
## and Non-parametric statistical procedures
## Pair A (0.86752073 and 0.007852593)
## Pair B (-0.07682082 and -0.375814539)

# Clear workspace
rm(list=ls())

# Load libraries
library(dplyr)
library(ggplot2)

# Constants
# Pair 1
z1 = 0.86752073
z2 = 0.007852593
# Pair 2
z3 = -0.07682082
z4 = -0.375814539

# Create data frame
df = data.frame(x = (1:200))

x = seq(1, 200)
a = 1/(x-3)
b = 1/(132-3)
c = sqrt(a + b)
z = (z1-z2)/c

df = df %>% mutate(a = 1/(x-3), b=1/(132-3), c = sqrt(a+b), z = (z1-z2)/c)

plot(x, z)
# actual number -0.8233

z1 = -0.07682082
z2 = -0.375814539

a = seq(1, 200)
b = seq(1, 200)
x = seq(1, 200)

a = 1/(a-3)
b = 1/(132-3)

c = sqrt(a + b)
z = z1-z2/c

plot(x, z)
# actual number is 2.038591470

# Test run with the numbers flipped around
z1 = 0.86752073
z2 = 0.007852593
a = seq(1, 200)
b = seq(1, 200)
x = seq(1, 200)

a = 1/(45-3)
b = 1/(b-3)

c = sqrt(a + b)
z = z1-z2/c

plot(x, z)
# actual number -0.8233

z1 = -0.07682082
z2 = -0.375814539

a = seq(1, 200)
b = seq(1, 200)
x = seq(1, 200)

a = 1/(45-3)
b = 1/(b-3)

c = sqrt(a + b)
z = z1-z2/c

plot(x, z)


z1 = 0.86752073
z2 = 0.007852593
a = seq(1, 200)
b = seq(1, 200)
x = seq(1, 200)

a = 1/(a-3)
b = 1/(b-3)

c = sqrt(a + b)
z = z1-z2/c

plot(x, z)

z1 = -0.07682082
z2 = -0.375814539

a = seq(1, 200)
b = seq(1, 200)
x = seq(1, 200)

a = 1/(a-3)
b = 1/(b-3)

c = sqrt(a + b)
z = z1-z2/c

a = 1/(30-3)
b = 1/(30-3)
top = seq (-1, 0.99, 0.01) 
bot = sqrt(a+b)
result = top/bot
plot(x, result)

a = 1/(100-3)
b = 1/(100-3)
top = seq (-1, 0.99, 0.01) 
bot = sqrt(a+b)
result = top/bot
plot(x, result)
