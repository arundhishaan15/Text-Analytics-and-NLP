mtcars
summary(mtcars)
colnames(mtcars)
rownames(mtcars)

mean(mtcars$hp)
mean(mtcars$mpg)
mean(mtcars$disp)
sd(mtcars$hp)
sd(mtcars$mpg)
sd(mtcars$disp)

my_func <- function(x){
  my_mean <- mean(x)
  my_std <- sd(x)
  return(c(my_mean, my_std))
}

my_func(x=mtcars$hp)
my_func(x=mtcars$mpg)
my_func(x=mtcars$disp)

table(mtcars$gear)

mean(mtcars$hp[which(mtcars$gear == 3)])

#Piping

mtcars %>%
  group_by(gear) %>%
  summarise(Avg_hp = mean(hp)) %>%
  arrange(desc(Avg_hp))
