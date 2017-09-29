# 1 - Check the structure of the titanic data set
str(titanic)

# 2 - Use ggplot() to create a dodged bar plot
ggplot(titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(position = "dodge")

# 3 - Add a facet_grid() layer to differentiate passengers who survived
ggplot(titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(position = "dodge") +
  facet_grid(.~Survived)

# 4 - Define an object for position_jitterdodge()
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Use the position object to create a scatterplot of the data
ggplot(titanic, aes(x = Pclass, y = Age, color = Sex)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) +
  facet_grid(.~Survived)