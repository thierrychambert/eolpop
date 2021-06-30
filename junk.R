out <- get_metrics(N)$scenario$impact_sc
TH <- dim(N)[2]
nsc <- dim(N)[3]

library(dplyr)

df <- as.data.frame(cbind(year = 1:nrow(out), out[,,1], scenario = 1))
for(j in 2:dim(out)[3]) df <- rbind(df, cbind(year = 1:nrow(out), out[,,j], scenario = j))
dim(df)
class(df)
names(df)

filter(df, scenario == 1)
filter(df, scenario > 1)


library(ggplot2)
library(plotly)

## pars
size = 1.5

# Plot lines
p <-
  ggplot(data = df, aes(x = year, y = avg)) +
  geom_line(data = filter(df, scenario > 1), size = size, aes(colour = factor(scenario))) +
  geom_line(data = filter(df, scenario == 1), size = size, colour = "black")

# Plot CIs
p <- p + geom_ribbon(data = filter(df, scenario > 1),
                     aes(ymin = uci, ymax = lci, fill = factor(scenario)), linetype = 0, alpha = 0.100)

# Add legend
Legend <- "parc"
nsc <- max(df$scenario)-1
p <- p + labs(x = "Annee", y = "Impact relatif",
              col = "Scenario", fill = "Scenario") +
  scale_color_hue(labels = paste(Legend, 1:nsc), aesthetics = c("colour", "fill"))
p









# Plot lines
p <-
  ggplot(data = df, aes(x = year, y = avg)) +
  geom_line(data = filter(df, scenario > 1), size = size, aes(colour = factor(scenario))) +
  geom_line(data = filter(df, scenario == 1), size = size, colour = "black")

# Plot CIs
p <- p + geom_ribbon(data = filter(df, scenario > 1),
                     aes(ymin = uci, ymax = lci, fill = factor(scenario)), linetype = 0, alpha = 0.100)

# Add legend
Legend <- "parc"
nsc <- max(df$scenario)-1
p <- p + labs(x = "Annee", y = "Impact relatif",
              col = "Scenario", fill = "Scenario") +
  scale_color_hue(labels = paste(Legend, 1:nsc)) +
  scale_fill_hue(labels = paste(Legend, 1:nsc))

p
ggplotly(p)

rm(fig)
fig <- ggplotly(p)
fig






#### Plotly
p <-
  ggplot(data = filter(df, scenario > 1), aes(x = year, y = avg)) +
  geom_line(size = size, aes(colour = factor(scenario)))


# Plot CIs
p <- p + geom_ribbon(aes(ymin = uci, ymax = lci, fill = factor(scenario)),
                     linetype = 0, alpha = 0.100)

# Add legend
Legend <- "parc"
nsc <- max(df$scenario)-1
p <- p + labs(x = "Annee", y = "Impact relatif",
              col = "Scenario", fill = "Scenario") +
  scale_color_hue(labels = paste(Legend, 1:nsc), aesthetics = c("colour", "fill"))
p

rm(fig)
fig <- ggplotly(p)
fig


fig <- ggplotly(p)
fig





#### Plotly

p <-
  ggplot(data = df, aes(x = year, y = avg)) +
  geom_line(size = size, aes(colour = factor(scenario)))
p

fig <- ggplotly(p, layerData = 1)
fig


##################


p <- ggplot(data = df, aes(x = year, y = avg, colour = factor(scenario)))
p <- p + geom_line(size = 2)
p

p <- p + geom_ribbon(aes(ymin = uci, ymax = lci, fill = factor(scenario)), linetype = 0, alpha = 0.075)
p


#########
p <- ggplot(data = df, aes(x = year, y = avg, colour = factor(scenario)))
p <- p + geom_line(size = 2)
p
