# preliminaries
library(tidyverse)
theme_set(theme_minimal(16))
library(greta)
library(causact)

# getting the data
wawaDF = read_csv("https://raw.githubusercontent.com/flyaflya/buad621/master/data/wawa.csv",
                  col_types = c(col_integer(),
                                col_integer(),
                                col_integer())) %>%
  mutate(storeNum = 1)

## create multi-store data
set.seed(234)
allWawaDF = tibble(
  storeNum = 2:19,
  alpha = rnorm(length(storeNum),-2.7,0.4),
  beta = rt(n = length(storeNum),df = 2) * 0.2 - 0.55,
  sampSize = sample(100:600, length(storeNum)))

allWawaDF = crossing(allWawaDF,distanceKM = 2:10) %>%
  mutate(linPred = alpha + beta * distanceKM) %>%
  mutate(theta = 1 / (1+exp(-linPred))) %>%
  mutate(theta = ifelse(theta > 0.36, theta / 6, theta)) %>%
  rowwise() %>%
  mutate(offers = as.integer(sampSize / distanceKM + round(sample(x = 1:20, size = 1, replace = TRUE) / distanceKM, 0))) %>%
  mutate(offers = ifelse(storeNum == 6 | storeNum == 12, sample(x = 150:220, size = 1, replace = TRUE), offers)) %>%
  mutate(redemptions = rbinom(n = 1, size = offers, prob = theta)) %>%
  select(distanceKM,offers,redemptions,storeNum) %>%
  arrange(storeNum,distanceKM)

allWawaDF = bind_rows(wawaDF,allWawaDF) %>%
  select(storeNum,distanceKM,offers, redemptions)
grpPctDF = allWawaDF %>%
  mutate(storeNum = as_factor(storeNum)) %>%
  group_by(storeNum) %>%
  summarize(maxOff = 0.65 * max(offers),
            maxPct = max(redemptions / offers),
            offers = sum(offers),
            redemptions = sum(redemptions)) %>%
  mutate(pctRedeemed = redemptions/offers) %>%
  mutate(pctRedeemedLabel = paste0("Redemption Pct:\n",round(100*pctRedeemed,1),"%"))    %>%
  arrange(desc(pctRedeemed))

allWawaDF %>%
  mutate(storeNum = factor(allWawaDF$storeNum, levels = as.character(1:max(allWawaDF$storeNum)))) %>%
  ggplot(aes(x = distanceKM, #map time to x-axis
             y = offers, #map numTrials to y-axis
         )) + 
  geom_col(aes(fill = "offers"),width = 0.75) +  # use transparency
  geom_col(aes(y = redemptions, fill = "redemptions"),
           width = 0.75) + # different y-mapping
  facet_wrap(vars(storeNum), labeller = label_both,
             scales = "free_y") + 
  theme_minimal(11) + 
  theme(plot.caption = 
          element_text(size = 9, face = "italic"),
        legend.position = "bottom") +
  ylab("# of Offers") +
  labs(x = "x-axis: Customer Distance from Store in KM") +
  geom_text(data = grpPctDF, label = grpPctDF$pctRedeemedLabel,
            aes(x = 8, y = maxOff)) + theme(legend.position = c(0.9, 0.07), legend.title = element_blank(), axis.title.x = element_text(hjust = 1)) +
  scale_fill_manual(values = c("#7E9BB5","#FFC20A")) +
  scale_x_continuous(breaks = 2:10) 

allWawaDF %>%
  mutate(pctRedeemed = redemptions/offers) %>%
  mutate(pctRedeemedLabel = paste0(round(100*pctRedeemed,1),"%")) %>%
  ggplot(aes(x = distanceKM, #map time to x-axis
             y = pctRedeemed, #map numTrials to y-axis
  )) + 
  geom_col(width = 0.75) + 
  facet_wrap(vars(storeNum), labeller = label_both) + 
  theme_minimal(11) + 
  theme(plot.caption = 
          element_text(size = 9, face = "italic")) +
  ylab("Percentage of Offers Redeemed") +
  geom_text(aes(x = distanceKM, y = pctRedeemed + 0.02, label = pctRedeemedLabel), hjust = 0.5, vjust = 0.4, angle = 90, color = "black", fontface = "bold") +
  scale_x_continuous(breaks = 2:10)  +
  geom_text(data = grpPctDF, aes(label = paste0("Offers Made: ",offers),
            x = 6, y = 0.12))


grpPctDF %>%
  mutate(storeNum = fct_reorder(storeNum,pctRedeemed)) %>%
  ggplot(aes(y = storeNum, x = pctRedeemed)) +
  geom_point()

write_csv(allWawaDF,"allWawa.csv")

graph = dag_create() %>%
  dag_node("# Redeemed","k",
           rhs = binomial(n, theta),
           data = allWawaDF$redemptions) %>%
  dag_node("# of Offers","n",
           data = allWawaDF$offers,
           child = "k") %>%
  dag_node("Redemption Probability","theta",
           rhs = 1 / (1 + exp(-y)),
           child = "k") %>%
  dag_node("Linear Predictor","y",
           rhs = alpha + beta*x,
           child = "theta") %>%
  dag_node("Base Succ Prob Param","alpha",
           rhs = student(nu_alpha,mu_alpha,sd_alpha),
           child = "y") %>%
  dag_node("Distance Effect Param","beta",
           rhs = student(nu_beta,mu_beta,sd_beta),
           child = "y")  %>%
  dag_node("Outlier Measure for Base Success","nu_alpha",
           rhs = gamma(2,0.1),
           child = "alpha") %>%
  dag_node("Outlier Measure for Distance Effect","nu_beta",
           rhs = gamma(2,0.1),
           child = "beta")  %>%
  dag_node("Exp Succ Prob Param","mu_alpha",
           rhs = normal(-3,1),
           child = "alpha") %>%
  dag_node("Exp Distance Effect Param","mu_beta",
           rhs = normal(0,3),
           child = "beta") %>%
  dag_node("Inter-store Variation for Base Succ","sd_alpha",
           rhs = uniform(0,2),
           child = "alpha") %>%
  dag_node("Inter-store Variation for Dist Eff","sd_beta",
           rhs = uniform(0,1),
           child = "beta") %>%
  dag_node("DistanceKM","x",
           data = allWawaDF$distanceKM,
           child = "y") %>%
  dag_plate("Store Number","j",
            nodeLabels = c("alpha","beta"),
            data = allWawaDF$storeNum) %>%
  dag_node("Store Number","j",
           data = allWawaDF$storeNum,
           child = "y") %>%
  dag_plate("Observation","i",
            nodeLabels = c("j","k","n","theta","y","x"))
graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)

drawsDF = graph %>% dag_greta()

drawsDF %>% dagp_plot()

# get posterior for theta given a distance
postTheta = function(distance) {
  tempDF = drawsDF %>% 
    mutate(y = alpha + beta * distance) %>%
    mutate(theta = 1 / (1+exp(-y)))
  return(tempDF$theta)
}

#test it
postTheta(distance = 4)

# and then show as density plot
tibble(x = postTheta(4)) %>% # need df for ggplot
  ggplot() +
  geom_density(aes(x=x), fill = "purple", alpha = 0.5)

# another way to plot posterior that is easy and
# also a little more compact is to pass a representative sample to a stat_geom from ggdist
# install.packages("ggdist")
library("ggdist")
tibble(x = postTheta(4)) %>% # need df for ggplot
  ggplot(aes(x = x, y = 4)) +
  stat_pointinterval(.width = 0.9) + # cred interval
  labs(x = "Success Prob Theta",
       y = "Distance x")

## really we should plot distance on x-axis
## so switch around axes and add some limits
## tso plot can be added to.
tibble(x = postTheta(4)) %>% # need df for ggplot
  ggplot(aes(y = x, x = 4)) +
  stat_pointinterval(.width = 0.9) + # cred interval
  labs(y = "Success Prob Theta",
       x = "Distance x") +
  coord_cartesian(xlim = c(2,10)) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.035))

# use for loop to make data frame for more than
# just 4 kilometers distance - go from 2km to 10km
plotDF = tibble() ## initialize dataframe for plot
for (dist in seq(2,10,by=0.5)) {
  tempDF = tibble(distance = dist,
                  draw = postTheta(dist))
  plotDF = bind_rows(plotDF,tempDF)
}

## now grab plot from above and graph this
# expanded dataset
plotDF %>% # need df for ggplot
  ggplot(aes(y = draw, x = distance)) +
  stat_pointinterval(.width = 0.9) + # cred interval
  labs(y = "Success Prob Theta",
       x = "Distance x") +
  coord_cartesian(xlim = c(2,10)) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0.00,0.04)) +
  geom_point(data = wawaDF,
             aes(x = distanceKM, y = pctRedeemed),
             color = "purple",
             size = 4)

                     