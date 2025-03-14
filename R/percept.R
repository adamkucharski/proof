# Credit: https://github.com/zonination/perceptions

# Import files, load plot and data packages, fire up the number machine.
probly <- read.csv("data/C5_probly.csv", stringsAsFactors=FALSE)
library(tidyverse)
library(scales)

#Melt data into column format.
probly <- gather(probly, "variable", "value", 1:17)
probly$variable <- gsub("[.]"," ",probly$variable)
probly$value<-probly$value/100 # convert to %

#Order in the court!
probly$variable <- factor(probly$variable,
                          c("Chances Are Slight",
                            "Highly Unlikely",
                            "Almost No Chance",
                            "Little Chance",
                            "Probably Not",
                            "Unlikely",
                            "Improbable",
                            "We Doubt",
                            "About Even",
                            "Better Than Even",
                            "Probably",
                            "We Believe",
                            "Likely",
                            "Probable",
                            "Very Good Chance",
                            "Highly Likely",
                            "Almost Certainly"))


#Modify Theme:
source("R/ztheme.R")

#Plot probability data
ggplot(probly,aes(variable,value))+
  geom_jitter(aes(color=variable),size=2,alpha=.5,width=0.1)+
  scale_color_grey(start = 0.2, end = 0.8) +  # Grayscale for points
  scale_y_continuous(breaks=seq(0,1,.1), labels=scales::percent)+
  guides("none")+
  labs(title="",
       x="Phrase",
       y="Assigned Probability")+
  coord_flip()+
  z_theme()
ggsave("plots/C5_estimative_probability.pdf", height = 8, width = 8)
