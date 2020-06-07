####################################################################
#################### DALAPICOLLA & PERCEQUILLO, 2020 ###############
####################################################################

####################################################################
############# VALE INSTITUTE OF TECHNOLOGY - ITV ###################
############### UNIVERSIDADE DE SÃO PAULO - USP ####################
####################################################################

#-------------------------------------------------------------------
#     Species Concepts And Taxonomic Practice In The Integrative   #
#      Taxonomy Era: An Example Using South American Rodents       #
#-------------------------------------------------------------------

## LOAD THE PACKAGES:
library(tidyverse)
library(ggrepel)
library(reshape2)


## LOAD THE csv:
data = read.csv("Dalapicolla_Percequillo_Data_Descriptions_Rodentia.csv")
data = tibble(data)


## DEFINE A THEME FOR GRAPHICS
theme_2020 = theme(legend.text = element_text(face = "italic",
                                             colour="black",
                                             family = "Helvetica",
                                             size = rel(1)), 
                  axis.title = element_text(colour="black",
                                            face = "bold",
                                            family = "Helvetica",
                                            size = rel(1.2)), 
                  axis.text = element_text(family = "Helvetica",
                                           colour = "black",
                                           angle = 90,
                                           hjust = 0.5,
                                           vjust = 0.5,
                                           size = rel(1)), 
                  axis.line = element_line(size = 1,colour = "black"), 
                  axis.ticks = element_line(colour="black",size = rel(1)),
                  
                  panel.grid.minor = element_blank(), 
                  panel.background = element_rect(fill = "whitesmoke"), 
                  panel.grid.major = element_line(colour="black",size = rel(0.2), linetype = "dotted"),
                  legend.key = element_blank(), 
                  legend.title = element_text(colour = "black",
                                              face = "bold",
                                              size = rel(1.5),
                                              hjust = 0.5,
                                              vjust = 0.5,
                                              family = "Helvetica"), 
                  plot.title = element_text(colour = "black",
                                            face = "bold",
                                            hjust = 0.5, #alingment
                                            size = rel(1.7),
                                            family = "Helvetica"))

#-------------------------------------------------------------------
## Graph 1 - Annual Descriptions by Family
#Selecting a data frame (df)
df1 = data %>% select(Year, Family)

#Graphic:
graph1 = ggplot(data=df1) +
  geom_bar(mapping = aes(x=factor(Year),
           y=(..count..),
           group = interaction(Year, Family),
           fill = Family), size=0.3, colour="black") +
  geom_text(stat='count', aes(x=factor(Year), y=(..count..), label=..count..), vjust=-0.5, fontface="bold") +
  scale_fill_manual (values = topo.colors(11)) +
  theme_2020 +
  theme(axis.text.y = element_text(angle = 0)) +
  labs(title = "Annual Descriptions by Family", x = "Year", y = "Frequency") 

#Save as pdf:
pdf("Species_by_Family.pdf", onefile = F)
plot.new()
graph1
dev.off()


#-------------------------------------------------------------------
## Graph 2 - Descriptions using Multiples Datasets
#Selecting a data frame (df)
df2 = data %>%
  select(Year, Morphological, Molecular, Cytogenetics, Ecological) %>%
  mutate(total_correct = rowSums(.[2:5])) %>%
  mutate(total_correct=replace(total_correct, total_correct!=0, 1)) %>%
  group_by(Year) %>% tally(total_correct)

df = data %>% count(factor(Year))
colnames(df) = c("Year", "n_total")

df2$total = df$n_total
df2
names(data)

df2 = df2 %>%
  group_by(Year)%>%
  mutate("pc"=round((n/total)*100,1))


#Graphic:
graph2 = ggplot(data=df2) +
  geom_point(mapping = aes(x=Year, y=pc), size= 2) +
  geom_line(mapping = aes(x=Year,y=pc)) +
  theme_2020 +
  theme(axis.text.y = element_text(angle = 0)) +
  geom_hline(yintercept=100, linetype="dashed", size=0.5) +
  labs(title = "Descriptions using \n Multiples Datasets", x = "Year", y = "Frequency (%)") +
  geom_label_repel(mapping = aes(x=Year,y=pc, label=round(pc,1)))+
  scale_y_continuous(breaks = c(seq(20, 100, by=20), 100)) +
  xlim(1999, 2020)

#Save as pdf:
pdf("Datasets_by_Description.pdf", onefile = F)
plot.new()
graph2
dev.off()


#-------------------------------------------------------------------
## Graph 3 - Datasets by Descriptions
#Selecting a data frame (df)
df3 = data %>%
  select(Year, Morphological, Molecular, Cytogenetics, Ecological) %>%
  mutate(Morphological=replace(Morphological, Morphological!=0, 1)) %>%
  mutate(Molecular=replace(Molecular, Molecular!=0, 1)) %>%
  mutate(Cytogenetics=replace(Cytogenetics, Cytogenetics!=0, 1)) %>%
  mutate(Ecological=replace(Ecological, Ecological!=0, 1))

#Grouping by year
morpho = df3 %>% group_by(Year) %>% tally(Morphological)
genetics = df3 %>% group_by(Year) %>% tally(Molecular)
n2 = df3 %>% group_by(Year) %>% tally(Cytogenetics)
eco = df3 %>% group_by(Year) %>% tally(Ecological)


# Creating a new data frame
df4 = df
colnames(df4) = c("Year", "Total")
df4$Morphological = morpho$n
df4$Molecular = genetics$n
df4$Cytogenetics = n2$n
df4$Ecological = eco$n
df4

#Melting data frame
df4_melted = melt(df4)

#Graphic:
graph3 = ggplot(data=df4_melted) +
  geom_bar(mapping = aes(x=Year, y = value, fill=variable), position = "dodge", stat = "identity", size=0.2, colour="black") +
  scale_fill_manual (values = c("grey", "red", "blue", "yellow", "green")) +
  theme_2020 + 
  labs(title = "Datasets used in Descriptions \n of Neotropical Rodents", x = "Year", y = "Frequency") +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Datasets"))

#Save as pdf:  
pdf("Datasets_by_Descriptions.pdf", onefile = F)
plot.new()
graph3
dev.off()


#-------------------------------------------------------------------
## Graph 4 - Species Concepts by Descriptions
#Selecting a data frame (df)
df5 = data %>%
  select(Year, Species.Concept) %>%
  count(Species.Concept)

#Graph:
graph4 = ggplot(data=df5, mapping = aes(x=factor(reorder(Species.Concept, -n)), y=n))+
  geom_bar(stat = "identity", size=0.2, colour="black") +
  theme_2020 + 
  labs(title = "Species Concepts used in \n Neotropical Rodents Descriptions", x = "Species Concepts", y = "Frequency") +
  scale_x_discrete(labels = c("NI","GSC","PSC", "BSC", "EvSC")) +
  ylim(0, 110) +
  geom_text(mapping = aes(x=factor(reorder(Species.Concept, -n)), y=n, label=n), vjust=-0.5, fontface="bold")

#Save as pdf:
pdf("SpeciesConcept_by_Description.pdf", onefile = F)
plot.new()
graph4
dev.off()

#-------------------------------------------------------------------
## Graph 5 - Species Concepts by Year
#Selecting a data frame (df)
df6 = data %>%
  select(Year, Species.Concept) %>%
  group_by(Year) %>%
  count(Species.Concept)

#Graphic:
graph5 =ggplot(data=df6) +
  geom_bar(mapping = aes(x=factor(Year), y=n, fill=Species.Concept), position = "fill", stat = "identity", size=0.2, colour="black") +
  theme_2020 + 
  labs(title = "Species Concepts by Year", x = "Year", y = "Frequency") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("yellow", "red", "blue", "gray", "brown"),
                    name="Species Concepts",
                    labels=c("BSC", "EvSC", "GSC", "NI", "PSC"))

#Save as pdf:  
pdf("SpeciesConcepts_by_Year.pdf", onefile = F)
plot.new()
graph5
dev.off()

  #END
