# --------------------------------------------------
####            Dataset Quart
# --------------------------------------------------
df = read.csv("Quart_2022.csv")

df$GENERE = as.factor(df$GENERE)
df$AREA_TERRITORIAL = as.factor(df$AREA_TERRITORIAL)
df$NATURALESA = as.factor(df$NATURALESA)
df$HÀBITAT = as.factor(df$HÀBITAT)
summary(df)

# A lot of NA so remove some colomuns
remove = which(colSums(is.na(df))>600000)
df2 = df[,-remove]

# Remove all the lines remained with NA
df3 = na.omit(df2)

summary(df3)

# Simplify the columns
colnames(df3)
keep = c("ANY", "CODI_ALUMNE", "PCAT", "PCAST", "PANG", "PMAT", "PCIEN", "GENERE", "MES_NAIXEMENT", "ANY_NAIXEMENT", "NATURALESA","HÀBITAT")

df4 = df3[, keep]

# Fix levels 
df4$HÀBITAT = droplevels(df4$HÀBITAT)

summary(df4)

Quart = df4

remove(list = c("df", "df2", "df3", "df4", "keep", "remove"))



# --------------------------------------------------
###               DATASET Sisè
# --------------------------------------------------

df = read.csv("Sise_2022.csv")

summary(df)
df$GENERE = as.factor(df$GENERE)
df$NATURALESA = as.factor(df$NATURALESA)
df$HÀBITAT = as.factor(df$HÀBITAT)


colnames(df)

# Coherence with other dataset
keep = c("ANY", "CODI_ALUMNE", "PCAT", "PCAST", "PMAT", "PANG", "PMED", "GENERE", "MES_NAIXEMENT", "ANY_NAIXEMENT", "AREA_TERRITORIAL", "NATURALESA", "HÀBITAT")         
df2 = df[,keep]

# Remove all the lines remained with NA
df3 = na.omit(df2)

# Fix levels 
df3$NATURALESA = droplevels(df3$NATURALESA)
summary(df3)

Sise = df3

remove(list = c("df", "df2", "df3", "keep", "remove"))

save(list = c("Quart", "Sise"), file = "data_bayes.Rdata")

# --------------------------------------------------
###       Exploratory analysis  
# --------------------------------------------------

df = Sise

library(tidyverse)

# keep = c("PCAT",   "PCAST",  "PANG",   "PMAT",   "PCIEN",  "GENERE")
keep = c("PCAT",   "PCAST",  "PANG",   "PMAT",   "PMED",  "GENERE")
df2 = df[,keep] %>%   # select relevant columns 
  pivot_longer(c(1,2,3,4,5),names_to = 'Test')



library(ggplot2)
library(GGally)
ggplot(data = df2, aes(x=GENERE,y=value, fill=GENERE)) + 
  geom_boxplot()+
#  scale_fill_brewer(palette="Green") + 
#  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females in math?',
       y='Scores',x='') +
  facet_wrap(~Test,nrow = 1)


p_ <- GGally::print_if_interactive

# keep = c("PCAT",   "PCAST",  "PANG",   "PMAT",   "PCIEN",  "GENERE")
keep = c("PCAT",   "PCAST",  "PANG",   "PMAT",   "PMED",  "GENERE")
df_sampl <- df[sample(1:dim(df)[1], 1000), ]

# Different aesthetics for different plot sections and plot types
my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(..., mapping = ggplot2::aes(color = GENERE, alpha = 0.7), fill=NA) 
}

pm <- ggpairs(
  df_sampl[,],
  mapping = ggplot2::aes(color = GENERE, alpha = 0.8),
  diag = list(continuous = my_dens),
  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box_no_facet"),
  lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4)),
  title = "GGpairs of data"
)
p_(pm)




# --------------------------------------------------
## Corss dataset
# --------------------------------------------------

# df1 = read.csv("Quart_2022.csv")
df$GENERE = as.factor(df$GENERE)
df$AREA_TERRITORIAL = as.factor(df$AREA_TERRITORIAL)
df$NATURALESA = as.factor(df$NATURALESA)
df$HÀBITAT = as.factor(df$HÀBITAT)
keep = c("ANY", "CODI_ALUMNE", "PCAT", "PCAST", "PANG", "PMAT", "GENERE", "MES_NAIXEMENT", "ANY_NAIXEMENT", "NATURALESA","HÀBITAT")
df1_1 = df1[-which(is.na(df1$CODI_ALUMNE)),keep]


# df2 = read.csv("Sise_2022.csv")
df$GENERE = as.factor(df$GENERE)
df$NATURALESA = as.factor(df$NATURALESA)
df$HÀBITAT = as.factor(df$HÀBITAT)
keep = c("ANY", "CODI_ALUMNE", "PCAT", "PCAST", "PANG", "PMAT", "GENERE", "MES_NAIXEMENT", "ANY_NAIXEMENT", "AREA_TERRITORIAL", "NATURALESA", "HÀBITAT")         
df2_1 = df2[-which(is.na(df2$CODI_ALUMNE)),keep]

df6 = merge(df1_1, df2_1, by = "CODI_ALUMNE")

df6$GENERE.y = as.factor(df6$GENERE.y)
df6$GENERE.x = as.factor(df6$GENERE.x)
df6$GENERE.y = droplevels(df6$GENERE.y)
sum(df6$GENERE.x == df6$GENERE.y)


# Consistency checks
df6 = df6[which(df6$GENERE.x == df6$GENERE.y),]
df6 = df6[which(df6$MES_NAIXEMENT.x == df6$MES_NAIXEMENT.y),]
df6 = df6[which(df6$ANY_NAIXEMENT.x == df6$ANY_NAIXEMENT.y),]

sum(df6$HÀBITAT.x == df6$HÀBITAT.y)
sum(df6$NATURALESA.x == df6$NATURALESA.y)


summary(df6)

colSums(is.na(df6))

levels(as.factor(df6$AREA_TERRITORIAL))
