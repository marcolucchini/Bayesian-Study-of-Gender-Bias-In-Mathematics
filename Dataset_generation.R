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

# save(list = c("Quart", "Sise"), file = "data_bayes.Rdata")

# --------------------------------------------------
###       Exploratory analysis  
# --------------------------------------------------

load("data_bayes.Rdata")

df = Alumni

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
##              Corss dataset
# --------------------------------------------------

df1 = read.csv("Quart_2022.csv")
df1$GENERE = as.factor(df1$GENERE)
df1$NATURALESA = as.factor(df1$NATURALESA)
df1$HÀBITAT = as.factor(df1$HÀBITAT)
keep = c("ANY", "CODI_ALUMNE", "PCAT", "PCAST", "PANG", "PMAT", "GENERE", "MES_NAIXEMENT", "ANY_NAIXEMENT", "NATURALESA","HÀBITAT")
df1_1 = df1[-which(is.na(df1$CODI_ALUMNE)),keep]


df2 = read.csv("Sise_2022.csv")
df2$GENERE = as.factor(df2$GENERE)
df2$AREA_TERRITORIAL = as.factor(df2$AREA_TERRITORIAL)
df2$NATURALESA = as.factor(df2$NATURALESA)
df2$HÀBITAT = as.factor(df2$HÀBITAT)
keep = c("ANY", "CODI_ALUMNE", "PCAT", "PCAST", "PANG", "PMAT", "GENERE", "MES_NAIXEMENT", "ANY_NAIXEMENT", "AREA_TERRITORIAL", "NATURALESA", "HÀBITAT")         
df2_1 = df2[-which(is.na(df2$CODI_ALUMNE)),keep]

df6 = merge(df1_1, df2_1, by = "CODI_ALUMNE")  # x = 4 and y = 6

# Consistency checks
df6$GENERE.y = droplevels(df6$GENERE.y)
df6$HÀBITAT.x = droplevels(df6$HÀBITAT.x)
df6$NATURALESA.y = droplevels(df6$NATURALESA.y)
df6 = df6[which(df6$GENERE.x == df6$GENERE.y),]     # same gender
df6 = df6[which(df6$MES_NAIXEMENT.x == df6$MES_NAIXEMENT.y),]   # same month
df6 = df6[which(df6$ANY_NAIXEMENT.x == df6$ANY_NAIXEMENT.y),]   # same age
df6 = df6[which(df6$HÀBITAT.x == df6$HÀBITAT.y),]   # same city
levels(df6$NATURALESA.y) <- c("Privada","Privada", "Pública", "Pública") # fix levels
df6 = df6[which(df6$NATURALESA.x == df6$NATURALESA.y),]

summary(df6)

# Remove Na
df7 = na.omit(df6)

# Remove columns
keep7 = c("PCAT.x", "PCAST.x", "PANG.x", "PMAT.x", "GENERE.x", "NATURALESA.x", "HÀBITAT.x", "PCAT.y", "PCAST.y", "PANG.y", "PMAT.y", "AREA_TERRITORIAL")      
df8 = df7[,keep7]
df8$PLENG_4 = rowMeans(df8[,c(1,2)])
df8$PLENG_6 = rowMeans(df8[,c(8,9)])

# Remove last columns and change name
keep8 = c("PMAT.x", "GENERE.x", "NATURALESA.x", "HÀBITAT.x","AREA_TERRITORIAL", "PMAT.y", "PLENG_4", "PLENG_6", "PANG.x", "PANG.y")         
df9 = df8[,keep8]
colnames(df9) = c("PMAT_6", "GENERE", "NATURALESA", "HÀBITAT","AREA_TERRITORIAL", "PMAT_4", "PLENG_4", "PLENG_6", "PANG_4", "PANG_6")         

Alumni = df9

remove(list=c("df1", "df2", "df1_1", "df2_1", "df6", "df7", "df8", "df9", "keep", "keep7", "keep8"))

# save(list = c("Alumni"), file = "alumni.Rdata")







