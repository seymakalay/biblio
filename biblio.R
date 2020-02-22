
rm(list=ls())
library(devtools); library( dplyr ); library(stringr); library(tidyr)
library(RTextTools); library(data.table);library(xtable)
library(bibliometrix); library(ggplot2);library(kableExtra)
library(devtools);library(kableExtra)
#biblioshiny()
#A <- readFiles("savedrecs.bib")
#M <- convert2df(A, dbsource = "isi", format = "bibtex");  M
M1 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
attributes(M1)
M1$AU_UN

A <- readFiles("savedrecs.174.txt")
M <- convert2df(A, dbsource = "wos", format = "plaintext") 
write.csv(M, file = "M.csv") # go to excel and give a name the fist unnamed coloumn as "Paper"
results <- biblioAnalysis(M, sep = ";") 
plot(x = results, k = 10, pause = FALSE)

attributes(results)
attributes(M)

#M <- read.csv( "M.csv", header = TRUE, sep = ",")
##################

options(width=100)
#Choosing k=10 you decide to see the first 10 Authors, the first 10 sources, etc.
S <- summary(object = results, k = 41, pause = FALSE); attributes(S) #check the max K cause it depends on TCpc length
S

mi <- S$MainInformation; mi
miDF <- S$MainInformationDF; miDF
ap <- S$AnnualProduction; ap
agr <- S$AnnualGrowthRate; agr
mpa <- S$MostProdAuthors; mpa
mscp <- S$MostCitedPapers; mscp
mpc <- S$MostProdCountries; mpc
TCpc <- S$TCperCountries; TCpc
mrs <- S$MostRelSources; mrs
mrk <- S$MostRelKeywords; mrk

#make a table of most productive countries
mpc$Freq <- as.numeric(mpc$Freq)
mpc$Freq <-  round(mpc$Freq,2)
mpc$SCP <- as.numeric(mpc$SCP)
mpc$SCP_Ratio <- round(mpc$SCP/sum(mpc$SCP),2)
mpc$MCP_Ratio  <- as.numeric(mpc$MCP_Ratio )     
mpc$MCP_Ratio <- round(mpc$MCP_Ratio,2)
mpc <- mpc[,c(1,2,4,5,3,6,7)]; mpc
#make a table of most productive countries

print(xtable(mpc[c(1:10),], caption = 'Most Productive Countries'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
###################
S.cbind <- cbind(mpa, mscp, mpc, TCpc, mrs, mrk); S.cbind; names(S.cbind)


# using stringr combine 2 text without case sensitivity
savedrecs.174 <- read.csv(file = "savedrecs.174.csv", header = T, sep = ",")
savedrecs.174$Title <- str_to_title(savedrecs.174$Title)
M$TI <- str_to_title(M$TI)

M.savedrecs.174 <- merge(savedrecs.174, M, by.x = "Title", by.y = "TI")
write.csv(M.savedrecs.174, file = "M.savedrecs.174.csv")
str(M.savedrecs.174)
names(M.savedrecs.174)
#######
#It calculates local citations (LCS) of authors and documents of a bibliographic collection
lclcttion <- localCitations(M, fast.search = F, sep = ";"); lclcttion; attributes(lclcttion)



#COMBINE M.savedrecs.174 WITH S.CBIND lclcttion$Papers


M.savedrecs.174$Authors <- as.character(M.savedrecs.174$Authors); class(M.savedrecs.174$Authors)
M.savedrecs.174$SR <- as.character(M.savedrecs.174$SR); class(M.savedrecs.174$SR)
lclcttion$Papers$Paper <- as.character(lclcttion$Papers$Paper); class(lclcttion$Papers$Paper )
names(lclcttion$Papers)

lclcttion.M.savedrecs.174 <- merge(lclcttion$Papers, M.savedrecs.174, by.x = "Paper", by.y = "SR" )
write.csv(lclcttion.M.savedrecs.174, file = "lclcttion.M.savedrecs.174.csv")
#cobine M.savedrecs.174\ SR or sr full with lclcttion$Papers to goet local and global citations
str(lclcttion.M.savedrecs.174)



#IN SOME CELLS THERE ARE MORE THEN 1 AFFILIATION JUST TAKE THE FIRST ONE AND NAME IT AU11_UN
s <- lclcttion.M.savedrecs.174$AU1_UN 
s1 = strsplit(s, split=';'); s1
lclcttion.M.savedrecs.174$AU1_UN  <- (mycol = unlist(lapply(s1, function(x){x[1]})))
lclcttion.M.savedrecs.174$AU1_UN 
class(lclcttion.M.savedrecs.174$AU1_UN)  
 
my.affiliations  <- lclcttion.M.savedrecs.174 %>%
                    group_by(AU1_UN) %>%
                    mutate(Affiliation.Paper = n(),  #summarise()
                            sum.LCS = sum(LCS),
                            sum.GCS = sum(GCS)
                            ) %>%
                     arrange(desc(GCS)); my.affiliations 
names(my.affiliations)
my.affiliations$sum.GCS <- as.integer(my.affiliations$sum.GCS); my.affiliations$sum.LCS <- as.integer(my.affiliations$sum.LCS)
my.affiliations1 <- my.affiliations[,c("AU1_UN","Affiliation.Paper", "sum.LCS","sum.GCS") ]
print(xtable(my.affiliations1[c(1:10),], caption = 'First Author Affiliation'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")


my.affiliations2 <- my.affiliations[,c("AU_UN" )]
print(xtable(my.affiliations2[c(1:50),], caption = 'Affiliations of Authors'), scalebox='0.90',
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")



           
my.papers <- lclcttion.M.savedrecs.174 %>%
              group_by(AU1_UN)%>%
              dplyr::select(Paper, Title,Year, LCS, GCS, AU, AU_UN, Source.Title, Total.Citations ) %>% # ,Average.per.Year
              mutate( Affiliation.Paper = n(),
                      sum.LCS = sum(LCS),
                      sum.GCS = sum(GCS) ) %>%
              arrange(desc(GCS)); my.papers

my.title <- my.papers[,c("Title")]

print(xtable(my.title[c(1:50),], caption = 'Title of most citited papers'), scalebox='0.8',
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")


#COMBINE MY.PAPERS WITH FIRST AUTHOR H-INDEX
authors=gsub(","," ",names(results$Authors))
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H <- as.data.frame(indices$H)

my.papers$AU
#TAKE THE FIRST AUTHOR NAME TO MUCH IT WITH MY.PAPERS NAME THE NEW COLUMN AS Author AND MACTH WITH INDEX
s <- my.papers$AU
s1 = strsplit(s, split=';'); s1
my.papers$Author  <- (mycol = unlist(lapply(s1, function(x){x[1]})))
my.papers$Author 
class(my.papers$Author ) 

my.papers1 <- merge(indices$H, my.papers, by = "Author")%>%
              arrange(desc(GCS)); head(my.papers1,5)

names(my.papers1)
my.papers2 <- my.papers1[, c("AU", #"h_index","g_index","m_index",
                             "AU_UN" )]


#my.papers2$h_index <- as.integer(my.papers2$h_index); my.papers2$g_index <- as.integer(my.papers2$g_index); my.papers2$m_index <- as.integer(my.papers2$m_index)
print(xtable(my.papers2[c(1:50),], caption = 'Most Citited Papers'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")


my.papers3 <- my.papers1[, c("AU", "Title")]
print(xtable(my.papers3[c(1:50),], caption = 'Most Citited Papers Continue 1'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")



my.papers4 <- my.papers1[, c("AU",    "Year", "LCS",    #   "AU",    "Year",  "Paper" 
                             "GCS" #,"Average.per.Year", 
                             )] %>% # "Affiliation.Paper","sum.LCS","sum.GCS", "AU1_UN"
                           arrange(desc(GCS))
colnames(my.papers4) <- c("Author","Year", "LCS", 
                          "GCS"#"Citation/t", 
                           ) #"#Papers","sum.LCS","sum.GCS", "AU1_UN"
           
my.papers4$Year <- as.integer(my.papers4$Year)
my.papers4$LCS <- as.integer(my.papers4$LCS); my.papers4$GCS <- as.integer(my.papers4$GCS)
#my.papers4$Average.per.Year <- as.integer(my.papers4$Average.per.Year)
#my.papers4$`Citation/t` <- as.integer(my.papers4$`Citation/t`)
my.papers4$sum.LCS <- as.integer(my.papers4$sum.LCS); my.papers4$sum.GCS <- as.integer(my.papers4$sum.GCS)

print(xtable(my.papers4[c(1:50),], caption = 'Most Citited Papers Continue 2'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")

 

my.papers5 <- my.papers1[, c("AU1_UN","Affiliation.Paper", "sum.LCS", "sum.GCS")] 
colnames(my.papers5) <- c("1st AU", "1st Aff", "Tot.LCS", "Tot.GCS")
my.papers5$Tot.LCS <- as.integer(my.papers5$Tot.LCS); my.papers5$Tot.GCS <- as.integer(my.papers5$Tot.GCS )
print(xtable(my.papers5[c(1:50),], caption = 'Most Citited Papers Continue 3'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
                            






my.source.title <- lclcttion.M.savedrecs.174 %>%
  group_by(SO)%>%
  dplyr::select( LCS, GCS) %>% # ,Average.per.Year
  mutate( Affiliation.Paper = n(),
          sum.LCS = sum(LCS),
          sum.GCS = sum(GCS) ) %>%
  arrange(desc(GCS)); my.source.title 

my.source.title$LCS <- as.integer(my.source.title$LCS)
my.source.title$GCS <- as.integer( my.source.title$GCS )
my.source.title$Affiliation.Paper <-  as.integer(my.source.title$Affiliation.Paper)
my.source.title$sum.LCS <- as.integer(my.source.title$sum.LCS)
my.source.title$sum.GCS <- as.integer( my.source.title$sum.GCS )

colnames(my.source.title) <- c("Source Title","LCS" ,"GCS", "#Aff Papers", "Tot.LCS","TotGCS"  )
 
print(xtable(my.source.title [c(1:50),], caption = 'Title of most citited sources'), scalebox='0.8',
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")



head(S$MostCitedPapers,50)




#most 10 institutions with the number of artichles
results$Affiliations
#results$Countries
results$CountryCollaboration
results$TotalCitation




FirstAuthors <- head(results$FirstAuthors,10); FirstAuthors
FirstAffiliation <- head(results$FirstAffiliation,10); FirstAffiliation 
CO <- head(results$CO,10); CO
TotalCitation <- head(results$TotalCitation,10); TotalCitation
TCperYear <- head(results$TCperYear,10); TCperYear
Sources <- head(results$Sources,10); Sources


str(results$FirstAuthors)
str(results$FirstAffiliation)
str(results$CO)
########

results$FirstAuthors <- word(results$FirstAuthors, 1)
results$FirstAuthors
#combine first authors info with savedrecs.174 
cmb.results <- as.data.frame(cbind(results$FirstAuthors, results$FirstAffiliation, results$CO ))
str(cmb.results)
names(cmb.results);cmb.results
colnames(cmb.results) <- c("First.Author", "First.Author.Affiliation", "First.Author.Country")

#create First.Author  and match the first name with cmb.results 
library(stringr)
savedrecs.174 <- read.csv(file = "savedrecs.174.csv", header = T, sep = ",")
savedrecs.174$First.Author <- word(savedrecs.174$Authors, 1)
savedrecs.174$First.Author <- gsub(",","", savedrecs.174$First.Author)
savedrecs.174$First.Author

names(savedrecs.174)
savedrecs.174 <- savedrecs.174[, c("Title", "Authors", "Source.Title","Publication.Year", "Volume",
                                   "Beginning.Page", "Ending.Page","DOI","Total.Citations","Average.per.Year", "First.Author" )]

colnames(savedrecs.174) <- c("Title", "Authors", "Source.Title","Publication.Year", "Volume",
                             "BP", "EP","DOI","Total.Citations","Average.per.Year", "First.Author" )
head(savedrecs.174)
str(savedrecs.174)

#merge the datasets base on the First Author info
cmb.results.savedrecs.174 <- merge(savedrecs.174, cmb.results,  by = c("First.Author" ))

cmb.results.savedrecs.174 <- cmb.results.savedrecs.174[order(-cmb.results.savedrecs.174$Total.Citations),]
head(cmb.results.savedrecs.174)













#######################
Affiliations <- head(results$Affiliations,10); Affiliations 
Authors <- head(results$Authors,10); Authors
AuthorsFrac <- head(results$AuthorsFrac,10); AuthorsFrac
FirstAuthors <- head(results$FirstAuthors,10); FirstAuthors
nAUperPaper <- head(results$nAUperPaper,10); nAUperPaper 
nAuthors <- head(results$nAuthors,10); nAuthors
AuMultiAuthoredArt <- head(results$AuMultiAuthoredArt,10);AuMultiAuthoredArt
AuSingleAuthoredArt <- head(results$AuSingleAuthoredArt,10); AuSingleAuthoredArt
AuSingleAuthoredArt <- head(results$AuSingleAuthoredArt,10); AuSingleAuthoredArt
MostCitedPapers <- head(results$MostCitedPapers,10); MostCitedPapers
Years <- head(results$Years,10); Years 
FirstAffiliation <- head(results$FirstAffiliation,10); FirstAffiliation 
Affiliations <- head(results$Affiliations,10); Affiliations 
Aff_frac <- head(results$Aff_frac,10); Aff_frac
CO <- head(results$CO,10); CO
Countries <- head(results$Countries,10); Countries
CountryCollaboration <- head(results$CountryCollaboration,10); CountryCollaboration
TotalCitation <- head(results$TotalCitation,10); TotalCitation
TCperYear <- head(results$TCperYear,10); TCperYear
Sources <- head(results$Sources,10); Sources
DE <- head(results$DE,10); DE
ID <- head(results$ID,10); ID
Documents <- head(results$Documents,10); Documents
DB <- head(results$DB,10); DB






options(width=100)
#Choosing k=10 you decide to see the first 10 Authors, the first 10 sources, etc.
S <- summary(object = results, k = 41, pause = FALSE); attributes(S) #check the max K cause it depends on TCpc length
S


mi <- S$MainInformation; mi
miDF <- S$MainInformationDF; miDF
ap <- S$AnnualProduction; ap
agr <- S$AnnualGrowthRate; agr
mpa <- S$MostProdAuthors; mpa
mscp <- S$MostCitedPapers; mscp
mpc <- S$MostProdCountries; mpc
TCpc <- S$TCperCountries; TCpc
mrs <- S$MostRelSources; mrs
mrk <- S$MostRelKeywords; mrk

mpc

#make a table of most productive countries
mpc$Freq <- as.numeric(mpc$Freq)
mpc$Freq <-  round(mpc$Freq,2)
mpc$SCP <- round(as.numeric(mpc$SCP),2)
mpc$SCP_Ratio <- round(mpc$SCP/sum(mpc$SCP),2)
mpc$MCP_Ratio  <- as.numeric(mpc$MCP_Ratio )     
mpc$MCP_Ratio <- round(mpc$MCP_Ratio,2)
mpc <- mpc[,c(1,2,4,5,3,6,7)]; mpc
#make a table of most productive countries
print(xtable(mpc[c(1:10),], caption = 'Most Productive Countries'),
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
###################



a <-metaTagExtraction(M, Field = "CR_AU", sep = ";", aff.disamb = TRUE)

S




# Example 1: First Authors for each cited reference
data(scientometrics)
scientometrics <- metaTagExtraction(scientometrics, Field = "CR_AU", sep = ";"); attributes(scientometrics )
unlist(strsplit(scientometrics$CR_AU[1], ";"))

#Example 2: Source for each cited reference
scientometrics <- metaTagExtraction(scientometrics, Field = "CR_SO", sep = ";")
unlist(strsplit(scientometrics$CR_SO[1], ";"))

#Example 3: Affiliation country for co-author
scientometrics <- metaTagExtraction(scientometrics, Field = "AU_CO", sep = ";")
scientometrics$AU_CO[1:10]

#Field
#"CR_AU" First Author of each cited reference
#"CR_SO" Source of each cited reference
#"AU_CO" Country of affiliation for each co-author
#"AU1_CO" Country of affiliation for the first author
#"AU_UN" University of affiliation for each co-author and the corresponding author (AU1_UN)
#"SR" Short tag of the document (as used in reference lists)

#####################################################################
#####################################################################

#S.cbind <- cbind(mpa[,c(1,2,4)], mscp, mpd, TCpc, mrs, mrk)
S.cbind <- cbind(mpa, mscp, mpc, TCpc, mrs, mrk); S.cbind
#xtable(S.cbind)
write.csv(S.cbind, file = "S.cbind.csv")
write.csv(miDF, file = "miDF.csv")
##########
colnames(TCpc) <- c("Country", "Total Citations","Average Article Citations"); names(TCpc)
my.table.2 <- merge(mpc, TCpc, by = "Country");my.table.2 

#my.table.2$Articles <- as.integer(my.table.2$Articles)
#colnames(my.table.2) <- c( "Country","Articles","SCP","MCP","%Freq","%MCP","%SCP","Total Citations", "Citations/t"  )
#print(xtable(my.table.2[c(1:10),], caption = 'Most Productive Countries'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
my.table.2$`Average Article Citations` <- as.integer(my.table.2$`Average Article Citations`)
my.table.2$`Total Citations` <- as.integer(my.table.2$`Total Citations`)
my.table.2$SCP <- as.integer(my.table.2$SCP)
my.table.2 <- my.table.2[order(-my.table.2$`Total Citations`),]; my.table.2
colnames(my.table.2) <- c( "Country","Articles","SCP","MCP","%Freq","%MCP","%SCP","Total Citations", "Citations/t"  )

#"Most Citated Countries"
#THIS IS MY FIRST TABLE IN TP2
print(xtable(my.table.2[c(1:20),], caption = 'Most Citated Countries'),
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")

####################################################################
####################################################################

kable(mpa, "latex", caption = "MostProdAuthors", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

 kable(S.cbind) %>%
       kable_styling("striped") %>%
       add_header_above(c( "MostProdAuthors" = 4, "MostCitedPapers" = 3, "MostProdCountries" = 7, "TCperCountries" = 3,
                          "MostRelSources" =2, "MostRelKeywords" = 4 )) %>%
       save_kable(file = "S.cbind.html", self_contained = T)



#It calculates local citations (LCS) of authors and documents of a bibliographic collection
lclcttion <- localCitations(M, fast.search = F, sep = ";"); lclcttion; attributes(lclcttion)
head(lclcttion$Authors)
head(lclcttion$Papers) # here papers means author in savedrec174 excel file
head(lclcttion$M)

#colnames(lclcttion$Papers) <- c("Authors", "DOI", "Year", "LCS", "GCS")


savedrecs.174 <- read.csv(file = "savedrecs.174.csv", header = T, sep = ",")
names(savedrecs.174)

savedrecs.174 <- savedrecs.174[, c("Title", "Authors", "Source.Title","Publication.Year", "Volume",
                                   "Beginning.Page", "Ending.Page","DOI","Total.Citations","Average.per.Year")]
                  
colnames(savedrecs.174) <- c("Title", "Authors", "Source.Title","Publication.Year", "Volume",
                             "BP", "EP","DOI","Total.Citations","Average.per.Year")

names(lclcttion$M)[names(lclcttion$M) == "DI" ] <- "DOI" #CHANGE THE COLNAME IN lclcttion$M 
names(lclcttion$M)[names(lclcttion$M) == "AF" ] <- "Authors"





#combine if one of the by matches
#M.savedrecs.174 <- left_join(lclcttion$M, savedrecs.174, by = c("DOI")) #, "EP", "DOI" check this again  missing values in merge
#str(M.savedrecs.174)








#kable(lclcttion) %>%
#  kable_styling("striped") %>%
#  add_header_above(c(" " =1, "lclcttion.Authors" = 2, "lclcttion.Papers" = 5, "lclcttion.M" = 64 )) %>%
#  save_kable(file = "lclcttion.html", self_contained = T)







#To obtain the most frequent cited manuscripts:
CR <- citations(M, field = "article", sep = ";"); attributes(CR) #"Cited"  "Year"   "Source"
most.freq.cited.manus <- cbind(CR$Cited[1:50], CR$Year[1:50], CR$Source[1:50])

#To obtain the most frequent cited first authors:
CR <- citations(M, field = "author", sep = ";"); attributes(CR)
most.freq.cited.author <- cbind(CR$Cited[1:50], CR$Year[1:50], CR$Source[1:50])



authors.domaninance <- dominance(results, k = 10); authors.domaninance
authors <- gsub(","," ",names(results$Authors)[1:50])
indices <- Hindex(M, field = "author",elements = authors, sep = ";", years = 50); attributes(indices) 
indices$H ; indices$CitationList
df.Citation.list  <- ldply (indices$CitationList, data.frame)


topAU <- authorProdOverTime(M, k = 50, graph = TRUE); attributes(topAU)

topAU$dfAU #Author's productivity per year
topAU$dfPapersAU #1 Author may have more than 1 article
topAU$graph

# Author Productivity. Empirical Distribution
L <- lotka(results); attributes(L)
L$AuthorProd
# Beta coefficient estimate
L$Beta
# Constant
L$C
# Goodness of fit
L$R2
#https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html


A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:50]

###Following this approach, you can compute several bipartite networks:

#Citation network
A <- cocMatrix(M, Field = "CR", sep = ".  ")

#Author network
A <- cocMatrix(M, Field = "AU", sep = ";")

#Country network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")

#Author keyword network
A <- cocMatrix(M, Field = "DE", sep = ";")

#Keyword Plus network
A <- cocMatrix(M, Field = "ID", sep = ";")





#Authors' Coupling
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
net <- networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 50, 
                Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,
                remove.multiple=TRUE,labelsize=0.6,label.n=49,label.cex=F)



#Author Collaboration
M <- metaTagExtraction(M, Field = "AU", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
net <- networkPlot(NetMatrix, n = 100, Title = "Author Collaboration",  alpha =0.5, verbose = T, edges.min =0,
                   type = "fruchterman", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
# type = "fruchterman" ,"kamada", "auto","circle", "sphere","mds

#Country Collaboration
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", verbose = T,
                type = "fruchterman", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")



#co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", verbose = T, cluster = "louvain", edges.min =2,
                type = "auto",  size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)



# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", verbose = T,
                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)



netstat <- networkStat(NetMatrix)


# Create a country collaboration network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network

net <- networkPlot(NetMatrix, n = 50, Title = "Country Collaboration", 
                   type = "mds", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none"); net

