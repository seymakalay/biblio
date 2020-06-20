
rm(list=ls())
library(devtools); library( dplyr ); library(stringr); library(tidyr);library(plyr)
library(RTextTools); library(data.table);library(xtable);library(ggrepel)
library(bibliometrix); library(ggplot2);library(kableExtra)
library(devtools);library(kableExtra)
#biblioshiny()
#A <- readFiles("savedrecs.bib")
#M <- convert2df(A, dbsource = "isi", format = "bibtex");  M
M1 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
attributes(M1)
M1$AU_UN
M1$AU_CO
write.csv(M1, file = "M1.csv")


#A <- readFiles("savedrecs (7).txt")
A <- readFiles("savedrecs.174.txt")
M <- convert2df(A, dbsource = "wos", format = "plaintext") 
write.csv(M, file = "M.csv") # go to excel and give a name the fist unnamed coloumn as "Paper"
results <- biblioAnalysis(M, sep = ";") 
plot(x = results, k = 10, pause = FALSE)

attributes(results)
#"Articles"             "Authors"              "AuthorsFrac"          "FirstAuthors"         "nAUperPaper"          "Appearances"          "nAuthors"            
#"AuMultiAuthoredArt"   "AuSingleAuthoredArt"  "MostCitedPapers"      "Years"                "FirstAffiliation"     "Affiliations"         "Aff_frac"            
#"CO"                   "Countries"            "CountryCollaboration" "TotalCitation"        "TCperYear"            "Sources"              "DE" = Most Relevant Keywords                 
#"ID"                   "Documents"            "DB"       
attributes(M)
#"PT"       "AU"       "AF"       "TI"       "SO"       "LA"       "DT"       "DE"       "ID"       "AB"       "C1"       "RP"       "EM"       "FU"       "FX"      
#"CR"       "NR"       "TC"       "Z9"       "U1"       "U2"       "PU"       "PI"       "PA"       "SN"       "EI"       "J9"       "JI"       "DI"       "EA"      
#"PG"       "WC"       "SC"       "GA"       "UT"       "DA"       "ER"       "PD"       "PY"       "VL"       "IS"       "BP"       "EP"       "OA"       "AR"      
#"OI"       "RI"       "SI"       "CT"       "CY"       "CL"       "SP"       "SU"       "BE"       "SE"       "BN"       "PN"       "GP"       "HO"       "DB"      
#"AU_UN"    "AU1_UN"   "AU_UN_NR" "SR_FULL"  "SR"  

#MAKE A DATASET WITH M AND RESULTS
results$FirstAuthors
results$nAUperPaper
results$MostCitedPapers
results$Years
results$FirstAffiliation
results$CO #CONTURY OF THE FIRST AUTHOR
results$TotalCitation 


results.tbl <- cbind(results$FirstAuthors, results$nAUperPaper, results$Years, results$FirstAffiliation,results$CO, results$TotalCitation, results$MostCitedPapers)
View(results.tbl)
#results$TCperYear DO NOT USE THIS USE FROM SAVEDRECS
#
results$Affiliations #top 10 affiliation
results$Aff_frac
results$Sources

results$Countries
results$CountryCollaboration


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

#'Most Productive Countries'
#print(xtable(mpc[c(1:10),], caption = 'Most Productive Countries'), caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
###################
S.cbind <- cbind(mpa, mscp, mpc, TCpc, mrs, mrk); S.cbind; names(S.cbind)
names(S.cbind) <- gsub(" ", "", names(S.cbind)); names(S.cbind) # remove extra spaces
write.csv(S.cbind, file = "S.cbind.csv")

# using stringr combine 2 text without case sensitivity
savedrecs.174 <- read.csv(file = "savedrecs.174.csv", header = T, sep = ",")
savedrecs.174$Title <- str_to_title(savedrecs.174$Title); head(savedrecs.174$Title)
M$TI <- str_to_title(M$TI); head(M$TI)
M$Title <- M$TI
M.savedrecs.174 <- merge(savedrecs.174, M, by = "Title")
write.csv(M.savedrecs.174, file = "M.savedrecs.174.csv")
str(M.savedrecs.174)
names(M.savedrecs.174)

#######
#It calculates local citations (LCS) of authors and documents of a bibliographic collection
lclcttion <- localCitations(M, fast.search = F, sep = ";"); lclcttion; attributes(lclcttion)

head(lclcttion$Authors)
head(lclcttion$Papers)
head(lclcttion$M)
names(lclcttion$M)
View(lclcttion$M)

#COMBINE M.savedrecs.174$SR WITH lclcttion$Papers$Paper
#M.savedrecs.174$Authors <- as.character(M.savedrecs.174$Authors); class(M.savedrecs.174$Authors)
M.savedrecs.174$SR <- as.character(M.savedrecs.174$SR); class(M.savedrecs.174$SR);
lclcttion$Papers$Paper <- as.character(lclcttion$Papers$Paper); class(lclcttion$Papers$Paper )
names(M.savedrecs.174)
names(lclcttion$Paper)
head(M.savedrecs.174$SR)
head(lclcttion$Papers$Paper)

#USE RIGHT JOIN TO KEEP ALL THE VALUES IN M.savedrecs.174
#CONSIDER TC AS GCS SAME THINGS
lclcttion$Papers <- lclcttion$Papers[, c("Paper","Year","LCS","GCS")]; names(lclcttion$Papers) #131 obs shows only the papers where localcitation avaliable
M.savedrecs.174$Paper <- M.savedrecs.174$SR #all dataset

lclcttion.M.savedrecs.174 <- right_join(lclcttion$Papers, M.savedrecs.174, by = "Paper" )
write.csv(lclcttion.M.savedrecs.174, file = "lclcttion.M.savedrecs.174.csv")
str(lclcttion.M.savedrecs.174)
############
#
prop.table(table(lclcttion.M.savedrecs.174$WC))
prop.table(table(lclcttion.M.savedrecs.174$SC))
#
lclcttion.M.savedrecs.174$LCS[(is.na(lclcttion.M.savedrecs.174$LCS))] <- 0
lclcttion.M.savedrecs.174$GCS[(is.na(lclcttion.M.savedrecs.174$GCS))] <- 0 #this is the limitation we dont have the values if the articles are cited elsewhere
#
library(stringr)
names(lclcttion.M.savedrecs.174)
#lclcttion.M.savedrecs.174$Paper.splt <- lclcttion.M.savedrecs.174$Paper
#lclcttion.M.savedrecs.174$Year <- str_split_fixed(lclcttion.M.savedrecs.174$Paper.splt, ",", 3)[,2] #split the column and take the second element of the matrix
#
lclcttion.M.savedrecs.174$Year <- as.numeric( lclcttion.M.savedrecs.174$Publication.Year)
lclcttion.M.savedrecs.174$today <- 2020
lclcttion.M.savedrecs.174$tdy_year <-  lclcttion.M.savedrecs.174$today - lclcttion.M.savedrecs.174$Year + 1
head(lclcttion.M.savedrecs.174$tdy_year)
head(lclcttion.M.savedrecs.174$Year)
#
lclcttion.M.savedrecs.174$`LCS/t` <- round(lclcttion.M.savedrecs.174$LCS/lclcttion.M.savedrecs.174$tdy_year ,2)
lclcttion.M.savedrecs.174$`GCS/t` <- round(lclcttion.M.savedrecs.174$GCS/lclcttion.M.savedrecs.174$tdy_year ,2)
lclcttion.M.savedrecs.174$`Average Article Citations` <- round(lclcttion.M.savedrecs.174$GCS/lclcttion.M.savedrecs.174$tdy_year ,2)
###############################################################################
###############################################################################
#Most Citated Countries
names(TCpc); names(mpc)
colnames(TCpc) <- c("Country", "Total Citations","Average Article Citations") 
head(mpc); head(TCpc)
mst.ctd.CO <- merge(mpc, TCpc, by = "Country"); head(mst.ctd.CO)
#
#mst.ctd.CO$`Average Article Citations` <- round(as.numeric(mst.ctd.CO$`Average Article Citations`),2)
mst.ctd.CO$`Total Citations` <- as.integer(mst.ctd.CO$`Total Citations`)
mst.ctd.CO$SCP <- as.integer(mst.ctd.CO$SCP)
mst.ctd.CO$MCP <- as.integer(mst.ctd.CO$MCP)
mst.ctd.CO$Articles <- as.integer(mst.ctd.CO$Articles)
#
mst.ctd.CO <- mst.ctd.CO[order(-mst.ctd.CO$Articles),]; mst.ctd.CO
#mst.ctd.CO <- mst.ctd.CO[order(-mst.ctd.CO$`Average Article Citations`),]; mst.ctd.CO
colnames(mst.ctd.CO) <- c( "Country","Articles","SCP","MCP","%Freq","%MCP","%SCP","TGC", "Average Article Citatation"  )
#
#"Most Citated Countries"
print(xtable(mst.ctd.CO[c(1:20),], caption = 'Most Citated Countries',label="tab:Most-Citated-Countries"),
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")

#
mst.ctd.CO <- mst.ctd.CO[order(-mst.ctd.CO$`Average Article Citatation`),]; mst.ctd.CO
print(xtable(mst.ctd.CO[c(1:20),], caption = 'Most Citated Countries',label="tab:Most-Citated-Countries"),
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")

#
###############################################################################
###############################################################################
#Most Citited Papers 
names(lclcttion.M.savedrecs.174)
lclcttion.M.savedrecs.174$Year <- as.integer(lclcttion.M.savedrecs.174$Year)
mst.ctd.PA <- lclcttion.M.savedrecs.174[, c("Paper","Year","LCS","GCS", "Average.per.Year")]

mst.ctd.PA <- mst.ctd.PA %>%
        mutate(today = 2020,
               tdy_year = (today-mst.ctd.PA$Year+1),
               `LCS/t` = round(LCS/tdy_year ,2)) %>%
        #arrange(desc(Average.per.Year)) ; head(mst.ctd.PA,20)
       arrange(desc(GCS)) ; head(mst.ctd.PA,20)
names(mst.ctd.PA)
mst.ctd.PA <- mst.ctd.PA[, c("Paper","Year","LCS","GCS","LCS/t","Average.per.Year")]             
colnames(mst.ctd.PA) <- c("Paper","Year","TLC","TGC","TLC/t", "TGC/t")    
mst.ctd.PA$TGC <- as.integer(mst.ctd.PA$TGC)
mst.ctd.PA$TLC <- as.integer(mst.ctd.PA$TLC)
head(mst.ctd.PA,20)

#Most Citited Papers 
print(xtable(mst.ctd.PA[c(1:30),], caption = 'Most Citited Papers',label = "tab:Most-Citited-Papers"),
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")

###############################################################################
###############################################################################
#Most Citited Journals
lclcttion.M.savedrecs.174$`LCS/t` <- round(lclcttion.M.savedrecs.174$LCS/lclcttion.M.savedrecs.174$tdy_year ,2)
mst.ctd.JOR <- lclcttion.M.savedrecs.174 %>%
  dplyr::group_by(J9)%>%
  dplyr::mutate(`LCS/t` = round(LCS/tdy_year ,2),
                 Affiliation.Paper = n(),
                 sum.LCS.t = sum(`LCS/t`),
                 sum.GCS.t = sum(Average.per.Year),
                 sum.LCS = sum(LCS),
                 sum.GCS = sum(GCS))%>%
   #dplyr::arrange(desc(Affiliation.Paper))
    dplyr::arrange(desc(sum.GCS.t ))


names(mst.ctd.JOR)
mst.ctd.JOR <- mst.ctd.JOR[, c("J9","Affiliation.Paper","sum.LCS","sum.GCS","sum.LCS.t","sum.GCS.t")]
colnames(mst.ctd.JOR) <-     c("Journal","# of Papers", "TLC","TGC", "TLC/t", "TGC/t" )
mst.ctd.JOR$TLC <- as.integer(mst.ctd.JOR$TLC)
mst.ctd.JOR$TGC <- as.integer(mst.ctd.JOR$TGC)
  
print(xtable(unique(mst.ctd.JOR[c(1:100),]), caption = 'Most Citited Journals',label = "tab:Most-Citited-Journals"),
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
###############################################################################
###############################################################################
#Most Citited Affiliations

mst.ctd.AFF <- lclcttion.M.savedrecs.174  %>%
               dplyr::mutate(row = dplyr::row_number()) %>%
               tidyr::separate_rows(AU_UN, sep = ";") %>%
               distinct() %>%
               dplyr::group_by(AU_UN) %>%
               dplyr::summarise(sum.papers = dplyr::n_distinct(row), 
                                sum.GCS = sum(GCS), 
                                sum.LCS = sum(LCS),
                                sum.LCS.t = sum(`LCS/t`),
                                sum.GCS.t = sum(Average.per.Year)
                                 ) %>%
             #dplyr::arrange(desc(sum.papers)); mst.ctd.AFF
             dplyr::arrange(desc(sum.GCS.t)) #this comes  ""  2   28 cause there are 2 ; at the end AU_UN indicate no uni ; was an excedent

#NAs in the first Row
mst.ctd.AFF <- mst.ctd.AFF[-1,];mst.ctd.AFF
colnames(mst.ctd.AFF) <- c("Affiliation", "# of Papers", "TGC", "TLC","TLC/t", "TGC/t")
mst.ctd.AFF$TGC <- as.integer(mst.ctd.AFF$TGC)
mst.ctd.AFF$TLC <- as.integer(mst.ctd.AFF$TLC)

print(xtable(mst.ctd.AFF[c(1:21),], caption = 'Most Citited Affiliations',label = "tab:Most-Citited-Affiliations"), scalebox='0.90',
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
###############################################################################
###############################################################################

#making a table
names(lclcttion.M.savedrecs.174)
lclcttion.M.savedrecs.174$Paper.splt <- lclcttion.M.savedrecs.174$Paper
lclcttion.M.savedrecs.174$Year <- str_split_fixed(lclcttion.M.savedrecs.174$Paper.splt, ",", 3)[,2] #split the column and take the second element of the matrix

my.table8 <- lclcttion.M.savedrecs.174[, c( "Paper","Title","Authors","AU_UN","LCS","GCS","GCS/t","LCS/t","Average Article Citations")]

my.table8 <-  my.table8 [order(-my.table8$`GCS/t`),]

head(my.table8)

write.csv(my.table8, file = "my.table8.csv")


x.big <- xtable(my.table8, caption = "A \\code{longtable} spanning several pages")
print(x.big, hline.after=c(-1, 0), tabular.environment = "longtable")




x <- matrix(rnorm(1000), ncol = 10)
x.big <- xtable(x, caption = "A \\code{longtable} spanning several pages")
print(x.big, hline.after=c(-1, 0), tabular.environment = "longtable")



















###############################################################################
###############################################################################
#Delete

#IN SOME CELLS THERE ARE MORE THEN 1 AFFILIATION JUST TAKE THE FIRST ONE AND NAME IT AU11_UN
s <- lclcttion.M.savedrecs.174$AU1_UN 
s1 = strsplit(s, split=';'); s1
lclcttion.M.savedrecs.174$AU1_UN  <- (mycol = unlist(lapply(s1, function(x){x[1]})))
lclcttion.M.savedrecs.174$AU1_UN 
class(lclcttion.M.savedrecs.174$AU1_UN)  
 
names(lclcttion.M.savedrecs.174)
#CHECK THIS ONE 
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
              dplyr::select(Paper, Title,Year, LCS, GCS,Average.per.Year, AU, AU_UN, Source.Title, Total.Citations ) %>% # ,Average.per.Year
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

#

my.papers4 <- my.papers1[, c("AU",    "Year", "LCS",    #   "AU",    "Year",  "Paper" 
                             "GCS" ,"Average.per.Year"
                             )] %>% # "Affiliation.Paper","sum.LCS","sum.GCS", "AU1_UN"
                           arrange(desc(GCS))
colnames(my.papers4) <- c("Author","Year", "LCS", 
                          "GCS", "GCS/t"  #"Citation/t", 
                           ) #"#Papers","sum.LCS","sum.GCS", "AU1_UN"
           
my.papers4$Year <- as.integer(my.papers4$Year)
my.papers4$LCS <- as.integer(my.papers4$LCS); my.papers4$GCS <- as.integer(my.papers4$GCS)
#my.papers4$Average.per.Year <- as.integer(my.papers4$Average.per.Year)
#my.papers4$`Citation/t` <- as.integer(my.papers4$`Citation/t`)
#my.papers4$sum.LCS <- as.integer(my.papers4$sum.LCS); my.papers4$sum.GCS <- as.integer(my.papers4$sum.GCS)
my.papers4$`GCS/t` <- round(as.numeric(my.papers4$`GCS/t`),2)
print(xtable(my.papers4[c(1:50),], caption = 'Most Citited Papers Continue 2'), # , label="tab:MyTable1")
      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")

 

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

colnames(my.source.title) <- c("Sources","LCS" ,"GCS", "#Aff Papers", "Tot.LCS","TotGCS"  )

mrs <- S$MostRelSources; colnames(mrs) <- c("Sources", "Articles" )


#write.csv(mrs, file= "mrs.csv")
#mrs <- read.csv( "mrs.csv", header = T, sep = ","); class(mrs); names(mrs)
#mrs <- mrs[,c("Sources","Articles")]
#mrs$Sources <- as.character(mrs$Sources)
#my.source.title$Sources <- as.character(my.source.title$Sources)
#my.source.title <- as.data.frame(my.source.title) 

#a <- dplyr::inner_join(my.source.title, mrs, by = "Sources")




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


head(results$MostCitedPapers)



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

