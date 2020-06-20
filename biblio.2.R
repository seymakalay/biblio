
rm(list=ls())
library(devtools); library( dplyr ); library(stringr); library(tidyr);library(plyr)
library(RTextTools); library(data.table);library(xtable);library(ggrepel)
library(bibliometrix); library(ggplot2);library(kableExtra)
library(devtools);library(kableExtra) 
#biblioshiny() 

M <- convert2df("savedrecs.370.txt", dbsource = "wos", format = "plaintext") 
M <-  M[!is.na(M$PY), ] #REMOVE WHEN PUBLISH YEAR IS NA (PY=NA) TO CONSIDER ONLY PUBLISH PAPER
M <- cbind(rownames(M), M)
rownames(M) <- NULL
colnames(M) <- c(names(M))

colnames(M)[1] <- "Paper"
names(M)

#write.csv(M, file = "M.csv") # Change the political connections with political connection
M <- read.csv(file = "M.csv",  sep = ",", header = T, stringsAsFactors = FALSE) #add delete the first nrow column




results <- biblioAnalysis(M, sep = ";")  #plot(x = results, k = 10, pause = FALSE)

attributes(results)
attributes(M)

#######
#It calculates local citations (LCS) of authors and documents of a bibliographic collection
lclcttion <- localCitations(M, fast.search = F, sep = ";"); lclcttion; attributes(lclcttion)

head(lclcttion$Authors)
head(lclcttion$Papers) # attributes(lclcttion$Papers)  "Paper" "DOI"   "Year"  "LCS"   "GCS"  

any(is.na(lclcttion$Papers$Year))
any(is.na(lclcttion$Papers$LCS))
any(is.na(lclcttion$Papers$GCS))

lclcttion$Papers$Paper <- as.character(lclcttion$Papers$Paper); class(lclcttion$Papers$Paper)
M$Paper <- as.character(M$Paper)

#TC AS GCS SAME THINGS
lclcttion$Papers <- lclcttion$Papers[, c("Paper","Year","LCS","GCS")]; names(lclcttion$Papers) #131 obs shows only the papers where localcitation avaliable

lclcttion.M.savedrecs <- right_join(lclcttion$Papers, M, by = "Paper")
#
write.csv(lclcttion.M.savedrecs, file = "lclcttion.M.savedrecs.csv")
str(lclcttion.M.savedrecs)
############
any(is.na(lclcttion.M.savedrecs$LCS))
any(is.na(lclcttion.M.savedrecs$GCS))
any(is.na(lclcttion.M.savedrecs$Year))
#
library(stringr)
names(lclcttion.M.savedrecs)
#
lclcttion.M.savedrecs$Year <- as.integer(lclcttion.M.savedrecs$Year)
lclcttion.M.savedrecs$today <- 2020
lclcttion.M.savedrecs$tdy_year <-  lclcttion.M.savedrecs$today - lclcttion.M.savedrecs$Year + 1
head(lclcttion.M.savedrecs$tdy_year); head(lclcttion.M.savedrecs$Year)
#
lclcttion.M.savedrecs$LCS <- as.integer(lclcttion.M.savedrecs$LCS)
lclcttion.M.savedrecs$GCS <- as.integer(lclcttion.M.savedrecs$GCS)
#
lclcttion.M.savedrecs$`LCS/t` <- as.numeric(round(lclcttion.M.savedrecs$LCS/lclcttion.M.savedrecs$tdy_year ,2))
lclcttion.M.savedrecs$`GCS/t` <- as.numeric(round(lclcttion.M.savedrecs$GCS/lclcttion.M.savedrecs$tdy_year ,2))
lclcttion.M.savedrecs$AAC     <- as.numeric(round(lclcttion.M.savedrecs$GCS/lclcttion.M.savedrecs$tdy_year ,2)) # ACC = Average Article Citations
#
lclcttion.M.savedrecs$Paper <- str_to_title(lclcttion.M.savedrecs$Paper)
lclcttion.M.savedrecs$TI <- str_to_title(lclcttion.M.savedrecs$TI)
lclcttion.M.savedrecs$AU <- str_to_title(lclcttion.M.savedrecs$AU)
lclcttion.M.savedrecs$J9 <- str_to_title(lclcttion.M.savedrecs$J9)
lclcttion.M.savedrecs$SO <- str_to_title(lclcttion.M.savedrecs$SO)
lclcttion.M.savedrecs$AF <- str_to_title(lclcttion.M.savedrecs$AF)
lclcttion.M.savedrecs$AU_UN <- str_to_title(lclcttion.M.savedrecs$AU_UN)
#
lclcttion.M.savedrecs$Paper <-  gsub(",\\s*", ", ", lclcttion.M.savedrecs$Paper)
lclcttion.M.savedrecs$TI <- gsub(",\\s*", ", ", lclcttion.M.savedrecs$TI)
lclcttion.M.savedrecs$TI <-  gsub(":\\s*", ": ", lclcttion.M.savedrecs$TI)
lclcttion.M.savedrecs$AU <-  gsub(";\\s*", "; ", lclcttion.M.savedrecs$AU)
#

###############################################################################
###############################################################################
options(width = 10000)
S <- summary(object = results, k = 60,  pause = FALSE); attributes(S)  #check the max K cause it depends on TCpc length
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

#make a table of most productive countriEs
mpc$Freq <- as.numeric(mpc$Freq)
mpc$SCP  <- as.numeric(mpc$SCP)
mpc$MCP  <- as.numeric(mpc$MCP)

mpc$SCP_Ratio <- mpc$SCP/sum(mpc$SCP)
#mpc$MCP_Ratio <- as.numeric(mpc$MCP_Ratio)  
mpc$MCP_Ratio <- mpc$MCP/sum(mpc$MCP)
mpc <- mpc[,c(1,2,4,5,3,6,7)]; mpc

###############################################################################
###############################################################################

#Most Citated Countries
names(TCpc); names(mpc)
colnames(TCpc) <- c("Country", "TGC","AAC") #ACC = Average Article Citations
TCpc <- as.data.frame(TCpc); mpc <- as.data.frame(mpc)
head(mpc); head(TCpc)
mst.ctd.CO <- merge(mpc, TCpc, by = "Country"); head(mst.ctd.CO)
mst.ctd.CO$Country <- str_to_title(mst.ctd.CO$Country)
#
#mst.ctd.CO$`Average Article Citations` <- round(as.numeric(mst.ctd.CO$`Average Article Citations`),2)
mst.ctd.CO$TGC <- as.integer(mst.ctd.CO$TGC)
mst.ctd.CO$SCP <- as.integer(mst.ctd.CO$SCP)
mst.ctd.CO$MCP <- as.integer(mst.ctd.CO$MCP)
mst.ctd.CO$Articles <- as.integer(mst.ctd.CO$Articles)
mst.ctd.CO$AAC <- as.numeric(mst.ctd.CO$AAC)

#
mst.ctd.CO <- mst.ctd.CO[order(-mst.ctd.CO$Articles),]; mst.ctd.CO
mst.ctd.CO <- mst.ctd.CO[,c(1,2,5,3,7,4,6,8,9)]; mst.ctd.CO

colnames(mst.ctd.CO) <- c( "Country","Articles","%Freq","SCP","%SCP", "MCP","%MCP","TGC","TGC/t") #AAC STANDS FOR AVERAGE ARTICLE CITATATION
#
rownames(mst.ctd.CO) <- NULL ; mst.ctd.CO #TO REMOVE THE ROW NUMBERS

mst.ctd.CO$Country <- str_to_title(mst.ctd.CO$Country)

mst.ctd.CO   <- xtable(mst.ctd.CO[c(1:10),],   digits = 3, 
                       caption = 'Most Influancial Countries', label = "tab:Most-Cited-Countries", scalebox='0.80')

print(mst.ctd.CO, #file = 'mst.ctd.CO.tex',  
      caption.placement = 'bottom' ) 

#mst.ctd.CO <- mst.ctd.CO[order(-mst.ctd.CO$`Average Article Citatation`),]; mst.ctd.CO
#print(xtable(mst.ctd.CO[c(1:20),], caption = 'Most Citated Countries',label="tab:Most-Citated-Countries"),
#      caption.placement = 'top',size="\\fontsize{9pt}{10pt}\\selectfont")
###############################################################################
###############################################################################
#Most Citited Papers 
names(lclcttion.M.savedrecs)

mst.ctd.PA <- lclcttion.M.savedrecs %>%
              arrange(desc(`GCS/t`)) ; head(mst.ctd.PA,10)
             # arrange(desc(`LCS/t`)) ; head(mst.ctd.PA,10)

mst.ctd.PA <- mst.ctd.PA[, c("AU","Year", "TI", "LCS","LCS/t", "GCS", "GCS/t")]
colnames(mst.ctd.PA)    <- c("Author(s) Name",  "Year", "Title","TLC","TLC/t", "TGC", "TGC/t") 

rownames(mst.ctd.PA) <- NULL # TO REMOVE ROW NUMBERS

mst.ctd.PA   <- xtable(mst.ctd.PA [c(1:10),],   digits = 2, 
                       caption = 'Most Influential Articles',label="tab:Most-Cited-Papers", scalebox='0.80')

print(xtable(mst.ctd.PA,
             #digits=rep(0,6),
             align=c(
               "p{0.2cm}|",
               "p{3cm}|","p{0.6cm}|",
               "p{7cm}|","p{0.7cm}|",
               "p{0.8cm}|","p{0.7cm}|",
               "p{0.8cm}|")),
      hline.after=c(-1, 0) )#,
     # file = 'mst.ctd.PA.tex')

#Add it  
#\end{tabular}
#\caption{GLM models with asset holding variables Networth, NW-HE (net-worth minus home equity) and liquid assets. Odd ratios (OR) are reported with significance: $^*p<0.1;~^{**}p<0.05;~^{***}p<0.01$. }
#\label{tab:Most-Cited-Papers}
#\end{table}

###############################################################################
###############################################################################
#Most Citited Journals

#lclcttion.M.savedrecs$`LCS/t` <- round(lclcttion.M.savedrecs$LCS/lclcttion.M.savedrecs$tdy_year ,2)
mst.ctd.JOR <- lclcttion.M.savedrecs %>%
  dplyr::group_by(SO)%>%
  dplyr::summarise(
                   Affiliation.Paper = n(),
                   sum.LCS.t = sum(`LCS/t`),
                   sum.GCS.t = sum(`GCS/t`),
                   sum.LCS = sum(LCS),
                   sum.GCS = sum(GCS))%>%
  #dplyr::arrange(desc(Affiliation.Paper))
  #dplyr::arrange(desc(sum.LCS.t))
  dplyr::arrange(desc(sum.GCS.t))

head(mst.ctd.JOR); names(mst.ctd.JOR)
mst.ctd.JOR <- mst.ctd.JOR[, c("SO","Affiliation.Paper","sum.LCS","sum.LCS.t", "sum.GCS","sum.GCS.t")];head(mst.ctd.JOR)
colnames(mst.ctd.JOR) <-     c("Journal","Article", "TLC","TLC/t", "TGC", "TGC/t" )

#rownames(mst.ctd.JOR ) <- NULL

mst.ctd.JOR   <- xtable(mst.ctd.JOR[c(1:10),],   digits = 2, 
                        caption = 'Most Citated Journals',label="tab:Most-Cited-Journals", scalebox='0.80')
mst.ctd.JOR 
#print(mst.ctd.JOR, file = 'mst.ctd.JOR.tex',  caption.placement = 'top' ) 

###############################################################################
###############################################################################
#Most Citited Affiliations

mst.ctd.AFF <- lclcttion.M.savedrecs  %>%
  dplyr::mutate(row = dplyr::row_number()) %>%
  tidyr::separate_rows(AU_UN, sep = ";") %>%
  distinct() %>%
  dplyr::group_by(AU_UN) %>%
  dplyr::summarise(sum.papers = dplyr::n_distinct(row), 
                   sum.GCS = sum(GCS), 
                   sum.LCS = sum(LCS),
                   sum.LCS.t = sum(`LCS/t`),
                   sum.GCS.t = sum(`GCS/t`)
  ) %>%
  #dplyr::arrange(desc(sum.papers)); mst.ctd.AFF
  dplyr::arrange(desc(sum.LCS)) 

mst.ctd.AFF <- na.omit(mst.ctd.AFF); mst.ctd.AFF

mst.ctd.AFF <- mst.ctd.AFF[,c(1,2,4,5,3,6)]; mst.ctd.AFF

colnames(mst.ctd.AFF) <- c("Affiliation", "Articles","TLC","TLC/t","TGC", "TGC/t")

rownames(mst.ctd.AFF) <- NULL
mst.ctd.AFF$Affiliation <- str_to_title(mst.ctd.AFF$Affiliation)

mst.ctd.AFF <- xtable(mst.ctd.AFF[c(1:10),],   digits = 2, 
                        caption = 'Most Citated Affliations',label="tab:Most-Cited-Affliations", scalebox='0.80')

print(mst.ctd.AFF, #file = 'mst.ctd.AFF.tex',  
      caption.placement = 'bottom' ) 

###############################################################################
###############################################################################
#Most Citited ARTHURS

#lclcttion.M.savedrecs$AU1st <- unlist(lapply(strsplit(lclcttion.M.savedrecs$AF, split=';', fixed=TRUE), `[`, 1))
lclcttion.M.savedrecs$AU1st <- unlist(lapply(strsplit(lclcttion.M.savedrecs$AU, split=';', fixed=TRUE), `[`, 1))
lclcttion.M.savedrecs$AU1_UN <- unlist(lapply(strsplit(lclcttion.M.savedrecs$AU_UN, split=';', fixed=TRUE), `[`, 1))

AUTHR <- lclcttion.M.savedrecs  %>%
         dplyr::group_by(AU1st) %>%
         dplyr::summarise(
                          uni = first(AU1_UN),
                          tot.papers = n(),
                          sum.LCS = sum(LCS),
                          sum.LCS.t = sum(`LCS/t`),
                          sum.GCS = sum(GCS), 
                          sum.GCS.t = sum(`GCS/t`))%>%
         #dplyr::arrange(desc(tot.papers)) %>%
         dplyr::arrange(desc(sum.LCS.t)) %>%
         #dplyr::arrange(desc( sum.GCS.t)) %>%
         as.data.frame(); head(AUTHR,10)

colnames(AUTHR) <- c("1st Author", "University", "Articles", "TLC","TLC/t", "TGC","TGC/t"); head(AUTHR,10)


AUTHR   <- xtable(AUTHR[c(1:10),],   digits = 2, 
                       caption = 'Most Influancial Authors', label = "tab:AUTHR", scalebox='0.80')

print(AUTHR , #file = 'AUTHR.tex',  
      caption.placement = 'bottom' ) 



#lclcttion.M.savedrecs.174$Paper.splt <- lclcttion.M.savedrecs.174$Paper
#lclcttion.M.savedrecs.174$Year <- str_split_fixed(lclcttion.M.savedrecs.174$Paper.splt, ",", 3)[,2] 


###############################################################################
###############################################################################

#making a table
names(lclcttion.M.savedrecs)

my.table <- lclcttion.M.savedrecs[, c( "Paper","TI",#"Authors",#"AU_UN",
 "LCS", "LCS/t","GCS","GCS/t")] #AAC=Average Article Citations

my.table$Paper <- gsub(";\\s*", "; ", my.table$Paper)

my.table <- my.table[order(my.table$Paper),]


#
colnames(my.table) <- c( "1st Author,Year,Journal","Title",#"Authors",#"AU_UN",
                          "TLC", "TLC/t", "TGC","TGC/t")

#my.table <-  my.table[order(-my.table$`TGC/t`),]
head(my.table)


#write.csv(my.table, file = "my.table.csv")
x.big <- xtable(my.table,  caption = 'Selected papers for bibliometric anaysis',label="tab:Biblio", scalebox='0.80')

rownames(x.big) <- NULL
print(x.big)




print(xtable(x.big,
             #digits=rep(0,6),
             align=c(
               "p{0.5cm}|",
               "p{4cm}|","p{6cm}|",
               "p{0.7cm}|","p{0.7cm}|",
               "p{0.7cm}|","p{0.7cm}|"
               )),
      hline.after=c(-1, 0),
      #file = 'x.big.tex' ,
      tabular.environment = "longtable")


###############################################################################
###############################################################################
#trend plot

df <- read.csv(file = "savedrecs.370.csv", header = TRUE, sep = ",", check.names=FALSE)
colnames(df)[1] <- "TI"
df$TI <- str_to_title(df$TI)

df.all <- merge(df, lclcttion.M.savedrecs, by =  "TI")
names(df.all)


df.vw <- df.all[, c(58,22:57)]; head(df.vw)


df.plt <-   df.vw %>%
            pivot_longer(cols=-1) %>%
            group_by(name) %>%
            filter(value==max(value)) %>%
            arrange(name, Paper) %>%
            select(name, Paper, value)

df.plt  <- as.data.frame(df.plt); head(df.plt) 
#d<-d[!(d$A=="B" & d$E==0),]
df.plt <- df.plt[,c(2,1,3)]; head(df.plt) 
#rownames(df.plt) <- NULL; head(df.plt)
df.plt <- df.plt[(df.plt$name >= 2000),]; head(df.plt) 

p.trend <- ggplot(df.plt, aes(name, value,label = Paper))+
           geom_point(color = ifelse(df.plt$value > 20, "red", "grey50"), size = 2.5) +
           #geom_point(color = 'blue')+
           #geom_text()+
           #geom_label(aes(label = Paper), size = 2)+
           theme_minimal(base_size = 12) + 
  theme( #plot.title=element_text(size=12,face="bold"),
         axis.text=element_text(size=12),
         axis.title=element_text(size=12))+#,face="bold"))+
           geom_text_repel(aes(label = Paper),  nudge_y = 0.5, #nudge_x = 1,
                            #box.padding = 0.5,
                            size = 3.5) +
  xlab("Year")+ 
  ylab("Total Global Citations (TGC)")

p.trend
ggsave(#path = "foldername", 
       filename ="p.trend.png", width = 40, height = 15, units = "cm")




#NETWORKS PART
#########################################
#########################################


#M <- convert2df("savedrecs.370.txt", dbsource = "wos", format = "plaintext") 


my.ntwrk  <- function(M, anlys, ntwrk, ttle){  
  NetMatrix <- biblioNetwork(M, analysis = anlys, network = ntwrk, sep = ";", shortlabel = TRUE)
  # Plot the network
  networkPlot(NetMatrix,  n = 10, Title = ttle, alpha = 0.5,
  normalize = "association", type="kamada" , cluster= "leading_eigen", label.color =T,
  size=10, size.cex=T, remove.multiple = FALSE, labelsize = 0.95,label.cex=F)
              
  }

#co-citation network
my.ntwrk(M,"co-citation","references","")
#authors’ collaboration network
my.ntwrk(M, "coupling", "authors","")
#keyword co-occurrences network
my.ntwrk(M, "co-occurrences", "author_keywords","") #author_keywords, keywords

my.ntwrk(M, "co-occurrences", "keywords","") # n=40 2c



##########################
##########################
##########################

my.ntwrk(M, "co-occurrences", "authors","")
my.ntwrk(M, "coupling", "authors","")

A <- cocMatrix(M, Field = "CR", sep = ";", binary = TRUE)


