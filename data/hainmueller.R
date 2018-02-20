
# scripts for plots in Causal Inference in Conjoint Analysis:
# Understanding Multidimensional Choices via Stated Preference Experiments
# Jens Hainmueller, Daniel Hopkins, Teppei Yamamoto
rm(list=ls())
library("ggplot2")

# Figure 2: Candidate Conjoint
results   <- c("Candrating.txt","Candchoiche.txt")
ffilenames<- c("Fig2a","Fig2b")
yylabs <- c("Change: Candidate Rating (0 'never support' - 1 'always support')",
            "Change: Pr(Prefer Candidate for President)")
yylims <- list(c(-.2, .2),c(-.4, .4))

for(i in 1:2){

# load estimatesfrom Stata 
d <- read.table(paste(results[i]))
ffilename <- ffilenames[i]
yylab <- yylabs[i]
yylim <- yylims[[i]]

d$var <- rownames(d)
colnames(d)[2] <- c("se")
colnames(d) <- c("pe","se","var")

# compute Cis
d$upper <-d$pe + 1.96*d$se
d$lower <-d$pe - 1.96*d$se

# group vars
Military     <- paste(c("1b",2),".atmilitary",sep="")
Religion     <- paste(c("1b",2:6),".atreligion",sep="")
Education    <- paste(c("1b",2:6),".ated",sep="")
Profession   <- paste(c("1b",2:6),".atprof",sep="")
Gender       <- paste(c("1b",2),".atmale",sep="")
Income       <- paste(c("1b",2:6),".atinc",sep="")
Race         <- paste(c("1b",2:6),".atrace",sep="")
Age          <- paste(c("1b",2:6),".atage",sep="")

d$gruppe <- NA
d$gruppe[d$var %in% Military]       <- "Military"
d$gruppe[d$var %in% Religion]       <- "Religion"
d$gruppe[d$var %in% Education]       <- "Education"
d$gruppe[d$var %in% Profession]       <- "Profession"
d$gruppe[d$var %in% Gender]       <- "Gender"
d$gruppe[d$var %in% Income]       <- "Income"
d$gruppe[d$var %in% Race]       <- "Race"
d$gruppe[d$var %in% Age]       <- "Age"

offset <- c("   ")

d$var[d$var %in% Gender] <- paste(offset,c("Male","Female"))

d$var[d$var %in% Military] <- paste(offset,c("Did not serve","Served"))

d$var[d$var %in% Religion] <- paste(offset,c("None",
                                             "Jewish",
                                             "Catholic",
                                             "Mainline protestant",
                                             "Evangelical protestant","Mormon"))

d$var[d$var %in% Education] <- paste(offset,c("No BA","Baptist college",
                                              "Community college","State university"
                                              ,"Small college","Ivy League university"))
  
  
d$var[d$var %in% Profession] <- paste(offset,c("Business owner",
                                               "Lawyer","Doctor",
                                               "High school teacher",
                                               "Farmer","Car dealer"))
  
d$var[d$var %in% Income] <- paste(offset,c("32K","54K",
                                           "65K","92K",
                                           "210K","5.1M"))


d$var[d$var %in% Race] <- paste(offset,c("White",
                                         "Native American",
                                         "Black",
                                         "Hispanic",
                                         "Caucasian","Asian American"))
  
d$var[d$var %in% Age] <- paste(offset,c(36,45,52,60,68,75))
                                                               
# add group labels
d$order <- 1:nrow(d)
dd <- data.frame(var= c("Military Service:",
                        " ",
                        "Religion:",
                        "  ",
                        "College:",
                        "   ",
                        "Profession:",
                        "    ",
                        "Gender:",
                        "     ",
                        "Income:",
                        "      ",
                        "Race/Ethnicity:",
                        "       ",
                        "Age:"
                        ),order=c(.5,2.1,2.5,8.1,8.5,14.1,14.5,20.1,20.5,22.1,22.5,28.1,28.5,34.1, 34.5),
                 pe=1,se=1,upper=1,lower=1,gruppe=NA)
d <- rbind(d,dd)
d <-d[order(d$order),]
d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])

# plot 
p = ggplot(d,aes(y=pe,x=var))##,colour=gruppe))
p = p + coord_flip(ylim = yylim)  
p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_colour_discrete("Attribute:") + scale_x_discrete(name="")
print(p)

# colour scheme
theme_bw1 <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_line(colour = "grey50"),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}

dev.off()
pdf(paste(ffilename,".pdf",sep=""),width=7,height=8) # for paper
p = p  + theme_bw1()
print(p)
dev.off()

}


## Figure 3: Immigration Coinjoint
rm(list=ls())
library("ggplot2")

d <- read.table("Immiglongregression.txt",comment.char="")
ffilename <- "Fig3"

  # group vars
  Gender     <- paste(c("1b",2),".FeatGender",sep="")
  Education     <- paste(c("1b",2:7),".FeatEd",sep="")
  LanguageSkills    <- paste(c("1b",2:4),".FeatLang",sep="")
  Origin   <- paste(c(1:5,"6b",7:10),".FeatCountry",sep="")
  Job       <- paste(c("1b",2:11),".FeatJob",sep="")
  JobExperience       <- paste(c("1b",2:4),".FeatExp",sep="")
  JobPlans       <- paste(c(1:2,"3b",4),".FeatPlans",sep="")
  ReasonforApp         <- paste(c("1b",2:3),".FeatReason",sep="")
  PriorEntry          <- paste(c("1b",2:5),".FeatTrips",sep="")


CIs <- function(d){
  colnames(d)[1:2] <- c("pe","se")
  d$upper <-d$pe + 1.96*d$se
  d$lower <-d$pe - 1.96*d$se
  return(d)
}
d<- CIs(d)
d$var <- rownames(d)

FillGroup <- function(d){

  d$gruppe <- NA
  d$gruppe[d$var %in% Gender]         <- "Gender"
  d$gruppe[d$var %in% Education]      <- "Education"
  d$gruppe[d$var %in% Job]            <- "Job"
  d$gruppe[d$var %in% Origin]         <- "Origin"
  d$gruppe[d$var %in% ReasonforApp]   <- "Reason for Application"
  d$gruppe[d$var %in% JobExperience]  <- "Job Experience"
  d$gruppe[d$var %in% PriorEntry]     <- "Prior Entry"
  d$gruppe[d$var %in% JobPlans]       <- "Job Plans"
  d$gruppe[d$var %in% LanguageSkills] <- "Language Skills"
  
  # reorder
  d <- rbind(d[d$var %in% Gender,],
             d[d$var %in% Education,],
             d[d$var %in% LanguageSkills,],
             d[d$var %in% Origin,],
             d[d$var %in% Job,],
             d[d$var %in% JobExperience,],
             d[d$var %in% JobPlans,],
             d[d$var %in% ReasonforApp,],
             d[d$var %in% PriorEntry,])
return(d)
}

d <- FillGroup(d)
d$order <- 1:nrow(d)

GetLabels <- function(d){
  offset <- c("   ")
  
  d$var[d$var %in% Gender] <- paste(offset,c("female","male"))
  d$var[d$var %in% Education] <- paste(offset,c("no formal","4th grade",
                                                "8th grade","high school",
                                                "two-year college","college degree",
                                                "graduate degree"))
  
  d$var[d$var %in% LanguageSkills] <- paste(offset,c("fluent English",
                                                     "broken English",
                                                     "tried English but unable",
                                                     "used interpreter"))
  
  d$var[d$var %in% Origin] <- paste(offset,c("Germany","France","Mexico",
                                             "Philippines","Poland","India",
                                             "China","Sudan","Somalia","Iraq"))
  
  d$var[d$var %in% Job] <- paste(offset,c("janitor","waiter","child care provider",
                                          "gardener","financial analyst",
                                          "construction worker","teacher",
                                          "computer programmer","nurse",
                                          "research scientist","doctor"))
  
  d$var[d$var %in% ReasonforApp] <- paste(offset,c("reunite with family",
                                                   "seek better job",
                                                   "escape persecution"))
  
  
  d$var[d$var %in% JobExperience] <- paste(offset,c("none","1-2 years",
                                                    "3-5 years","5+ years"))
  
  
  d$var[d$var %in% JobPlans] <- paste(offset,c("contract with employer",
                                               "interviews with employer",
                                               "will look for work",
                                               "no plans to look for work"))
  
  d$var[d$var %in% PriorEntry] <- paste(offset,c("never","once as tourist",
                                                 "many times as tourist","six months with family",
                                                 "once w/o authorization"))
return(d)
}

d <- GetLabels(d)
  
  # bring in sublabels           
  d <- d[order(d$order),]
  dd <- data.frame(var= c("Gender:",
                          " ",
                          "Education:",
                          "  ",
                          "Language:",
                          "   ",
                          "Origin:",
                          "    ",
                          "Profession:",
                          "     ",
                          "Job experience:",
                          "      ",
                          "Job plans:",
                          "       ",
                          "Application reason:",
                          "        ",
                          "Prior trips to U.S.:"
  ),order=c(.5,2.1,2.5,9.1,9.5,13.1,13.5,23.1,23.5,34.1,34.5,38.1,38.5,42.1, 42.5,45.1,45.5),
                   pe=1,se=1,upper=1,lower=1,gruppe=NA)
  d <- rbind(d,dd)
  d <-d[order(d$order),]
  d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])

yylab  <- c("Change in Pr(Immigrant Preferred for Admission to U.S.)")

p = ggplot(d,aes(y=pe,x=var))#,colour=gruppe))
p = p + coord_flip(ylim = c(-.3, .3))  
p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_colour_discrete("Attribute:") + scale_x_discrete(name="")
print(p)

theme_bw1 <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size , colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_line(colour = "grey50"),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}

dev.off()
pdf(paste(ffilename,".pdf",sep=""),width=10,height=12.5) # for paper
p = p  + theme_bw1()
print(p)
dev.off()


## Figure 4: By Ethnocentrism
k=2
subfilename <- "Fig4"
subsetnlabel <- ""
slevels <- c(1,2)

d <- read.table("Ethno1.txt",comment.char="")
rows <- rownames(d)
p <- nrow(d)
for(i in 2:k){
  d <-   rbind(d,read.table(paste("Ethno",i,".txt",sep=""),comment.char=""))
}
d$subset      <- rep(1:k,each=p)
d$subsetlabel <- paste(subsetnlabel, rep(c("Low Ethnocentrism","High Ethnocentrism"),each=p))


d <- CIs(d)
d$var <- rep(rows,k)
d <- FillGroup(d)
d$order <- 1:nrow(d)
d <- d[order(d$subset,d$order),]
d$order <- 1:nrow(d)
d <- GetLabels(d)
d <- d[order(d$order),]
dd <- data.frame(var= c("Gender:",
                        " ",
                        "Education:",
                        "  ",
                        "Language:",
                        "   ",
                        "Origin:",
                        "    ",
                        "Profession:",
                        "     ",
                        "Job experience:",
                        "      ",
                        "Job plans:",
                        "       ",
                        "Application reason:",
                        "        ",
                        "Prior trips to U.S.:"
),order=c(.5,2.1,2.5,9.1,9.5,13.1,13.5,23.1,23.5,34.1,34.5,38.1,38.5,42.1, 42.5,45.1,45.5),
                 pe=1,se=1,upper=1,lower=1,gruppe=NA,subset=NA,subsetlabel=NA)
d <- rbind(d,dd)
d <-d[order(d$order),]
d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])

d$subsetlabel[is.na(d$subsetlabel)] <- unique(d$subsetlabel)[2]
d$subsetlabel <- factor(d$subsetlabel,levels=unique(d$subsetlabel))

p = ggplot(d ,aes(y=pe,x=var))#,colour=gruppe))
p = p + facet_grid(.~subsetlabel)
p = p + coord_flip(ylim = c(-.3, .3))
p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_x_discrete(name="") + scale_colour_discrete("")
print(p)

dev.off()
pdf(paste(subfilename,".pdf",sep=""),width=14,height=9.5) # for paper
p = p  + theme_bw1()
print(p)
dev.off()

# Figure 5: By Immigrants Job Plans
k=2
subfilename <- "Fig5"
subsetnlabel <- ""
slevels <- c(1,2)

d <- read.table("Plans1.txt",comment.char="")
rows <- rownames(d)
p <- nrow(d)
for(i in 2:k){
  d <-   rbind(d,read.table(paste("Plans",i,".txt",sep=""),comment.char=""))
}
d$subset      <- rep(1:k,each=p)
d$subsetlabel <- paste(subsetnlabel, rep(c("Immigrant Has Contract with an Employer",
                                           "Immigrant Has No Plans to Look for Work"),each=p))


d <- CIs(d)
d$var <- rep(rows,k)
d <- FillGroup(d)

d$order <- 1:nrow(d)
d <- d[order(d$subset,d$order),]
d$order <- 1:nrow(d)
d <- GetLabels(d)

d <- d[order(d$order),]
dd <- data.frame(var= c("Gender:",
                        " ",
                        "Education:",
                        "  ",
                        "Language:",
                        "   ",
                        "Origin:",
                        "    ",
                        "Profession:",
                        "     ",
                        "Job experience:",
                        "      ",
                        "Application reason:",
                        "        ",
                        "Prior trips to U.S.:"
),order=c(.5,2.1,2.5,9.1,9.5,13.1,13.5,23.1,23.5,34.1,34.5,38.1,c(42.5,45.1,45.5)-4),
                 pe=1,se=1,upper=1,lower=1,gruppe=NA,subset=NA,subsetlabel=NA)
d <- rbind(d,dd)
d <-d[order(d$order),]
d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])

d$subsetlabel[is.na(d$subsetlabel)] <- unique(d$subsetlabel)[2]
d$subsetlabel <- factor(d$subsetlabel,levels=unique(d$subsetlabel))

p = ggplot(d ,aes(y=pe,x=var))#,colour=gruppe))
p = p + facet_grid(.~subsetlabel)
p = p + coord_flip(ylim = c(-.4, .4))
p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.2,.2,.2),1),labels=c("-.2","0",".2"))
p = p + scale_x_discrete(name="") + scale_colour_discrete("")
print(p)

dev.off()
pdf(paste(subfilename,".pdf",sep=""),width=14,height=9.5) # for paper
p = p  + theme_bw1()
print(p)
dev.off()


# Figure A.2: Effects of Immigrant Attributes on Preference for Admission by Choice Task

k=5
subfilename <- "FigA2"
subsetnlabel <- ""
slevels <- c(1:5)

# load subsets
d <- read.table("Task1.txt",comment.char="")
rows <- rownames(d)
p <- nrow(d)
for(i in 2:k){
  d <-   rbind(d,read.table(paste("Task",i,".txt",sep=""),comment.char=""))
}
d$subset      <- rep(1:k,each=p)
d$subsetlabel <- paste(subsetnlabel, rep(paste("Task Number:",1:5),each=p))

d <- CIs(d)
d$var <- rep(rows,k)
d <- FillGroup(d)

d$order <- 1:nrow(d)
d <- d[order(d$subset,d$order),]
d$order <- 1:nrow(d)
d <- GetLabels(d)

d <- d[order(d$order),]
dd <- data.frame(var= c("Gender:",
                        " ",
                        "Education:",
                        "  ",
                        "Language:",
                        "   ",
                        "Origin:",
                        "    ",
                        "Profession:",
                        "     ",
                        "Job experience:",
                        "      ",
                        "Job plans:",
                        "       ",
                        "Application reason:",
                        "        ",
                        "Prior trips to U.S.:"
),order=c(.5,2.1,2.5,9.1,9.5,13.1,13.5,23.1,23.5,34.1,34.5,38.1,38.5,42.1, 42.5,45.1,45.5),
                 pe=1,se=1,upper=1,lower=1,gruppe=NA,subset=NA,subsetlabel=NA)
d <- rbind(d,dd)
d <-d[order(d$order),]
d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])

d$subsetlabel[is.na(d$subsetlabel)] <- unique(d$subsetlabel)[2]
d$subsetlabel <- factor(d$subsetlabel,levels=unique(d$subsetlabel))

p = ggplot(d ,aes(y=pe,x=var))#,colour=gruppe))
p = p + facet_grid(.~subsetlabel)
p = p + coord_flip(ylim = c(-.4, .4))
p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.2,.2,.2),1),labels=c("-.2","0",".2"))
p = p + scale_x_discrete(name="")
p = p + scale_colour_discrete("")
print(p)

dev.off()
pdf(paste(subfilename,".pdf",sep=""),width=14,height=9.5) # for paper
p = p  + theme_bw1()
print(p)
dev.off()

# Figure A.3: Effects of Immigrant Attributes on Preference for Admission by Profile Number
k=2
subfilename <- "FigA3"
subsetnlabel <- ""
slevels <- c(1,2)

# load subsets
d <- read.table("Profile1.txt",comment.char="")
rows <- rownames(d)
p <- nrow(d)
for(i in 2:k){
  d <-   rbind(d,read.table(paste("Profile",i,".txt",sep=""),comment.char=""))
}
d$subset      <- rep(1:k,each=p)
d$subsetlabel <- paste(subsetnlabel, rep(paste("Profile Number:",1:2),each=p))

d <- CIs(d)
d$var <- rep(rows,k)
d <- FillGroup(d)

d$order <- 1:nrow(d)
d <- d[order(d$subset,d$order),]
d$order <- 1:nrow(d)
d <- GetLabels(d)

d <- d[order(d$order),]
dd <- data.frame(var= c("Gender:",
                        " ",
                        "Education:",
                        "  ",
                        "Language:",
                        "   ",
                        "Origin:",
                        "    ",
                        "Profession:",
                        "     ",
                        "Job experience:",
                        "      ",
                        "Job plans:",
                        "       ",
                        "Application reason:",
                        "        ",
                        "Prior trips to U.S.:"
),order=c(.5,2.1,2.5,9.1,9.5,13.1,13.5,23.1,23.5,34.1,34.5,38.1,38.5,42.1, 42.5,45.1,45.5),
                 pe=1,se=1,upper=1,lower=1,gruppe=NA,subset=NA,subsetlabel=NA)
d <- rbind(d,dd)
d <-d[order(d$order),]
d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])

d$subsetlabel[is.na(d$subsetlabel)] <- unique(d$subsetlabel)[2]
d$subsetlabel <- factor(d$subsetlabel,levels=unique(d$subsetlabel))

p = ggplot(d ,aes(y=pe,x=var))#,colour=gruppe))
p = p + facet_grid(.~subsetlabel)
p = p + coord_flip(ylim = c(-.4, .4))
p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.2,.2,.2),1),labels=c("-.2","0",".2"))
p = p + scale_x_discrete(name="") + scale_colour_discrete("")
print(p)

dev.off()
pdf(paste(subfilename,".pdf",sep=""),width=14,height=9.5) # for paper
p = p  + theme_bw1()
print(p)
dev.off()


# Figure A.4: Effects of Immigrant Attributes on Preference for Admission by Row Position of Attribute

xxlab  <- yylab
for(i in 1:2){
d <- read.table(c("LangByPosition.txt",
                  "PriorByPosition.txt")[i],comment.char="",row.names=NULL)
mmain <- c("Language: fluent English -> used interpreter",
           "Prior trips to U.S.: never -> once w/o authorization")[i]
yylab=c("Row Position of Language Attribute",
        "Row Position of Prior trips Attribute")[i]
subfilename <- c("FigA4a","FigA4b")[i]

d$position[d$position==-1] <- 0
d$position <- d$position*-1

pdf(paste(subfilename,".pdf",sep=""),width=8,height=7) 
plot(y=d$position,x=d$pe,xlim=c(-.4,.1),xlab=xxlab,ylim=c(-9,.5),yaxt="n",
     ylab=yylab,main=mmain)
arrows(x0=d$lb,y0=d$position,
       x1=d$up,
       code=3,angle=90,length=.1
)
points(y=d$position[1],x=d$pe[1],col="black",pch=19)
arrows(x0=d$lb[1],y0=d$position[1],
       x1=d$up[1],
       code=3,angle=90,length=.1,col="black"
)
abline(v=0,lty=2,col="blue",lwd=.5)
axis(2,at=seq(-9,0,1),labels=c(seq(9,1,-1),"Pooled"))
dev.off()

}


# Figure A.5: Effects of Immigrant Attributes on Preference for Admission by Number of Atypical Profiles

k=3
subfilename <- "FigA5"
subsetnlabel <- ""
slevels <- c(1:3)

d <- read.table("Atypical1.txt",comment.char="")
rows <- rownames(d)
p <- nrow(d)
for(i in 2:k){
  d <-   rbind(d,read.table(paste("Atypical",i,".txt",sep=""),comment.char=""))
}
d$subset      <- rep(1:k,each=p)
d$subsetlabel <- paste(subsetnlabel, rep(paste("# of countertypical profiles: ",
                                               c("0-3","4-5","6-9")),each=p))

d <- CIs(d)
d$var <- rep(rows,k)
d <- FillGroup(d)

d$order <- 1:nrow(d)
d <- d[order(d$subset,d$order),]
d$order <- 1:nrow(d)
d <- GetLabels(d)

d <- d[order(d$order),]
dd <- data.frame(var= c("Gender:",
                        " ",
                        "Education:",
                        "  ",
                        "Language:",
                        "   ",
                        "Origin:",
                        "    ",
                        "Profession:",
                        "     ",
                        "Job experience:",
                        "      ",
                        "Job plans:",
                        "       ",
                        "Application reason:",
                        "        ",
                        "Prior trips to U.S.:"
),order=c(.5,2.1,2.5,9.1,9.5,13.1,13.5,23.1,23.5,34.1,34.5,38.1,38.5,42.1, 42.5,45.1,45.5),
                 pe=1,se=1,upper=1,lower=1,gruppe=NA,subset=NA,subsetlabel=NA)
d <- rbind(d,dd)
d <-d[order(d$order),]
d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])

d$subsetlabel[is.na(d$subsetlabel)] <- unique(d$subsetlabel)[2]
d$subsetlabel <- factor(d$subsetlabel,levels=unique(d$subsetlabel))

yylab  <- c("Change in Pr(Immigrant Preferred for Admission to U.S.)")

p = ggplot(d ,aes(y=pe,x=var))#,colour=gruppe))
p = p + facet_grid(.~subsetlabel)
p = p + coord_flip(ylim = c(-.4, .4))
p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
p = p +geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
p = p + scale_y_continuous(name=yylab,breaks=round(seq(-.2,.2,.2),1),labels=c("-.2","0",".2"))
p = p + scale_x_discrete(name="") + scale_colour_discrete("")
print(p)

dev.off()
pdf(paste(subfilename,".pdf",sep=""),width=14,height=9.5) # for paper
p = p  + theme_bw1()
print(p)
dev.off()



