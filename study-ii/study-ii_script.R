#Set here the workspace as the folder containing the working dataset
WORKINGDIR<-getwd()

#Load Data

if (file.exists("study-ii_responses.csv")) {
  data<-read.table("study-ii_responses.csv",sep=",",header=TRUE)
} else if (file.exists("input.txt")) {
  data<-read.table("input.txt",sep=",",header=TRUE)
} else {
  stop("No data for generating the plots")
}

#Remove the null answers
data_commons_not_null <- data[which(data["O_AC_majorChanges"] != 0 &
data["O_AC_diffCatClassMeth"] != 0 & data["O_AC_newMethUtilsCat"] != 0
& data["O_AC_author"] != 0 & data["A_AC_improvements"] != 0
& data["A_AC_deletedCC"] != 0 & data["A_AC_deprecatedCC"] != 0
& data["A_AC_refactoring"] != 0 & data["A_AC_fineGrainedChanges"] != 0
& data["A_AC_issueTrackLink"] != 0 & data["A_AC_nItems"] != 0),]


data_lucene_not_null <- data[which(data["O_L_optimizations"] != 0 &
data["O_L_backCompatibility"] != 0 & data["O_L_apiChanges"] != 0
& data["O_L_build"] != 0 & data["O_L_documentation"] != 0
& data["O_L_author"] != 0 & data["A_L_improvements"] != 0
& data["A_L_deletedCC"] != 0 & data["A_L_deprecatedCC"] != 0
& data["A_L_fineGrainedChanges"] != 0 & data["A_L_refactoring"] != 0 & data["A_L_CCvisibility"] != 0 & data["A_L_addedCC"] != 0  & data["A_L_modifiedCC"] != 0 & data["A_L_knownIssues"] != 0),]

#subsets
rn_experts <- data[which(data["RNcreation"] == 4),]
rn_experts_notNull <- rn_experts[which(rn_experts["fixBugs"] != 0 &
rn_experts["newFeatures"] != 0 & rn_experts["enhancedFeatures"] != 0
& rn_experts["newCC"] != 0 & rn_experts["modifiedCC"] != 0
& rn_experts["deprecatedCC"] != 0 & rn_experts["deletedCC"] != 0
& rn_experts["replacedCC"] != 0 & rn_experts["visibilityCC"] != 0
& rn_experts["configFiles"] != 0 & rn_experts["testSuites"] != 0
& rn_experts["refactorings"] != 0 & rn_experts["architecture"] != 0
& rn_experts["documentation"] != 0 & rn_experts["licensesChanges"] != 0
& rn_experts["librariesUp"] != 0),]

#SUMMARY INFO
#Developers background
#years of experience
summary(data$experience)
sd(data$experience)

#How often do you use software release notes?
summary(data$RNusage)

#Involved Subjects
summary(data$type)
#Use tool
summary(rn_experts$RNcreationTool)


#*******Box Plots participants experience*******

#years
pdf(paste(WORKINGDIR,"ProgrammingExperience.pdf",sep="/"), width=3, height=5)
boxplot(data$experience, col="gray", boxwex = 0.25, at = 1:1 - 0.0, ylab="Years of Programming Experience")
#points(1,mean(data$experience), col="red", pch=16)
dev.off() 

#release note usage
pdf(paste(WORKINGDIR,"ReleaseNoteUsage.pdf",sep="/"), width=3, height=5)
boxplot(data$RNusage, col="gray", boxwex = 0.25, at = 1:1 - 0.0, ylab="How often do you use software release notes?")
#points(1,mean(data$RNusage), col="red", pch=16)
dev.off() 

#release note creation
pdf(paste(WORKINGDIR,"ReleaseNoteCreation.pdf",sep="/"), width=3, height=5)
boxplot(data$RNcreation, col="gray", boxwex = 0.25, at = 1:1 - 0.0, ylab="Have you ever created release notes for a software system?")
#points(1,mean(data$RNcreation), col="red", pch=16)
dev.off() 

#contents of the release notes
pdf(paste(WORKINGDIR,"BoxPlotContentIncluded.pdf",sep="/"), width=13, height=3)
boxplot(rn_experts_notNull$fixBugs, rn_experts_notNull$newFeatures, rn_experts_notNull$enhancedFeatures, rn_experts_notNull$newCC, rn_experts_notNull$modifiedCC, rn_experts_notNull$deprecatedCC, rn_experts_notNull$deletedCC, rn_experts_notNull$replacedCC, rn_experts_notNull$visibilityCC, rn_experts_notNull$configFiles, rn_experts_notNull$testSuites, rn_experts_notNull$refactorings, rn_experts_notNull$architecture, rn_experts_notNull$documentation, rn_experts_notNull$licensesChanges, rn_experts_notNull$librariesUp, rn_experts_notNull$knownIssues, col="gray", boxwex = 0.25, at = 1:17 - 0.0, names = c("Fixed\nbugs","New\nfeatures", "Enhanced\nfeatures", "New code\ncomp.", "Modified\ncode comp.", "Deprecated\ncode comp.", "Deleted\ncode comp.", "Replaced\ncode comp.", "Changes vis.\n code comp.", "Changes to\nconf. files", "Changes to\ntest suites", "Refactoring\noperations", "Architectural\nchanges", "Changes to\ndocum.", "Changes to\nlicenses", "Libraries\nUpgrades", "Known\nIssues"), xlab="What kind of content do you include in release notes?", cex.axis = 0.62)
dev.off()

#time needed to create release notes
pdf(paste(WORKINGDIR,"TimeNeeded.pdf",sep="/"), width=3, height=5)
boxplot(rn_experts$RNcreationTime, col="gray", boxwex = 0.25, at = 1:1 - 0.0, ylab="How much time does it take to create a release note?")
#points(1,mean(data$RNcreation), col="red", pch=16)
dev.off()

#difficulty in creating release notes
pdf(paste(WORKINGDIR,"Difficulty.pdf",sep="/"), width=3, height=5)
boxplot(rn_experts$RNcreationDiff, col="gray", boxwex = 0.25, at = 1:1 - 0.0, ylab="How difficult is it to create a release note?")
#points(1,mean(data$RNcreation), col="red", pch=16)
dev.off()

#Apache Commons Collections results
pdf(paste(WORKINGDIR,"CommonsCollections.pdf",sep="/"), width=12, height=3)
boxplot(data_commons_not_null$O_AC_majorChanges, data_commons_not_null$O_AC_diffCatClassMeth, data_commons_not_null$O_AC_newMethUtilsCat, data_commons_not_null$O_AC_author, data_commons_not_null$A_AC_improvements, data_commons_not_null$A_AC_deletedCC, data_commons_not_null$A_AC_deprecatedCC, data_commons_not_null$A_AC_fineGrainedChanges, data_commons_not_null$A_AC_refactoring, data_commons_not_null$A_AC_issueTrackLink, data_commons_not_null$A_AC_nItems, col="gray", boxwex = 0.25, at = 1:11 - 0.0, names = c("Major changes\nsince 3.2.1","Separated new\nclasses and new meth.", "New meth.\nin Utils", "Author of\nthe change", "Improvements\n", "Deleted\ncode comp.", "Deprecated\ncode comp.", "Fine grained\nchanges", "Refactoring\noperations", "Issue tracker\nlinks", "#Items in\neach category"), xlab="How important are the following contents?", cex.axis = 0.62)
dev.off()

#Apache Lucene results
pdf(paste(WORKINGDIR,"Lucene.pdf",sep="/"), width=12, height=3)
boxplot(data_lucene_not_null$O_L_optimizations, data_lucene_not_null$O_L_backCompatibility, data_lucene_not_null$O_L_apiChanges, data_lucene_not_null$O_L_build, data_lucene_not_null$O_L_documentation, data_lucene_not_null$O_L_author, data_lucene_not_null$A_L_improvements, data_lucene_not_null$A_L_deletedCC, data_lucene_not_null$A_L_deprecatedCC, data_lucene_not_null$A_L_addedCC, data_lucene_not_null$A_L_modifiedCC, data_lucene_not_null$A_L_CCvisibility, data_lucene_not_null$A_L_fineGrainedChanges, data_lucene_not_null$A_L_refactoring, data_lucene_not_null$A_L_knownIssues, col="gray", boxwex = 0.25, at = 1:15 - 0.0, names = c("Optimizations\n","Backward\ncompatibility", "API\nchanges", "Build\n", "Changes to\ndocum.", "Author of\nthe change", "Improvements\n", "Deleted\ncode comp.", "Deprecated\ncode comp.", "Added\ncode comp.", "Modified\ncode comp.", "Changes vis.\n code comp.", "Fine grained\nchanges", "Refactoring\noperations", "Known\nissues"), cex.axis = 0.62)
dev.off()
