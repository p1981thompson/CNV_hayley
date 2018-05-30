

#################################################################################
#
# Test script for the CNV analysis
#
################################################################################

#Paul Thompson, created 09-05-2018

CNV_dat<-read.table('SLIC_df.txt',header=TRUE,stringsAsFactors=FALSE)

##################################################################################
#Loads required packages.

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(scales)
require(sandwich)
require(msm)

##############################################################################

#Function borrowed and adapted from the 'scales' package. This applies the censoring to the two potentially interesting variables.

censor_PT<-function (x, limit = 7, only.finite = TRUE) 
{
  force(range)
  finite <- if (only.finite) 
    is.finite(x)
  else TRUE
  x[finite & x > limit] <- limit+1
  x
}

CNV_dat$sum_pLi_C<-censor_PT(CNV_dat$sum_pLi,limit=7,only.finite = F)
#CNV_dat$total_Kbp_C<-censor_PT(CNV_dat$total_Kbp,limit=3000,only.finite = F)

#-----------------------------------------------------------------------------#

# Scatter plot with marginal histograms 

p1 <- ggplot(CNV_dat,aes(x=sum_pLi_C,y=total_Kbp,colour=factor(Group))) + geom_point() +
  scale_x_continuous(expand=c(0.02,0)) +
  scale_y_continuous(expand=c(0.02,0)) +
  theme_bw() +
  theme(legend.position="none",plot.margin=unit(c(0,0,0,0),"points"))

theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.margin = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0,"null"),
                               axis.ticks.margin = unit(0,"null"),
                               panel.border=element_rect(color=NA),...)

p2 <- ggplot(CNV_dat,aes(x=sum_pLi_C,colour=factor(Group),fill=factor(Group))) + 
  geom_density(alpha=0.5) + 
  scale_x_continuous(breaks=NULL,expand=c(0.02,0)) +
  scale_y_continuous(breaks=NULL,expand=c(0.00,0)) +
  theme_bw() +
  theme0(plot.margin = unit(c(1,0,-0.48,2.2),"lines")) 

p3 <- ggplot(CNV_dat,aes(x=total_Kbp,colour=factor(Group),fill=factor(Group))) + 
  geom_density(alpha=0.5) + 
  coord_flip()  + 
  scale_x_continuous(labels = NULL,breaks=NULL,expand=c(0.02,0)) +
  scale_y_continuous(labels = NULL,breaks=NULL,expand=c(0.00,0)) +
  theme_bw() +
  theme0(plot.margin = unit(c(0,1,1.2,-0.48),"lines"))

mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p2,ncol=2,widths=c(3,1)),
             arrangeGrob(p1,p3,ncol=2,widths=c(3,1)),
             heights=c(1,3))

##############################################################################
#Merge the phenotype data to the CNV data

CNV_Mdata<-merge(CNV_dat,SLI_pheno_dat,by="id")

##############################################################################

#Histogram of the phenotype of interest. Should be roughly normal with a longer tail and truncated at zero.

gplot(SLI_pheno_dat,aes(x=global_neurodev_rating)) + geom_histogram(aes(y =..density..), 
                                                                    col="darkblue", 
                                                                    fill="blue", 
                                                                    alpha = .2) + 
  geom_density(alpha=0.5)+theme_bw()

##############################################################################

#Multiple testing: Wilcoxon sum rank test with Bonferroni correction.

BF_correct_alpha<-0.05/3

w1<-wilcox.test(total_cnvs~Group,data=CNV_dat,conf.int=TRUE)
print(list(p.value=w1$p.value,corrected.alpha=BF_correct_alpha,res=w1$p.value<=BF_correct_alpha))

w2<-wilcox.test(total_Kbp~Group,data=CNV_dat,conf.int=TRUE)
print(list(p.value=w2$p.value,corrected.alpha=BF_correct_alpha,res=w2$p.value<=BF_correct_alpha))

w3<-wilcox.test(sum_pLi_C~Group,data=CNV_dat,conf.int=TRUE)
print(list(p.value=w3$p.value,corrected.alpha=BF_correct_alpha,res=w3$p.value<=BF_correct_alpha))

#-----------------------------------------------------------------------------#

#Poisson regression as the dependent variable is a count. This will naturally allow for the lower bound of zero.

#reference for analysis: https://stats.idre.ucla.edu/r/dae/poisson-regression/

summary(m1 <- glm(global_neurodev_rating ~ sum_pLi_C + Group, family="poisson", data=CNV_Mdata))

#-----------------------------------------------------------------------------#
#check the assumption that mean and variance ar roughly equal. If not, we can use robust estimates. I think thi will make little difference in our case but included for completeness.

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est
#-----------------------------------------------------------------------------#

#test the effect of dropping group. We hope that this is significant!
m2 <- update(m1, . ~ . - Group)
## test model differences with chi square test
anova(m2, m1, test="Chisq")

