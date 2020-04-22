---
title: "Kelberman_Capstone"
author: "Michael Kelberman"
date: "4/22/2020"
output: GitHub Page (Although my theme isn't working)
---

## Initializing Library

```{r setup, include=FALSE}
library(tidyverse)
library(ez)

```

## 1: Background

**One commmon feature of post-traumatic stress disorder (PTSD) is abnormal fear generalization (fear response to non-threatening stimuli). However, the two FDA approved drugs to treat PTSD are selective serotonin reuptake inhibitors that are largely thought to target mood symptoms rather than fear generalization. Moreover, specific populations present with PTSD that is resistant to first-line treatments, necessitating the development of new therapeutics. Specifically, targeting thalamic and subthalamic brain regions is of interest because these areas gate behavioral output based on incoming sensory cues.**

**An emerging therapeutic approach for Alzheimer's disease is 40 Hz sensory stimulation, which improves cognitive deficits in rodent models of the disease. Thalamic and subthalamic regions that mediate fear responses receive input from sensory systems that are responsive to 40 Hz sensory stimulation. 40 Hz rhythms are also disrupted in many brain regions in PTSD patients. Overall, this evidence indicates that entrainment with 40 Hz sensory stimulation could be efficacious for treating abnormal fear generalization in PTSD.**

**I propose to pre-treat 4 groups of mice for 1 hour per day for 5 days prior to fear conditioning with 40 Hz visual stimulation (treatment), random visual stimulation (control), 8 Hz visual stimulation (control), and darkness (control).**

## 2: Question

**Even though 40 Hz sensory stimulation can improve cognition in Alzheimer's disease models and holds promise for targeting and correcting abnormal 40 Hz rhythms in PTSD, this has not been directly investigated.**

## 3: Hypothesis

**If mice are pre-treated with 40 Hz visual stimulation, then they will exhibit lower levels of freezing to an unconditioned tone (ie. lower fear generalization)**

## 4: Variable Characteristics

**The independent variable here is treatment group which has 4 levels (40 Hz, random, 8 Hz, and darkness) and is a discrete sorted variable. Darkness is a true, negative control where there is no intervention. 8 Hz and random stimulation represent two alternative stimulation parameters that represent positive controls (alternative stimulation patterns but are not supposed to be therapeutic but control for light stimulation itself). The dependent variable is percent freezing and this is a continous measured variable.**

## 5: Statistical Hypothesis

_ANOVA: Primary Endpoint_

**$H_0: MS<sub>treatment</sub> = MS<sub>residual</sub>; The null hypothesis is that the variance associated with the four treatments is equal to the residual variance**

**$H_1: MS<sub>treatment</sub> != MS<sub>residual</sub>; The alternative hypothesis is that the variance associated with the four treatments is not equal to the residual variance.**

_Post-Hoc: Secondary Endpoint_

**$H_0: mu<sub>40 Hz</sub> >= mu<sub>dark</sub>; The null hypothesis is that the average freezing levels in 40 Hz treated mice is greater than or equal to the average freezing levels of dark treated animals.**

**$H_1: mu<sub>40 Hz</sub> < mu<sub>dark</sub>; The alternative hypothesis is that the average freezing levels in 40 Hz treated mice is less than the average freezing levels of dark treated animals.**

## 6: Statistical Test

**Here, I will use a one-way completely random ANOVA. This is a one-way design because the independent variable (treatment) has more than two levels but there is only one independent variable. I will also perform a pairwise post-hoc test with a Bonferroni correction to compare each treatment to control (ie. compare 40 Hz, 8 Hz, and random stimulation to dark treated mice). Since mice aren't receiving multiple treatments or being tested more than once, the design is not paired. Furthermore, the fear responses from each mouse are not intrinsically linked and I will use an outbred mouse strain for testing, meaning that each individual mouse will represent an independent replicate. This further justifies the use of a completely randomized design. Finally, the use of non-parametric tests are not warranted here because each replicate is independent of all others, a random process will be used to assign mice to treatments, the variances of the groups are expected to be approximately equal, and the residual distributions are expected to be continuous random normal. Other post-hoc tests are able to be used, but I decided to use the Bonferroni test because it is a stringent test. I feel as though a stringent test is warrented here since I am investigating a potential therapeutic for PTSD.**

## 7: Experimental Procedures

**Mice will be trained on a traditional fear conditioning paradigm. Briefly, mice will be trained to associate a high intensity foot-shock with a specific tone (conditioned stimulus). On test days, a new tone that was not associated with the foot-shock (unconditioned stimulus/tone) will be presented and percent freezing levels will be measured. Mice that fear generalize will normally freeze approximately 65% to the unconditioned tone. Interventions targeting fear behavior in preclinical models aim to lower freezing levels to the unconditioned tone, which would be indicative of fear discrimination (which is what we want to see from these mice receiving treatment).**

**The indepdent replicate here will be the individual mice. This is because I will use an outbred strain of mouse which will make their fear responses to treatment completely independent. The use of outbred strains means that each mouse is not an exact immortalized clone and can be treated as indpendent replicates. Each indepdent replicate will be randomly assigned to 1 of the 4 treatment groups and all mice will be tested at the same age. The primary end-point will be a significant effect of treatment of freezing levels. The secondary end-point will be a reduction in freezing levels in mice treated with 40 Hz stimulation compared to control (dark treated mice). This is an exploratory study for PTSD treatment. Since nothing like this has been attempted before and it is not yet being tested in clinical trials, I will use traditional type1 and type 2 thresholds. My type1 threshold will be 5% and will operate on 80% power meaning a type2 threshold of 20%. However, I will use a stringent post-hoc corrected test, the Bonferroni test.**

## 8: Simulated Results

```{r}
##Simulating Results

dark = 65 #Dark Avg
eight = .9 #Expected Fold Change, 8 Hz Avg
ran = 1.0 #Expected Fold Change, Random Avg
forty = 0.35 #Expected Fold Change, 40 Hz Avg

sd = 15 #Expected Standard Deviation
n = 4 # number of independent replicates per group
sims = 100 #number of Monte Carlo simulations to run. 

CRdataMaker <- function(n, dark, eight, ran, forty, sd) { 
  
  
  a1 <- rnorm(n, dark, sd) #Dark
  a2 <- rnorm(n, (dark*eight), sd) #8 Hz
  a3 <- rnorm(n, (dark*ran), sd) #Random
  a4 <- rnorm(n, (dark*forty), sd) #40 Hz
    
    Percent_Freezing <- c(a1, a2, a3, a4)
    Treatment <- c(rep(c("Dark", "8 Hz", "Random", "40 Hz"), each = n))
    ID <- as.factor(c(1:length(Treatment)))
    df <-data.frame(ID, Treatment, Percent_Freezing)
    }

dat <- CRdataMaker(n,dark,eight,ran,forty,sd)


ggplot(dat, aes(Treatment, Percent_Freezing, color=Treatment))+
  geom_jitter(width=0.1,size = 4, alpha=0.5)+
      stat_summary(
     fun.data=mean_sdl, 
     fun.args=list(mult=1), 
     geom= "crossbar", 
     width=0.2,
     color="Black")+
    labs(y="Percent Freezing", 
       x="Treatment"
       )+
    ggtitle("Predicted Percent Freezing to an 
            Unconditioned Tone by Treatment Group")
```
**Simulated percent freezing data to an unconditioned tone by treatment group. Means and standard deviations are based on previously reported literature on conditioned fear paradigms. Normal mice trained on these regimens fear generalize by freezing approximately 65% of the time to the unconditioned tone. Mice that discriminate only freeze about 20% of the time to the same tone. The standard deviation of freezing behaviors remains similar across groups, about 15%. I also predict a slightly lower level of freezing in the 8 Hz treated mice, because the stimulation parameters are still biologically relevant, whereas the random stimulation should have no effect on freezing levels. I also expect that the 40 Hz stimulation will have the most dramatic effect on reducing freezing levels. All of this is reflected in my CRdataMaker function above.**

## 9: Monte Carlo

```{r}
##ANOVA Power Calc
pval_ANOVA <- replicate(
  sims, {
 
    sample.df <- CRdataMaker(n,dark,eight,ran,forty,sd)
    
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = ID,
            dv = Percent_Freezing,
            between = Treatment,
            type = 2
            )
  
  pval_ANOVA <- sim.ezaov$ANOVA[1,5]
    
    }
  )

pwr.pct <- sum(pval_ANOVA<0.05)/sims*100
paste(pwr.pct, sep="", "% power for one-way CR ANOVA with n=4")


##T-Test Power Calc
t.pwr <- function(n){
  
  p.values <- c()
  i <- 1
  repeat{
    x=rnorm(n, dark, sd); 
    y=rnorm(n, (dark*forty), sd);
    
    alpha=0.05/3 #Bonferroni corrected post-hoc alpha
    
    p <- t.test(x, y, 
                paired=F, 
                alternative="greater", 
                var.equal=T,
                conf.level=1-alpha)$p.value
    p.values[i] <- p
    
    if (i==sims) break
    i = i+1
    
    pwr <- length(which(p.values<alpha))/sims
  }
  return(pwr)
  
}

paste(t.pwr(4), sep="", "% power for t-test w/Bonferroni correction (dark vs. 40 Hz only) where n=4")


```
**A sample size of 4 independent replicates per treatment group (16 animals total) is necessary for approximately 90% power (3 indepedent replicates per group only provides 70-75% power). 4 indepdent replicates per group will provide above 80% power for post-hoc t-tests with Bonferroni correction. Thus, a total sample size of 16 animals will be necessary for this experimental procedure.**


## 10: Knit and Submit or GitHub it
