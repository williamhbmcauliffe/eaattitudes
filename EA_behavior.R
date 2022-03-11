library(lavaan)
library(MIIVsem)
Study2 <- as.data.frame(read_sav("Study2.sav"))
#donation task 3 = the amount donated to the effective charity
Study2$donation_std<-scale(Study2$donation_task_3) # standardize to reduce variance. use this item to represent effectiveness in donating.
Study2$splitting<-as.factor(Study2$splitting)
Study2$margin_thinking<-as.factor(Study2$margin_thinking)
Study2$expected_value<-as.factor(Study2$expected_value_score)


### Expansive Altruism Scale ------------------------------------------------


###ea_attitudes_scale_1
#As long as my and my family's basic material needs are covered, I want to use a
#significant amount of my resources (e.g., money or time) to improve the world.

###ea_attitudes_scale_2
#I am willing to make significant sacrifices for people in need that I don't know and will
#never meet.

###ea_attitudes_scale_3
#People in wealthy countries should donate a substantial proportion of their income to 
#make the world a better place.

###ea_attitudes_scale_4
# I would make a career change if it meant that I could improve the lives of people in need.

###ea_attitudes_scale_5
# We should put a lot of emphasis on the well-being of people who will live thousands of
# years from now, even relative to the well-being of people who live today.

###ea_attitudes_scale_7
# From a moral perspective, the suffering of all beings matters roughly the same, no
# matter to what species they belong to.


# effectiveness focus scale -----------------------------------------------

###effectiveness_scale_19
#it would be wrong to do something that only does some amount of good if there is an
#alternative course of action that would do much more good.

###effectiveness_scale_4
#it would be the right choice to refrain from helping one person if that makes it possible to
#help a larger number of people.

###effectiveness_scale_5
#helping one person is less valuable than helping two people to the same extent.

###effectiveness_scale_6
#you should follow evidence and reason to do what is most effective, even if you
#emotionally prefer another option.

###effectiveness_scale_15
#the most important consideration is effectiveness - choosing the option that does the
#most good per resource invested.

###effectiveness_scale_18
#you should usually help a large group of people over a smaller group, even if it seems
#unfair.

# SEM ---------------------------------------------------------------------

# just the scales
twofactor= '
expansive =~ ea_attitudes_scale_1	+ ea_attitudes_scale_2 +	ea_attitudes_scale_3 +	ea_attitudes_scale_4 +	ea_attitudes_scale_5 +	ea_attitudes_scale_7
effective =~ effectiveness_scale_19 +	effectiveness_scale_4	+ effectiveness_scale_5	+ effectiveness_scale_6	+ effectiveness_scale_15	+ effectiveness_scale_18	
'
fit_two <- sem(twofactor, data = Study2, estimator = "MLR")

summary(fit_two, standardized=T, fit.measures=T, rsquare=T)
modificationindices(fit_two, sort. = TRUE)
lavResiduals(fit_two)
out <- capture.output(summary(fit_two, standardized=T, fit.measures=T, rsquare=T))
cat("two_factor", out, file="two_factor.txt", sep="\n", append=TRUE)
#, sarg.adjust = "holm"
###MIIV SEM
twomiiv<-miive(twofactor, Study2)
charmiiv<-miive(charity_outcome, Study2,ordered = c("splitting","margin_thinking",
                                         "expected_value"))
summary(twomiiv)
out_twomiiv <- capture.output(summary(twomiiv))
out_twomiiv
cat("out_twomiiv", out_twomiiv, file="out_twomiiv.txt", sep="\n", append=TRUE)


##charity outcomes as latent
charity_outcome= '
expansive =~ ea_attitudes_scale_1	+ ea_attitudes_scale_2 +	ea_attitudes_scale_3 +	ea_attitudes_scale_4 +	ea_attitudes_scale_5 +	ea_attitudes_scale_7
effective =~ effectiveness_scale_19 +	effectiveness_scale_4	+ effectiveness_scale_5	+ effectiveness_scale_6	+ effectiveness_scale_15	+ effectiveness_scale_18	
behavior ~ expansive + effective
behavior =~ donation_std +identifiable_victim+ deprioritization_ave+  ineffective_donating+ equity_vs_effective + expected_value+splitting + margin_thinking
'
fit <- sem(charity_outcome, data = Study2,
           ordered = c("splitting","margin_thinking",
                       "expected_value"))

summary(fit, standardized=T, fit.measures=T, rsquare=T)

out <- capture.output(summary(fit, standardized=T, fit.measures=T, rsquare=T))
cat("full_sem", out, file="full_sem.txt", sep="\n", append=TRUE)

varTable(fit)
summary(fit, data=Study2)
sortedmodindices <- capture.output(modificationindices(fit, sort. = TRUE))
cat("sortedmodindices", sortedmodindices, file="sortedmodindices.txt", sep="\n", append=TRUE)

table(Study2$change_career)
eainterest_outcome= '
expansive =~ ea_attitudes_scale_1	+ ea_attitudes_scale_2 +	ea_attitudes_scale_3 +	ea_attitudes_scale_4 +	ea_attitudes_scale_5 +	ea_attitudes_scale_7
effective =~ effectiveness_scale_19 +	effectiveness_scale_4	+ effectiveness_scale_5	+ effectiveness_scale_6	+ effectiveness_scale_15	+ effectiveness_scale_18	
behavior ~ expansive + effective
behavior =~ ea_agreement+ ea_interest +ea_newsletter+ ea_book+  pledge_give10+ change_career
'
fit_interest <- sem(eainterest_outcome, data = Study2,
           ordered = c("ea_newsletter","ea_book",
                       "pledge_give10", "change_career"))

summary(fit_interest, standardized=T, fit.measures=T, rsquare=T)
modificationindices(fit_interest, sort. = TRUE)
out_interest <- capture.output(summary(fit_interest, standardized=T, fit.measures=T, rsquare=T))
cat("eainterest", out_interest, file="eainterest.txt", sep="\n", append=TRUE)
sortedmods_interest <- capture.output(modificationindices(fit_interest, sort. = TRUE))
cat("sortedmods_interest", sortedmods_interest, file="sortedmods_interest.txt", sep="\n", append=TRUE)
