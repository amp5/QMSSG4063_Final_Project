## read in the issues data 
issues <- read.csv("issues.csv")

library(dplyr)
grouped <- group_by(issues, X)
whole_issues <- summarise(grouped, 
                          Islamic.Issue = sum(Islamic.Issue),
                          Syrian.Refugees = sum(Syrian.Refugees),
                          Death.Penalty = sum(Death.Penalty),
                          Trans.Pacific.Partnership = sum(Trans.Pacific.Partnership),
                          Gun.Control = sum(Gun.Control),
                          Syria.no.fly.zone = sum(Syria.no.fly.zone),
                          Health.Care = sum(Health.Care),
                          Israel = sum(Israel),
                          Climate.Change = sum(Climate.Change),
                          Immigration = sum(Immigration),
                          Same.sex.Marriage = sum(Same.sex.Marriage),
                          NSA = sum(NSA),
                          Government.Spending = sum(Government.Spending),
                          Lobbying = sum(Lobbying),
                          China = sum(China))
write.csv(whole_issues, file = "issues_sum.csv")
