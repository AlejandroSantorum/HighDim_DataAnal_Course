library(dagdata)
data(admissions)

print( admissions )

index = which(admissions$Gender==1) #Gender=1 are men, Gender=0 are women
m_accepted = sum(admissions$Number[index] * admissions$Percent[index]/100)
m_applied = sum(admissions$Number[index])
men_accepted_proportion = m_accepted/m_applied
men_accepted_proportion

# Q1: What is the proportion of women that were accepted?
index = which(admissions$Gender==0) #Gender=1 are men, Gender=0 are women
w_accepted = sum(admissions$Number[index] * admissions$Percent[index]/100)
w_applied = sum(admissions$Number[index])
women_accepted_proportion = w_accepted/w_applied
women_accepted_proportion

# Q2: If you perform a chi-square independence test, what is the p-value?
M <- as.table(rbind(c(m_accepted, m_applied-m_accepted), c(w_accepted, w_applied-w_accepted)))
(Xsq <- chisq.test(M))

# Q3: Which is the hardest major?
major = admissions[1:6,1]
men = admissions[1:6,]
women =admissions[7:12,]
H = (men$Number*men$Percent/100 + women$Number*women$Percent/100) / (men$Number+women$Number)
major[which.min(H)]

# Q4: What proportion of students is admitted for the hardest major from Confounding Exercises #3?
admissions
((0.06*373)+(0.07*341))/(373+341)

# Q5: For men, what is the correlation between the number of applications across majors and H?
cor(men$Number, H)

# Q6: For women, what is the correlation between the number of applications across majors and H?
cor(women$Number, H)










