#
#original file <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")
library(tidyverse)
library(extrafont)
library(gridExtra)
library(directlabels)
library(ggrepel)

#more details : https://github.com/innocenter/tidytuesday-1/tree/master/data/2019/2019-11-26


#loading data

loans <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")
glimpse(loans)

# 

#renaming dates


loans %>% 
  mutate(year = case_when(year == 15 ~ 2015, 
                          year == 16 ~ 2016,
                          year == 17 ~ 2017,
                          year == 18 ~ 2018)) -> loans




#summary of loans over the years
loans %>% 
  group_by(year) %>% 
  summarise(Total.starting = sum(starting, na.rm=TRUE)/10^9, 
            Total.added = sum(added, na.rm = TRUE)/10^9,
            Total.repaid = sum(total, na.rm=TRUE)/10^9,
            consolidation.pay = sum(consolidation, na.rm = TRUE)/10^9,
            rehabilitation.pay = sum(rehabilitation, na.rm = TRUE)/10^9,
            voluntary.pay = sum(voluntary_payments, na.rm=TRUE)/10^9,
            wagegarnishment.pay =sum(wage_garnishments, na.rm = TRUE)/10^9 ) %>% 
  arrange(desc(year))  %>% 
  ungroup()-> payment.summary

#how to pay for loans preferred payment method
#voluntary, wage garnishment, 
#in need of loan... loan rehabilitation, consolidation

#repaid = sum(Total.consolida… Total.rehabilition..Total.voluntary.pay.. Total.wage.garnishment)

#payment.summary %>% 
 # unite(qtr.year, c(quarter, year), sep="-", remove = FALSE) -> payment.summary


#plot 1 without log transformation
notrans <- payment.summary %>% 
  pivot_longer(starts_with("Total"), names_to = "status", values_to = "totals")  %>% 
  mutate(label = if_else(year == max(year), as.character(status), NA_character_)) %>% 
  ggplot(mapping = aes(year, totals, col=status)) +
    geom_line(show.legend = FALSE)+
    scale_color_discrete(guide = FALSE) +
    theme_classic() +
  labs(title= "Loan Amounts per Year", subtitle = "Without log transformation",
       x= "Year", y="Amount in USD (billion)", caption = "By am_innocenter for #TidyTuesdy 26.11.2019 \n Data source : https://studentaid.ed.gov/sa/about/data-center/student/default")+
  geom_label_repel(aes(label=label),nudge_x = 1, na.rm = TRUE )+
  theme(panel.background = element_rect(fill="gray8")) + #, colour = "#6D9EC1")) +
  theme(text = element_text(family = "Impact", size = 12),
        plot.caption = element_text( size=10, color = "grey40"),
        plot.title =element_text(size = 14, face="bold") ) 

#with log trans
logtrans  <-payment.summary %>% 
  pivot_longer(starts_with("Total"), names_to = "status", values_to = "totals")  %>% 
  mutate(label = if_else(year == max(year), as.character(status), NA_character_)) %>% 
  ggplot(mapping = aes(year, totals, col=status)) +
    geom_line(show.legend = FALSE)+
    scale_color_discrete(guide = FALSE) +
    scale_y_log10() +
    theme_classic() +
    labs(title= "Loan Amounts per Year", subtitle = "With log transformation",
       caption = "By am_innocenter for #TidyTuesdy 26.11.2019 \n Data source : https://studentaid.ed.gov/sa/about/data-center/student/default", 
       x= "Year", y="Amount in USD (billion)"
    )+
    geom_label_repel(aes(label=label),nudge_x = 1, na.rm = TRUE )+
    theme(panel.background = element_rect(fill="gray8")) + #, colour = "#6D9EC1")) +
    theme(text = element_text(family = "Impact", size = 12),
      plot.caption = element_text( size=10, color = "grey40"),
        plot.title =element_text(size = 14, face="bold") ) 

grid.arrange(notrans, logtrans, ncol=2)



#payment methods preferred/seen mostly


paynotrans <- payment.summary %>% 
  pivot_longer(ends_with("pay"), names_to = "Method", values_to = "Payment")   %>% 
  mutate(label = if_else(year == max(year), as.character(Method), NA_character_)) %>% 
  ggplot(mapping = aes(year, Payment, col=Method)) +
  geom_line(show.legend = FALSE) +
  scale_color_discrete(guide = FALSE) +
  theme_classic() +
  labs(title= "Payment Methods & Amount Paid per Year", subtitle = "Without log transformation",
       x= "Year", y="Amount in USD (billion)", caption = "By am_innocenter for #TidyTuesdy 26.11.2019 \n Data source : https://studentaid.ed.gov/sa/about/data-center/student/default")+
  geom_label_repel(aes(label=label),nudge_x = 2, na.rm = TRUE )+
  theme(panel.background = element_rect(fill="gray8")) + #, colour = "#6D9EC1")) +
  theme(text = element_text(family = "Impact", size = 12),
        plot.caption = element_text( size=10, color = "grey40"),
        plot.title =element_text(size = 14, face="bold") ) 


paylogtrans <- payment.summary %>% 
  pivot_longer(ends_with("pay"), names_to = "Method", values_to = "Payment")   %>% 
  mutate(label = if_else(year == max(year), as.character(Method), NA_character_)) %>% 
  ggplot(mapping = aes(year, Payment, col=Method)) +
  geom_line(show.legend = FALSE) +
  scale_color_discrete(guide = FALSE) +
  scale_y_log10() +
  theme_classic() +
  labs(title= "Payment Methods & Amount Paid per Year", subtitle = "With log transformation",
       x= "Year", y="Amount in USD (billion)", caption = "By am_innocenter for #TidyTuesdy 26.11.2019 \n Data source : https://studentaid.ed.gov/sa/about/data-center/student/default")+
  geom_label_repel(aes(label=label),nudge_x = 0.4, na.rm = TRUE )+
  theme(panel.background = element_rect(fill="gray8")) + #, colour = "#6D9EC1")) +
  theme(text = element_text(family = "Impact", size = 12),
        plot.caption = element_text( size=10, color = "grey40"),
        plot.title =element_text(size = 14, face="bold") ) 

grid.arrange(paynotrans, paylogtrans, ncol=2)


#Names of agency loaning much and not receiving back

loans %>% 
  group_by(agency_name) %>% 
  summarise(Total.starting = sum(starting, na.rm=TRUE)/10^9, 
            Total.added = sum(added, na.rm = TRUE)/10^9,
            Total.repaid = sum(total, na.rm=TRUE)/10^9) %>% 
  arrange(desc(Total.starting)) %>% 
  filter(Total.starting > 30) %>% 
  ungroup() %>% 
  kableExtra::kable() %>% kableExtra::kable_styling()

