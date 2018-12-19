options(stringsAsFactors = FALSE)
library(tidyverse)
library(lubridate)

# Internet user Data
read_csv("Major_XR_3/Data/InternetUsers.csv") %>% 
  # Select only Dec and June
  filter(Month == "December") %>% 
  # Enusre Year is malleable
  mutate(Year = as.character(Year)) %>% 
  # new data var
  mutate(monyr = ymd(paste(Year, Month, "1", sep ="-"))) %>% 
  # to tbl
  as.tibble() ->> internetusers

# Company product launches
read_csv("Major_XR_3/Data/Companies.csv") %>% 
  # Change the name of cols
  set_names("Product", "Company", "Event", "Date", "y") %>% 
  # Ensure date is a date
  mutate(Date = ymd(Date)) %>% 
  filter(Company != "Amazon") %>% 
  as.tibble() ->> CompDa

# data frame
internetusers %>% 
  # pass to ggplot function
  ggplot(aes(x=monyr, y = NumUsers_Millions))+
  # Vertical lines for companies
  geom_label(data = CompDa,
            aes(x =Date, y = y, 
                label = Event, 
                fill = Company,
                hjust = "right"),colour = "#ffffff",
            show.legend = FALSE)+
  
  geom_point(data = CompDa,
             aes(x = Date, y = y), colour = "grey10")+
  
  # Line for internet users
  geom_line(colour = "grey50")+
  # Points
  geom_point(shape = 21,
             colour = "grey50",
             fill = "grey10",
             # Size is the number of users in millions
             aes(size = NumUsers_Millions),
             # No legend for this
             show.legend = FALSE)+
  scale_x_date(date_labels = "%Y",
               breaks = "1 year")+
  theme_minimal()+
  theme(axis.text = element_text(colour = "grey10"),
        axis.text.x = element_text(angle = 90, colour = "grey10"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
        legend.text = element_text(colour = "grey10"),
        legend.title = element_text(colour = "grey10")) ->> Plot_Comp
