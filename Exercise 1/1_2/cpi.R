library(ggplot2)
library(scales)

cpi <- read.csv("cpi.csv", sep = ";", header = TRUE)
cpi <- cpi[c("id_daty", "wartosc")]
cpi <- cpi[cpi$id_daty <= 2022, ]
print(cpi)

ggplot(cpi, aes(x=id_daty, y=wartosc)) + geom_line() + geom_point() +
  labs(title = "Wskaźnik cen towarów i usług konsumpcyjnych (CPI) w skali rocznej", x = "Rok", y = "CPI") +
  geom_hline(yintercept=100, linetype="dashed") + 
  scale_x_continuous(breaks = pretty_breaks())
