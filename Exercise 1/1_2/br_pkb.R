library(ggplot2)
library(scales)

br <- read.csv("br_pkb.csv", sep = ";", header = TRUE)
br <- br[c("id_daty", "wartosc")]
print(br)

ggplot(br, aes(x = id_daty, y = wartosc)) +
  geom_line() +
  geom_point() +
  labs(title = "Nakłady na Badania i Rozwój w relacji do PKB", x = "Rok", y = "[%]")
