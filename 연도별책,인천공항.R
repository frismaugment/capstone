df_books <- read.csv("yes24_books_cleaned.csv")
head(df_books$pub_date,10)

df_books_date <- df_books %>%
  mutate(pub_year = str_extract(pub_date, "^[0-9]{4}")) %>%
  mutate(pub_year = as.numeric(pub_year))

ggplot(df_books_date, aes(x = pub_year)) +
  geom_histogram(binwidth = 1, fill = "royalblue", color = "white") +
  labs(
    title = "책 발행년도 분포",
    x = "발행년도",
    y = "책 수"
  ) +
  theme_minimal()
#1. 1989년부터 해외여행 자유화 -> 1989년 발행된 즐거운 해외여행 가이드
#2. 꾸준히 우상향 -> 여행이 점차 일상적인 문화 활동
#3. 2020,2021,2022 책 발행의 급격한 감소 -> 코로나로 인한 여행제한 

library(readxl) #2003.4~ 2025.4
flight_data <- readxl::read_excel("C:/Users/jsj99/Documents/flight.xlsx")
flight_data <- flight_data[-c(1,2, nrow(flight_data)), ]
flight_data <- flight_data[, -c(1,3,5,6,8,9,10,11)]
colnames(flight_data) <- c("nation" , "plane", "passenger")

flight_data$plane <- as.numeric(flight_data$plane)
flight_data$passenger <- as.numeric(flight_data$passenger)

top20_plane <- flight_data %>%
  group_by(nation) %>%
  summarise(total_plane = sum(plane, na.rm = TRUE)) %>%
  arrange(desc(total_plane)) %>%
  slice_head(n = 20)
top20_passenger <- flight_data %>%
  group_by(nation) %>%
  summarise(total_passenger = sum(passenger, na.rm = TRUE)) %>%
  arrange(desc(total_passenger)) %>%
  slice_head(n = 20)

ggplot(top20_plane, aes(x = reorder(nation, total_plane), y = total_plane)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(total_plane)),  # 숫자 형식 개선 (1,000 단위 쉼표)
            hjust = -0.1, size = 3.5) +               # 막대 바깥 오른쪽에 표시
  coord_flip() +
  labs(title = "국가별 운항편 수", x = "국가", y = "운항편 수") +
  theme_minimal()

ggplot(top20_passenger, aes(x = reorder(nation, total_passenger), y = total_passenger)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = scales::comma(total_passenger)),  # 숫자 형식 개선 (1,000 단위 쉼표)
            hjust = -0.1, size = 3.5) +               # 막대 바깥 오른쪽에 표시
  coord_flip() +
  labs(title = "국가별 이용객 수", x = "국가", y = "이용객 수") +
  theme_minimal()
