library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(maps)
library(ggmap)
library(ggplot2)
# 위키피디아 페이지 URL
url <- "https://ko.wikipedia.org/wiki/대한민국의_인구순_도시_목록"
# 페이지 읽기
page <- read_html(url)
# 테이블 전체 가져오기
tables <- page %>% html_table(fill = TRUE)
# 첫 번째 테이블이 인구순 도시 목록임
domestic_cities <- tables[[1]]
# 행정구역 열 기준으로 시도와 도시 분리
domestic_clean <- domestic_cities %>%
  select(행정구역 = 2) %>%  # 열 번호 2가 행정구역
  filter(!is.na(행정구역)) %>%
  mutate(
    시도 = str_extract(행정구역, "^[^\\s]+도|^[^\\s]+시|^[^\\s]+광역시|^[^\\s]+특별시|^[^\\s]+특별자치시"),
    도시 = str_remove(행정구역, "^[^\\s]+도\\s*|^[^\\s]+시\\s*|^[^\\s]+광역시\\s*|^[^\\s]+특별시\\s*|^[^\\s]+특별자치시\\s*") %>%
      str_remove("(특례시|특별시|광역시|특별자치시|시|군|구)$") %>%
      str_trim()
  ) %>%
  select(시도, 도시) %>%
  distinct()

domestic_clean$도시[c(1,2,3,4,5,6,8,34)] <- c('서울','부산','인천','대구',
                                            '대전','광주','울산','세종')

domestic_clean <- domestic_clean[-nrow(domestic_clean), ]
head(domestic_clean)
# 1. 도시 목록
cities <- unique(domestic_clean$도시)

# 2. 중의적인 도시 이름들
ambiguous_cities <- c("예산", "동해", "남해", "고성", "동해", "공주", "고양")

# 3. 전체 텍스트 결합
df_books <- read.csv("yes24_books_cleaned.csv")
full_text <- paste(df_books$title, df_books$description,
                   df_books$intro, df_books$index, sep = " ")
# 4. 도시 매칭 (문맥 필터 포함)
city_mentions <- map_dfr(seq_along(full_text), function(i) {
  matches <- cities[str_detect(full_text[i], paste0("\\b", cities))]
  
  # ambiguous 도시 필터
  filtered <- map_chr(matches, function(city) {
    if (!(city %in% ambiguous_cities)) {
      return(city)
    }
    
    context <- str_extract_all(full_text[i], paste0(".{0,10}", city, ".{0,10}"))[[1]]
    
    if (any(str_detect(context,
                       "시|군|여행|지역|풍경|전라|전북|전남|
                        충청|충북|충남|경상|경북|경남|강원|경기"))) {
      return(city)
    } else {
      return(NA_character_)
    }
  })
  
  # 중복 제거 후 반환
  tibble(book_id = i, city = na.omit(unique(filtered)))
})
city_counts <- city_mentions %>%
  count(city, name = "count") %>%
  arrange(desc(count))
# 결과 확인
head(city_counts, 20)
#5
data(world.cities)
a <- world.cities[world.cities$country.etc %in% "Korea South",]
target_cities <- c(
  "seoul", "cheju", "inchon", "pusan", "kyongju", "chinju",
  "kangnung", "andong", "yonggwang", "sunchon", "chonju",
  "taegu", "chunchon", "tongyong", "kwangju",
  "taebaek", "yosu", "wonju", "namwon"
)
korea_selected <- subset(world.cities,
                         country.etc == "Korea South" &
                           tolower(name) %in% target_cities) %>%
  filter(!(name == "Kwangju" & pop < 200000)) %>%
  select(-pop)
new_row <- data.frame(name = "Pyeongchang",country.etc = "Korea South",
                      lat = 37.37,long = 128.39,capital = 0)
korea_selected <- bind_rows(korea_selected, new_row)
new_column <- c(443, 1257, 534, 428, 415,
                1140, 463, 408, 651, 379,
                761, 1617, 437, 393, 418,
                409, 389, 440, 389, 399)
korea_selected <- bind_cols(korea_selected, new_column)
colnames(korea_selected) <- c("name", "country.etc", "lat", "long", "capital", "book")

korea <- map_data("world", region = "South Korea")
ggplot() + 
  geom_polygon(data=korea, 
               aes(x=long, y=lat, group=group),
               fill="white",
               colour="black")+
  geom_point(data=korea_selected, aes(x=long, y=lat, size = book),
             shape = 16, color = "green", alpha = 0.4)+
  scale_size_area(max_size=30)+ 
  geom_text(data=korea_selected, aes(x=long+0.2, y=lat+0.2, label= paste(name, book, sep = "\n")))

            