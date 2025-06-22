library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
# 나무위키 국가 목록 URL
url_2 <- "https://namu.wiki/w/%EA%B5%AD%EA%B0%80/%EB%AA%A9%EB%A1%9D"
# 페이지 읽기
page_2 <- read_html(url_2)
df_books <- read.csv("yes24_books_cleaned.csv")
full_text <- paste(df_books$title, df_books$description,
                   df_books$intro, df_books$index, sep = " ")
# 모든 테이블 추출
tables_2 <- page_2 %>% 
  html_elements("table") %>% 
  html_table(fill = TRUE)
# 앞 16개 테이블만 선택
tables_16 <- tables_2[1:16]
# 각 테이블에서 첫 행 제거, 앞 3개 열 추출
cleaned_tables <- tables_16 %>%
  map(~ .x[-1, 1:min(3, ncol(.x))]) %>%  # 첫 행 제거, 최대 3열 선택
  bind_rows()
# 열 이름 지정
colnames(cleaned_tables) <- c("국가명", "도시", "대륙")[1:ncol(cleaned_tables)]
# 결과 확인
head(cleaned_tables)
nation = c(rep("일본",28),
           rep("중국",35),
           rep("대만",3),rep("필리핀",4),rep("베트남",5),
           "태국", rep("인도네시아",2),"말레이시아",rep("카자흐스탄",2),
           "캄보디아","미얀마","인도","스리랑카","아랍에미리트",
           "튀르키예",rep("독일",2),
           "스페인","폴란드",rep("스위스",3),"포르투갈",rep("영국",2),
           rep("이탈리아",3),rep("프랑스",2),"러시아","오스트리아",
           rep("호주",3),rep("미국",15),rep("캐나다",4),"멕시코",
           rep("브라질",2),"칠레",
           "이집트",rep("남아프리카 공화국",2),"모로코")
city = c("오사카","나고야","후쿠오카","삿포로","오키나와","아사히카와",
         "센다이","구마모토","미야자키","나고야","하코다테","히로시마",
         "마츠야마","오이타","가고시마","사가","도쿠시마","요나고",
         "다카마쓰","아오모리","니가타","코마츠","오카야마","나가사키",
         "키타큐슈","타카마츠","미야코","이시가키",
         "상하이","광저우","옌지","창춘","하얼빈","다롄","톈진","칭다오",
         "옌청","난징","항저우","창사","시안","선전","청두","충칭","자무쓰",
         "스자좡","웨이하이","싼야","선양","정저우","장자제","지난","옌타이",
         "우시","쿤밍","하이커우","양저우","허페이","샤먼","우한","푸저우",
         "홍콩","마카오","타이베이","가오슝","타이중",
         "세부","클락","보홀","보라카이","호찌민","다낭","나트랑","푸꾸옥","하이퐁",
         "치앙마이","발리","바탐","코타키나발루","알마티","심켄트","씨엠립",
         "양곤","델리","콜롬보","두바이",
         "이스탄불","프랑크푸르트","뮌헨",
         "바르셀로나","브로츠와프","취리히","인터라켄","제네바",
         "포르투","에든버러","카디프",
         "밀라노","베네치아","나폴리","니스","마르세유","상트페테르부르크",
         "잘츠부르크",
         "시드니","멜버른","브리즈번","괌","사이판",
         "하와이","시애틀","샌프란시스코","로스앤젤레스","뉴욕",
         "댈러스-포트워스","라스베이거스","시카고","보스턴","애틀란타",
         "솔트레이크시티","디트로이트","미니애폴리스",
         "토론토","몬트리올","퀘벡","밴쿠버","몬테레이",
         "리우데자네이루","상파울루","파타고니아",
         "알렉산드리아","케이프타운","요하네스버그","카사블랑카")
continent = c(rep("아시아",86),
              rep("유럽",18),
              rep("오세아니아",5),
              rep("아메리카",21),
              rep("아프리카",4))
tour <- cleaned_tables %>%
  add_row(국가명 = nation, 도시 = city, 대륙 = continent)
head(tour,10)
#도시언급
library(stringr)
library(dplyr)
library(purrr)

# 앞에 단어 없을 때만 매칭하는 패턴 생성 함수
city_pattern <- function(city) {
  if (city == "포르투") {
    return("(?<![가-힣a-zA-Z0-9])포르투(?!갈)")
  } else if (city == "빈") {
    return("(?<![가-힣a-zA-Z0-9])빈(?![가-힣a-zA-Z0-9])")
  } else if (city == "산티아고") {
    # '산티아고 순례길'이나 '산티아고 데'는 제외
    return("(?<![가-힣a-zA-Z0-9])산티아고(?!\\s?(순례길|데))")
  } else {
    return(paste0("(?<![가-힣a-zA-Z0-9])", city))
  }
}


city_mentions <- tour %>%
  distinct(국가명, 도시) %>%
  mutate(
    pattern = map_chr(도시, city_pattern),
    언급_책수 = map_int(pattern, function(pat) {
      sum(str_detect(full_text, regex(pat)))
    })
  ) %>%
  select(국가명, 도시, 언급_책수)
top20_cities <- city_mentions %>%
  arrange(desc(언급_책수)) %>%
  slice_head(n = 24) %>%
  mutate(도시_표시 = paste0(도시, " (", 국가명, ")"))
top20_cities <- top20_cities[-c(1,12, 15, 19), ]
ggplot(top20_cities, aes(x = reorder(도시, 언급_책수), y = 언급_책수)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "도시별 언급된 책 수",
    x = "도시",
    y = "언급된 책 수"
  )# 1. 도시 이름과 언급 수
europe_city <- c("파리", "로마", "런던", "베네치아", "프라하", "바르셀로나", "빈", "마드리드", "나폴리", "밀라노",
          "베를린", "뮌헨", "이스탄불", "잘츠부르크", "암스테르담", "프랑크푸르트", "바티칸", "아테네", "취리히",
          "리스본", "모나코", "베른", "브뤼셀", "인터라켄", "포르투", "모스크바", "에든버러", "자그레브",
          "헬싱키", "제네바", "코펜하겐", "스톡홀름", "소피아", "오슬로", "바르샤바", "상트페테르부르크", "레이캬비크")
europe_mentions <- c(1095, 977, 700, 519, 484, 466, 420, 397, 368, 339,
              316, 291, 285, 278, 252, 251, 244, 242, 230,
              218, 214, 201, 197, 190, 188, 173, 145, 141,
              135, 134, 129, 129, 127, 121, 117, 112, 95)
# 2. 위도 경도 (수동 매핑)
europe_lat <- c(48.8566, 41.9028, 51.5074, 45.4408, 50.0755, 41.3851, 48.2092, 40.4168, 40.8518, 45.4642,
         52.5200, 48.1351, 41.0082, 47.8095, 52.3676, 50.1109, 41.9029, 37.9838, 47.3769,
         38.7169, 43.7384, 46.9481, 50.8503, 46.6863, 41.1579, 55.7558, 55.9533, 45.8150,
         60.1695, 46.2044, 55.6761, 59.3293, 42.6977, 59.9139, 52.2297, 59.9311, 64.1355)
europe_long <- c(2.3522, 12.4964, -0.1278, 12.3155, 14.4378, 2.1734, 16.3738, -3.7038, 14.2681, 9.1900,
          13.4050, 11.5820, 28.9784, 13.0550, 4.9041, 8.6821, 12.4534, 23.7275, 8.5417,
          -9.1399, 7.4246, 7.4474, 4.3517, 7.8632, -8.6291, 37.6173, -3.1883, 15.9819,
          24.9354, 6.1432, 12.5683, 18.0686, 23.3219, 10.7522, 21.0122, 30.3609, -21.8954)
# 도시 이름과 언급 수
world_city <- c("도쿄", "뉴욕", "홍콩", "오사카", "세부", "산티아고", "방콕", "싱가포르", "하와이", "베이징",
                "상하이", "후쿠오카", "하노이", "다낭", "샌프란시스코", "치앙마이", "발리", "나트랑", "나가사키",
                "타이베이", "삿포로", "시드니", "오키나와", "라스베이거스", "밴쿠버", "리마", "시애틀", "카이로",
                "로스앤젤레스", "보스턴", "하코다테", "가고시마", "시카고", "아바나", "괌", "두바이", "카트만두")
world_mention <- c(669, 566, 455, 394, 341, 334, 311, 294, 254, 243,
                   236, 230, 214, 210, 197, 183, 181, 179, 171,
                   165, 162, 160, 159, 149, 126, 123, 123, 122,
                   118, 117, 114, 109, 109, 106, 95, 91, 91)
# 위도 및 경도
world_lat <- c(35.6762, 40.7128, 22.3193, 34.6937, 10.3157, -33.4489, 13.7563, 1.3521, 21.3069, 39.9042,
               31.2304, 33.5904, 21.0285, 16.0544, 37.7749, 18.7883, -8.3405, 12.2388, 32.7503,
               25.0330, 43.0621, -33.8688, 26.2124, 36.1699, 49.2827, -12.0464, 47.6062, 30.0444,
               34.0522, 42.3601, 41.7688, 31.5966, 41.8781, 23.1136, 13.4443, 25.276987, 27.7172)
world_long <- c(139.6503, -74.0060, 114.1694, 135.5023, 123.8854, -70.6693, 100.5018, 103.8198, -157.8583, 116.4074,
                121.4737, 130.4017, 105.8542, 108.2022, -122.4194, 98.9853, 115.0920, 109.1967, 129.8777,
                121.5654, 141.3544, 151.2093, 127.6809, -115.1398, -123.1207, -77.0428, -122.3321, 31.2357,
                -118.2437, -71.0589, 140.7288, 130.5571, -87.6298, -82.3666, 144.7937, 55.296249, 85.3240)

# 3. 데이터프레임 생성
europe_cities <- tibble(
  city = europe_city,
  mentions = europe_mentions,
  lat = europe_lat,
  long = europe_long
)
europe_cities <- europe_cities[-c(30,24), ]
world_df <- tibble(
  city = world_city,
  mentions = world_mention,
  lat = world_lat,
  long = world_long
)
# 아메리카 대륙 도시 목록 (북미, 남미, 중남미)
america_cities <- c("뉴욕", "샌프란시스코", "밴쿠버", "시애틀", "로스앤젤레스",
                    "보스턴", "시카고", "하와이", "리마", "아바나","라스베이거스",
                    "산티아고")
japan_cities <- c("도쿄","오사카","후쿠오카","나가사키","삿포로","오키나와",
                  "하코다테","가고시마")
# 아메리카 대륙
world_df_america <- world_df %>%
  filter(world_city %in% america_cities) %>% slice(-2)

# 나머지 대륙
world_df_other <- world_df %>%
  filter(!(world_city %in% america_cities))
# 일본 도시만 필터링
world_df_japan <- world_df %>%
  filter(world_city %in% japan_cities)

#도시 지도
library(maps)
library(ggmap)
library(ggrepel)
World <- map_data('world')
Japan <- map_data('world',region = 'Japan')
Europe <- World %>%
  filter(long >= -25 & long <= 45,
         lat >= 35 & lat <= 72)
America <- World %>%
  filter(long >= -160 & long <= -50,
         lat >=-50 & lat <= 60)
RestofWorld <- World %>%
  filter(long >= 25 & long <= 170,
         lat >=-40 & lat <= 50)
ggplot() + 
  geom_polygon(data=Europe, 
               aes(x=long, y=lat, group=group),
               fill="white",
               colour="black")+
  geom_point(data=europe_cities, aes(x=long, y=lat, size = mentions),
             shape = 16, color = "green", alpha = 0.4)+ 
  scale_size_area(max_size=20)+
  geom_text_repel(data = europe_cities,
                  aes(x = long, y = lat, label = paste(city, mentions, sep = "\n")),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "red")
ggplot()+
  geom_polygon(data=America,
               aes(x=long, y=lat, group=group),
               fill="white",
               colour="black")+
  geom_point(data=world_df_america, aes(x=long, y=lat, size = mentions),
             shape = 16, color = "green", alpha = 0.4)+ 
  scale_size_area(max_size=20)+
  geom_text_repel(data = world_df_america,
                  aes(x = long, y = lat, label = paste(city, mentions, sep = "\n")),
                  size = 3,
                  box.padding = 1,
                  point.padding = 0.3,
                  segment.color = "red")
ggplot()+
  geom_polygon(data=RestofWorld,
               aes(x=long, y=lat, group=group),
               fill="white",
               colour="black")+
  geom_point(data=world_df_other, aes(x=long, y=lat, size = mentions),
             shape = 16, color = "green", alpha = 0.4)+
  scale_size_area(max_size=20)+
  geom_text(data=world_df_other, aes(x=long+0.2, y=lat+0.2, label= paste(city, mentions, sep = "\n")))

ggplot()+
  geom_polygon(data=Japan,
               aes(x=long, y=lat, group=group),
               fill="white",
               colour="black")+
  geom_point(data=world_df_japan, aes(x=long, y=lat, size = mentions),
             shape = 16, color = "green", alpha = 0.4)+ 
  scale_size_area(max_size=20)+
  geom_text(data=world_df_japan, aes(x=long+0.2, y=lat+0.2, label= paste(city, mentions, sep = "\n")))
