# 원본 국가 리스트
all_countries <- tour %>%
  distinct(국가명) %>%
  mutate(표현 = 국가명)  # 초기에는 표현 == 원래 국가명
country_aliases <- tibble(
  실제_국가명 = c("튀르키예", "튀르키예"),
  표현 = c("튀르키예", "터키")
)
country_patterns <- all_countries %>%
  left_join(country_aliases, by = c("표현" = "표현")) %>%
  mutate(실제_국가명 = coalesce(실제_국가명, 국가명)) %>%
  mutate(
    pattern = case_when(
      표현 == "인도" ~ "(?<![가-힣a-zA-Z0-9])인도(?!\\s?(길|로|가|걷|보도))",
      TRUE ~ paste0("(?<![가-힣a-zA-Z0-9])", 표현)
    )
  )
book_country_df <- map_dfr(seq_along(full_text), function(i) {
  text <- full_text[[i]]
  country_patterns %>%
    mutate(
      book_id = i,
      is_mentioned = str_detect(text, regex(pattern))
    ) %>%
    filter(is_mentioned) %>%
    select(book_id, 실제_국가명)
})
country_mentions <- book_country_df %>%
  distinct(book_id, 실제_국가명) %>%
  count(실제_국가명, name = "언급_책수") %>%
  arrange(desc(언급_책수))
head(country_mentions,30)

country_name_map <- tibble(
  실제_국가명 = c("대한민국", "일본", "미국", "프랑스", "중국",
             "스페인", "영국", "이탈리아", "독일", "스위스", "베트남",
             "그리스", "오스트리아","태국","러시아","체코","포르투갈","캐나다",
             "네덜란드","헝가리","멕시코","페루","호주","이집트","벨기에",
             "크로아티아","싱가포르","브라질","스웨덴"),
  country.etc = c("Korea South", "Japan", "USA", "France", "China",
                  "Spain", "UK", "Italy", "Germany", "Switzerland", "Vietnam",
                  "Greece" ,"Austria","Thailand","Russia","Czech Republic","Portugal","Cananda"
                  ,"Netherlands","Hungary","Mexico","Peru","Australia","Egypt",
                  "Belgium","Croatia","Singapore","Brazil","Sweden")
)
mention_mapped <- country_mentions %>%
  left_join(country_name_map, by = "실제_국가명")
library(maps)
library(ggmap)
library(ggrepel)
# 지도 데이터
world_map <- map_data("world")

# country.etc 기준으로 언급수 붙이기
plot_data <- world_map %>%
  left_join(mention_mapped, by = c("region" = "country.etc"))

# 시각화
ggplot(plot_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = 언급_책수), color = "white") +
  scale_fill_viridis_c(na.value = "gray90") +
  theme_minimal() +
  labs(title = "국가별 언급된 책 수", fill = "책 수")

