library(rvest)
library(dplyr)
library(stringr)

# 1. 파일 목록
html_dir <- "yes24_html"
html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)

# 2. 저장용 벡터 초기화
titles <- authors <- descriptions <- ISBN13s <- pub_dates <- intros <- index <- character(length(html_files))
sell_nums <- numeric(length(html_files))

# 3. 루프 처리
for (i in seq_along(html_files)) {
  message("Processing: ", html_files[i])
  
  try({
    page <- read_html(html_files[i])
    
    # 제목
    title_node <- html_element(page, xpath = "//meta[@name='title']")
    titles[i] <- html_attr(title_node, "content")
    
    # 저자
    author_node <- html_element(page, xpath = "//meta[@name='author']")
    authors[i] <- html_attr(author_node, "content")
    
    # 설명 (책 소개)
    desc_node <- html_element(page, xpath = "//meta[@name='description']")
    descriptions[i] <- html_attr(desc_node, "content")
    
    # ISBN
    isbn_node <- html_element(page, xpath = "//meta[@property='books:isbn']")
    ISBN13s[i] <- html_attr(isbn_node, "content")
    
    # 출판일
    pub_dates[i] <- page %>%
      html_element("span.gd_date") %>%
      html_text2()
    
    # 판매지수 숫자만 추출
    sell_text <- page %>%
      html_element("span.gd_sellNum") %>%
      html_text2()
    sell_nums[i] <- str_extract_all(sell_text, "\\d+") %>%
      unlist() %>%
      paste(collapse = "") %>%
      as.numeric()
    
    # 책 소개 상세 (본문)
    intros[i] <- page %>%
      html_element("div.infoWrap_txtInner") %>%
      html_text2()
    
    # 책 목차 상세
    index[i] <- page %>%
      html_element("#infoset_toc .infoWrap_txt") %>%
      html_text2()
    
  }, silent = TRUE)
}

# 4. 데이터프레임 구성
df_books <- data.frame(
  file = basename(html_files),
  title = titles,
  author = authors,
  description = descriptions,
  ISBN13 = ISBN13s,
  pub_date = pub_dates,
  sales_point = sell_nums,
  intro = intros,
  index = index,
  stringsAsFactors = FALSE
)

# 5. 결과 저장
write.csv(df_books, "yes24_books_cleaned.csv", row.names = FALSE)

# 결과 미리보기
head(df_books)
