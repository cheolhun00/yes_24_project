#워킹디렉토리설정&파일 업로드
getwd()
setwd("C:/바탕에서 옮긴 것/캡스톤디자인")
all_books <- readRDS("all_books.rds")
all_detailed_books <- readRDS("all_detailed_books.rds")
# 필요한 라이브러리
library(RSelenium)
library(rvest)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# RSelenium 세션 시작
# java -jar selenium-server-standalone-4.0.0-alpha-1.jar

library(RSelenium)

# 사용자 지정 chrome.exe 경로
chrome_path <- "C:/selenium/chrome-win64/chrome-win64/chrome.exe"

# Chrome 실행 옵션 설정
eCaps <- list(
  chromeOptions = list(
    binary = chrome_path,
    args = c(
      '--headless',                 # ← 필요 없으면 제거 가능
      '--disable-gpu',
      '--no-sandbox',
      '--disable-dev-shm-usage',
      '--incognito',               # 시크릿 모드
      '--disable-application-cache', # 앱 캐시 비활성화
      '--disable-cache',           # 일반 캐시 비활성화
      '--disk-cache-size=0'        # 디스크 캐시 크기 0
    )
  )
)

# 드라이버 실행
driver <- rsDriver(
  browser = "chrome",
  port = as.integer(4444),
  extraCapabilities = eCaps
)

# remote driver 객체 추출
remote_driver <- driver$client


# 하위 카테고리 목록
subcategories <- c(
  "처세술/삶의 자세", "인간관계", "성공학/경력관리", "화술/협상/회의진행",
  "기획/정보/시간관리", "창조적사고/두뇌계발", "여성을 위한 자기계발",
  "취업/유망직업", "성공스토리", "유학/이민"
)

# 자기계발 메인 페이지로 이동
remote_driver$navigate("https://www.yes24.com/Product/Category/Display/001001026")
Sys.sleep(3)

# 누적 저장 변수 초기화
all_books <- 0
all_books <- tibble()


for (subcategory in subcategories) {
  cat("▶ [서브카테고리] 클릭 중:", subcategory, "\n")
  
  tryCatch({
    remote_driver$findElement(using = "link text", value = subcategory)$clickElement()
    Sys.sleep(1)
    
    remote_driver$findElement(using = "xpath", value = "//a[contains(text(), '신상품순')]")$clickElement()
    Sys.sleep(1)
    
    remote_driver$findElement(using = "id", value = "pg_size")$sendKeysToElement(list("120"))
    Sys.sleep(1)
    
    repeat {
      cat("📄 페이지 정보 수집 중...\n")
      page_source <- remote_driver$getPageSource()[[1]]
      page_html <- read_html(page_source)
      
      books <- page_html %>%
        html_nodes(".itemUnit") %>%
        map_df(~ {
          title_node <- .x %>% html_node(".info_name .gd_name")
          title <- if (!is.na(title_node)) html_text(title_node, trim = TRUE) else NA
          url <- if (!is.na(title_node)) str_c("https://www.yes24.com", html_attr(title_node, "href")) else NA
          
          author_nodes <- .x %>% html_nodes(".info_pubGrp .info_auth")
          author <- if (length(author_nodes) > 0) html_text(author_nodes, trim = TRUE) %>% paste(collapse = ", ") else NA
          
          publisher_node <- .x %>% html_node(".info_pubGrp .info_pub a")
          publisher <- if (!is.na(publisher_node)) html_text(publisher_node, trim = TRUE) else NA
          
          pubdate_node <- .x %>% html_node(".info_pubGrp .info_date")
          pubdate <- if (!is.na(pubdate_node)) html_text(pubdate_node, trim = TRUE) else NA
          
          price_node <- .x %>% html_node(".info_price strong")
          price <- if (!is.na(price_node)) html_text(price_node, trim = TRUE) else NA
          
          discount_node <- .x %>% html_node(".info_price")
          discount <- if (!is.na(discount_node)) html_text(discount_node) %>% str_extract("\\d+%") else NA
          
          sales_node <- .x %>% html_node(".saleNum")
          sales <- if (!is.na(sales_node)) html_text(sales_node, trim = TRUE) %>% str_extract("\\d+(,\\d+)*") else NA
          
          status_node <- .x %>% html_node(".item_btnCol .soldOut")
          status <- if (!is.na(status_node)) html_text(status_node, trim = TRUE) else "판매중"
          
          tibble(
            서브카테고리 = subcategory,
            책_제목 = title,
            책_URL = url,
            저자 = author,
            출판사 = publisher,
            출간일 = pubdate,
            가격 = price,
            할인율 = discount,
            판매지수 = sales,
            판매상태 = status
          )
        }) %>% 
        filter(!is.na(책_제목))  # 제목 없는 항목 제거
      
      all_books <- bind_rows(all_books, books)
      
      cat("📘 현재 페이지에서 수집한 책 수:", nrow(books), "\n")
      cat("📊 누적 책 수:", nrow(all_books), "\n")
      
      current_page <- page_html %>%
        html_element(".yesUI_pagen strong.num") %>%
        html_text(trim = TRUE) %>%
        as.numeric()
      
      # 다음 페이지 존재 여부 확인
      next_page_num <- current_page + 1
      
      next_page_node <- page_html %>%
        html_elements(".yesUI_pagen a.num") %>%
        keep(~ html_text(.x, trim = TRUE) == as.character(next_page_num))
      
      if (length(next_page_node) > 0) {
        # 10페이지 이전까지는 숫자 클릭
        cat("➡ 다음 페이지로 이동 (", next_page_num, ")\n")
        remote_driver$findElement(
          using = "css selector",
          value = paste0("a.num[title='", next_page_num, "']")
        )$clickElement()
        Sys.sleep(3)
      } else {
        # 다음 숫자 버튼 없으면 '다음' (>) 버튼 클릭
        next_button <- tryCatch({
          remote_driver$findElement(using = "css selector", value = "a.next[title]")
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(next_button)) {
          cat("➡ '다음' 버튼 클릭으로 페이지 넘김 (현재:", current_page, ")\n")
          next_button$clickElement()
          Sys.sleep(3)
        } else {
          cat("✅ 마지막 페이지 도달 (", current_page, ")\n\n")
          break
        }
      }
    }
  }, error = function(e) {
    cat("❌ 오류 발생:", e$message, "\n")
  })
}

# 책 상세정보 수집 함수
get_book_details <- function(book_url, remote_driver, index = NULL, save_html = TRUE, save_dir = "성공스토리") {
  tryCatch({
    remote_driver$navigate(book_url)
    Sys.sleep(runif(1, 0.3, 0.5))
    
    # ✅ 스크롤 내려주기 (중요!)
    remote_driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(0.3)
    
    # ✅ 펼쳐보기 스크립트 (안전하게)
    remote_driver$executeScript("
  document.querySelectorAll('a[onclick*=\"toggleInfoSet\"], a[onclick*=\"toggleInfoSubSet\"]').forEach(function(button) {
    try {
      if (button.offsetParent !== null) {
        button.click();
      }
    } catch(e) {}
  });
")
    Sys.sleep(0.3)
    
    
    page <- remote_driver$getPageSource()[[1]] %>% read_html()
    
    list(
      카테고리_경로 = page %>%
        html_nodes("dl.yesAlertDl ul.yesAlertLi li a") %>%
        html_text(trim = TRUE) %>%
        paste(collapse = " > "),
      
      책소개_제목 = page %>% html_element("div#infoset_introduce h4.tit_txt") %>% html_text(trim = TRUE),
      책소개_내용 = page %>% html_element("div#infoset_introduce div.infoWrap_txtInner") %>% html_text(trim = TRUE) %>% str_squish(),
      
      목차 = page %>% html_element("div#infoset_toc div.infoWrap_txt textarea.txtContentText") %>%
        html_text(trim = TRUE) %>%
        str_replace_all("&lt;br/&gt;", "\n") %>%
        str_replace_all("&amp;", "&") %>%
        str_squish(),
      
      저자명 = page %>% html_element("div.author_name a.lnk_author") %>% html_text(trim = TRUE),
      저자_소개 = page %>% html_element("span.author_info.info_origin") %>% html_text(trim = TRUE),
      
      책_속으로 = page %>%
        html_element("div#infoset_inBook textarea.txtContentText") %>%  # 수정된 부분
        html_text(trim = TRUE) %>%
        str_remove_all("<.*?>") %>%
        str_replace_all("\n{2,}", "\n\n") %>%
        str_trim(),
      
      출판사_리뷰 = page %>% html_element("div#infoset_pubReivew textarea.txtContentText") %>%
        html_text(trim = TRUE) %>%
        str_remove_all("<.*?>") %>%
        str_replace_all("\n{2,}", "\n\n") %>%
        str_trim(),
      
      품목정보_테이블 = page %>% html_elements("table.tb_nor") %>% html_table(fill = TRUE) %>% pluck(1)
    )
  }, error = function(e) {
    cat("❌ 책 정보 수집 실패:", book_url, "\n")
    return(NULL)
  })
}

all_books <- readRDS("all_books.rds")
all_detailed_books <- readRDS("all_detailed_books.rds")

#방법 3(실패 url 추가버전)
# 실패한 URL 저장용 벡터
failed_urls <- c()

# 결과 저장 리스트 초기화
#인간관계 나머지 해야함
results <- vector("list", length = nrow(all_books[42772:43905,]))

# 크롤링 진행
for (i in seq_len(nrow(all_books[42772:43905,]))) {
  cat("▶", i, "번째 책 크롤링 중:", all_books$책_제목[42771+i], "\n")
  
  result <- tryCatch({
    get_book_details(all_books$책_URL[42771 + i], remote_driver)
  }, error = function(e) {
    failed_urls <<- c(failed_urls, all_books$책_URL[42771 + i])
    cat("❌", i, "번째 책: 크롤링 실패 (에러 발생)\n\n")
    return(NULL)
  })
  
  results[[i]] <- result
  
  if (is.null(result)) {
    cat("❌", i, "번째 책: 크롤링 실패 (NULL)\n\n")
  } else {
    cat("✅", i, "번째 책: 크롤링 성공\n\n")
  }
}


# 결과를 all_books 데이터프레임에 붙이기
detailed_books <- all_books[43906:44557,] %>%
  mutate(상세정보 = result_유학이민)

all_detailed_books <- bind_rows(all_detailed_books, detailed_books)


saveRDS(all_detailed_books, "all_detailed_books.rds")



result_화술협상회의진행 <- readRDS("result_화술협상회의진행.rds")
result_기획정보시간관리 <- readRDS("result_기획정보시간관리.rds")
result_유학이민 <- readRDS("result_유학이민.rds")
detailed_books <- all_books[34120:37005,] %>%
  mutate(상세정보 = result_기획정보시간관리)

all_detailed_books <- bind_rows(all_detailed_books, detailed_books)
remote_driver$close()

