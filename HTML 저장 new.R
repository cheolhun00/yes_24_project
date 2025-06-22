setwd("C:/Users/hun-0/Desktop/캡스톤디자인")
getwd()
# 필요한 라이브러리
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
  extraCapabilities = eCaps,
  check = FALSE
)

# remote driver 객체 추출
remote_driver <- driver$client

#함수
get_book_details <- function(book_url, remote_driver, index = NULL, save_html = TRUE, save_dir = "처세술_삶의자세_page") {
  tryCatch({
    cat("▶ Navigating to:", book_url, "\n")
    remote_driver$navigate(book_url)
    Sys.sleep(runif(1, 0.1, 0.2))
    
    # 스크롤 다운
    remote_driver$executeScript("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(0.1)
    
    # 펼쳐보기 버튼 클릭
    remote_driver$executeScript("
      document.querySelectorAll('a[onclick*=\"toggleInfoSet\"], a[onclick*=\"toggleInfoSubSet\"]').forEach(function(button) {
        try {
          if (button.offsetParent !== null) {
            button.click();
          }
        } catch(e) {}
      });
    ")
    Sys.sleep(0.1)
    
    # 페이지 소스 가져오기
    html_source <- remote_driver$getPageSource()[[1]]
    
    # HTML 저장
    if (save_html && !is.null(index)) {
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
      goods_no <- str_extract(as.character(book_url), "\\d+$")
      group_no <- ((index - 1) %/% 120) + 1  # 120개씩 그룹화
      file_name <- sprintf("%s/%d_book_%s.html", save_dir, group_no, goods_no)
      writeLines(html_source, file_name, useBytes = TRUE)
      cat("✅ HTML saved to:", file_name, "\n")
    }
    },
      error = function(e) {
      cat("❌ 책 정보 수집 실패:", book_url, "\n")
      return(NULL)
    })
  }
  



#반복문 실행
# 최초 한 번만 실행
if (!exists("failed_urls_all")) failed_urls_all <- c()

df <- readRDS("처세술_삶의자세_books.rds")
book_url <- df[, "책_URL"]
results <- vector("list", length = nrow(book_url))
failed_urls <- c()

for (i in seq_len(nrow(book_url))) {
  current_url <- book_url$책_URL[i]
  cat("📘", i, "번째 책 크롤링 중:", current_url, "\n")
  
  result <- tryCatch({
    get_book_details(current_url, remote_driver, index = i)
  }, error = function(e) {
    cat("❌", i, "번째 책: 에러 발생\n")
    return(NULL)
  })
  
  results[[i]] <- result
  
  if (is.null(result)) {
    cat("❌", i, "번째 책: 크롤링 실패 (NULL)\n\n")
    failed_urls <- c(failed_urls, current_url)
  } else {
    cat("✅", i, "번째 책: 크롤링 성공\n\n")
  }
}

result_유학이민 <- results
# 이번 루프에서 실패한 URL 누적 저장
failed_urls_all <- c(failed_urls_all, failed_urls)

cat("⏹ 이번 구간 완료!\n")
cat("❌ 이번 구간 실패 개수:", length(failed_urls), "\n")
cat("📦 전체 실패 누적 개수:", length(failed_urls_all), "\n")



#상세정보 all_books에 붙이기
detailed_books_next <- all_books[1:4800,] %>%
  mutate(상세정보 = results)


all_detailed_books <- bind_rows(all_detailed_books, detailed_books_next)
