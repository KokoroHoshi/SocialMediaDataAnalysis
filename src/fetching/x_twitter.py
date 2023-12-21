# reference: https://github.com/shaikhsajid1111/twitter-scraper-selenium/tree/main

import re
import os
import time
import json
import pandas as pd
from datetime import timedelta
from selenium import webdriver
from dateutil.parser import parse
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


HOLOLIVE_USERNAME = "hololivetv"

cur_username = HOLOLIVE_USERNAME

secret_file = "./secrets/x_login_info.json"

# 01-01~01-01
# 2022-2023 -> 1~4
# 2021-2022 -> 5~7
# 2020-2021 -> 8~11
# 2019-2020 ->
# 2018-2019 ->
# 2017-2018 ->

save_csv_path = "./data/X_Twitter/tweets_data_14.csv"

start_date = "2019-01-01"
end_date = "2019-08-14"

chrome_options = Options()
chrome_options.add_argument('--incognito') 
driver = webdriver.Chrome('./chromedriver.exe')

def main(): 
    if os.path.exists(secret_file):
        with open(secret_file) as f:
            temp = json.load(f)
            LOGIN_USER = temp["USERNAME"]
            LOGIN_PSW = temp["PASSWORD"]
            del(temp)
    else:
        print("Not found api key.")
        return
    
    start_time = time.time()
    
    login_to_twitter(LOGIN_USER, LOGIN_PSW)
    time.sleep(3)
    
    tweets_data = get_tweets(cur_username, max_results=-1, start_date=start_date, end_date=end_date)
    # tweets_data = get_tweets(cur_username, max_results=-1, start_date="2022-01-01", end_date="2023-01-01", content_filter="\u2605\u2605\u2605 \u8a3a\u65ad\u5b8c\u4e86 \u2605\u2605\u2605")
    # tweets_data = get_tweets(cur_username, max_results=2500)
    
    driver.quit()
    
    data = pd.DataFrame(tweets_data)
    # print(data.head())
    
    pd.DataFrame(data).to_csv(save_csv_path, index=False)
    print(f"saved to '{save_csv_path}'.")
    
    end_time = time.time()
    
    print(f"execution time: {str(timedelta(seconds=end_time - start_time))}")
    
def login_to_twitter(username, password):
    driver.get('https://twitter.com/i/flow/login')
    
    username_input = WebDriverWait(driver, 5).until(
        EC.presence_of_element_located((By.CSS_SELECTOR, 'input[type="text" i]'))
    )
    username_input.send_keys(username)
    username_input.send_keys(Keys.RETURN)

    password_input = WebDriverWait(driver, 5).until(
        EC.presence_of_element_located((By.CSS_SELECTOR, 'input[type="password" i]'))
    )
    password_input.send_keys(password)
    password_input.send_keys(Keys.RETURN)
        
def get_tweets(username, max_results=5, start_date=None, end_date=None, content_filter=None, max_wait_times=5):
    def extract_digits(string):
        try:
            result = int(re.search(r'\d+', string).group(0))
            return result
        except:
            return -1
    
    def can_scrolling_down(current_scroll_position):
        document_height = driver.execute_script("return Math.max( document.body.scrollHeight, document.body.offsetHeight, document.documentElement.clientHeight, document.documentElement.scrollHeight, document.documentElement.offsetHeight);")
        window_height = driver.execute_script("return window.innerHeight;")
        max_scroll_height = document_height - window_height
        
        return current_scroll_position < max_scroll_height
    
    tweets_data = []
    
    if start_date is None or end_date is None:
        # https://twitter.com/search?q=from%3Ahololivetv%20since%3A2018-01-01%20until%3A2019-01-01&src=typed_query&f=live
        # driver.get(f'https://twitter.com/{username}/media')
        print("start date and end date are needed.")
        return
    if content_filter != None:
        search_query = f"from:{username} since:{start_date} until:{end_date} -\"{content_filter}\""
    else:
        search_query = f"from:{username} since:{start_date} until:{end_date}"

    search_input = WebDriverWait(driver, 5).until(
        EC.presence_of_element_located((By.CSS_SELECTOR, '[data-testid="SearchBox_Search_Input"]'))
    )
    search_input.send_keys(search_query)
    search_input.send_keys(Keys.RETURN)
    
    # click the "Latest button"
    lastest_btn = WebDriverWait(driver, 5).until(
        EC.presence_of_element_located((By.XPATH, '//a[contains(@href, "f=live")]'))
    )
    lastest_btn.click()
    
    # tweets = driver.find_elements(By.CSS_SELECTOR, '[data-testid="tweet"]')
    tweets = WebDriverWait(driver, 10).until(
        EC.presence_of_all_elements_located((By.CSS_SELECTOR, '[data-testid="tweet"]'))
    )
    
    driver.execute_script("arguments[0].scrollIntoView();", tweets[0])
    
    try:
        count = 1
        while count <= max_results or max_results == -1:
            tweet = tweets[count-1]
            
            try:
                content = WebDriverWait(tweet, 5).until(
                    EC.presence_of_element_located((By.CSS_SELECTOR, 'div[lang]'))
                )
            except:
                print("not found content\n")
            # content = tweet.find_element(By.CSS_SELECTOR, 'div[lang]')
            # print(f"{count}.")
            # print(content.text)
            # print()
            
            # print("tags:")
            tags = tweet.find_elements(By.XPATH, './/a[contains(@href, "/hashtag")]')
            tags_str = " ".join(tag.text for tag in tags)
            # print(tags_str)
            # print("\n\n")
                    
            timestamp = tweet.find_element(By.TAG_NAME,"time").get_attribute("datetime")
            posted_time = parse(timestamp).isoformat()
            # print(posted_time)
            
            try:
                replies_element = tweet.find_element(By.CSS_SELECTOR, '[data-testid="reply"]')
                replies = replies_element.get_attribute("aria-label")
            except:
                replies = None
            # print(f"replies: {extract_digits(replies)}", end=" ")
            
            try:
                retweet_element = tweet.find_element(By.CSS_SELECTOR, '[data-testid="retweet"]')
                retweets = retweet_element.get_attribute("aria-label")
            except:
                retweets = None
            # print(f"retweets: {extract_digits(retweets)}", end=" ")
            
            try:
                like_element = tweet.find_element(By.CSS_SELECTOR, '[data-testid="like"]')
                likes = like_element.get_attribute("aria-label")
            except:
                likes = None
            # print(f"likes: {extract_digits(likes)}", end=" ")
            
            try:
                view_element = tweet.find_element(By.XPATH, './/*[contains(@aria-label, "View post analytics")]')
                views = view_element.get_attribute("aria-label")
            except:
                views = None
            # print(f"views: {extract_digits(views)}")
            
            # print()
            
            tweet_data = dict(content = content.text,
                                tags = tags_str,
                                posted_time = posted_time,
                                replies = extract_digits(replies),
                                retweets = extract_digits(retweets),
                                likes = extract_digits(likes),
                                views = extract_digits(views))
            
            tweets_data.append(tweet_data)
            
            driver.execute_script("arguments[0].scrollIntoView();", tweet)

            count += 1
            
            if len(tweets) == count-1:  
                if count <= 6:
                    time.sleep(1)
                               
                current_scroll_position = driver.execute_script("return window.pageYOffset;")
                WebDriverWait(driver, 5).until(lambda _: can_scrolling_down(current_scroll_position))
                
                temp = WebDriverWait(driver, 10).until(
                    EC.presence_of_all_elements_located((By.CSS_SELECTOR, '[data-testid="tweet"]'))
                )
                
                for new_tweet in temp:
                    if new_tweet not in tweets:
                        tweets.append(new_tweet)
                      
                i = 1
                while len(tweets) == count-1 and i <= max_wait_times:
                    print(f"{i}. not more tweets or scroll too fast")
                    time.sleep(i)

                    current_scroll_position = driver.execute_script("return window.pageYOffset;")
                    WebDriverWait(driver, 5).until(lambda _: can_scrolling_down(current_scroll_position))
                    
                    temp = WebDriverWait(driver, 10).until(
                        EC.presence_of_all_elements_located((By.CSS_SELECTOR, '[data-testid="tweet"]'))
                    )
                    
                    for new_tweet in temp:
                        if new_tweet not in tweets:
                            tweets.append(new_tweet)
                    
                    i += 1
                            
                if len(tweets) == count-1:
                    # need to fix x(twitter) Somthing went wrong problem (there's a Retry btn or just refresh the page)
                    print("not more tweets or something went wrong ( x(twitter) or network problem )")
                    break

    except Exception as e:
        print(f"error: {e}")
    
    print(f"total tweet nums: {count-1}")
    return tweets_data

if __name__ == "__main__":
    main()