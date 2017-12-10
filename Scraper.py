from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait # available since 2.4.0
from selenium.webdriver.support import expected_conditions as EC # available since 2.26.0
from selenium import webdriver
from selenium.webdriver.chrome.options import Options

from datetime import date
from datetime import timedelta

from bs4 import BeautifulSoup
import csv

def pull_new_data (place):
    page_source = get_page("https://das.sbcapcd.org/StationSummaryNew.aspx", place, 7)
    table = parse_data (parse_source)
    return table

def get_page (website, place, days):
    driver = get_unmodified_page (website)
    select_place_option (driver, place)
    select_time_option (driver, days)
    page_source = driver.page_source
    driver.close()
    return page_source

def get_unmodified_page (website):
    #CHROME_PATH = '/usr/bin/google-chrome'
    #CHROMEDRIVER_PATH = '/usr/bin/chromedriver'
    WINDOW_SIZE = "1920,1080"
    
    chrome_options = Options()
    #schrome_options.add_argument("--headless")
    chrome_options.add_argument("--window-size=%s" % WINDOW_SIZE)
    #chrome_options.binary_location = CHROME_PATH
    
    driver = webdriver.Chrome(chrome_options=chrome_options)
    driver.get("https://das.sbcapcd.org/StationSummaryNew.aspx")
    return driver

def select_time_option (driver, days):
    select = driver.find_element_by_name ("StationsSummaries1$ddlTime")
    options = select.find_elements_by_tag_name("option")
    new_option = str(days)
    for option in options:
        #print "Value is: " + option.get_attribute("value")
        if (new_option == option.get_attribute("value")):
            option.click()

    new_date = date.today()-timedelta(days=days-1, hours=5)
    waitfor = new_date.strftime("%m/%d/%Y")
    try:
        WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.PARTIAL_LINK_TEXT, waitfor)))
    except:
        print "Could not find " + waitfor

def select_place_option (driver, place):
    select = driver.find_element_by_name ("StationsSummaries1$SiteList")
    options = select.find_elements_by_tag_name("option")
    for option in options:
        print "Value is: " + option.get_attribute("value")
        if (place == option.get_attribute("value")):
            option.click()

    waitfor = "Hourly Results for " + place
    try:
        WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.PARTIAL_LINK_TEXT, waitfor)))
    except:
        print "Could not find " + waitfor


def parse_data (page_source):
    soup = BeautifulSoup(page_source, "html.parser")

    tags = soup.find_all (["td"])
    table = []
    row = 0
    column = 0
    for tag in tags:
        
        if "class" in tag.attrs and "SummaryValue" in tag ["class"]:
            if column == 0:
                table.append ([])
            table[row].append (tag.getText().encode ('ascii'))
            #print tag.getText()
            column +=1
        if column == 7:
            column = 0
            row+=1
    return table



def read_data(filename):
    data = []
    with open(filename, 'rb') as f:
        reader = csv.reader(f, delimiter=',', quoting=csv.QUOTE_NONE)
        for row in reader:
            data.append (row)
    return data

def merge_data (data_1, data_2):
    complete_data = data_1
    for row in data_2:
        if not row in complete_data:
            complete_data.append (row)
    return complete_data

def write_data (filename, data):
    with open(filename, 'wb') as f:
        writer = csv.writer(f)
        writer.writerows(data)

def update_data (filename, place):
    
    new_data = pull_new_data(place)
    print (new_data)
    old_data = read_data(filename)
    complete_data = merge_data (old_data, new_data)
    write_data (filename, complete_data)

place = "Santa Barbara"
filename = "lib/AQI.csv"
filename = "lib/SantaBarbaraAQI.csv"
update_data(filename, place)


