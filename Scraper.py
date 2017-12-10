
from mechanize import Browser
from bs4 import BeautifulSoup
import csv

def pull_new_data ():
    br = Browser()
    br.open('https://das.sbcapcd.org/StationSummaryNew.aspx')
    
    soup = BeautifulSoup(br.response().read(), "html.parser")

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

def update_data (filename):
    
    new_data = pull_new_data()
    old_data = read_data(filename)
    complete_data = merge_data (old_data, new_data)
    write_data (filename, complete_data)

filename = "lib/AQI.csv"
update_data(filename)


