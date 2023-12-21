import csv
import requests
from bs4 import BeautifulSoup
from datetime import datetime

root_path = './data/Holidays'

start_year = "2018"
end_year = "2024"

countries = ["japan", "us", "indonesia", "taiwan"]

def mdy_to_ymd(d):
    result = ""
    for format in ('%b %d, %Y', '%d %b, %Y'):
        try:
            result = datetime.strptime(d, format).strftime('%Y-%m-%d')
        except:
            pass
    return result

def get_holidays(start_year, end_year, country = "japan"):
    holidays = []

    for cur_year in range(int(start_year), int(end_year)):
        url = f"https://www.timeanddate.com/holidays/{country}/{cur_year}"
        response = requests.get(url)
        soup = BeautifulSoup(response.text, "html.parser")

        table_data = soup.find('section', class_ = 'table-data__table')
        trs = table_data.find_all('tr')

        for tr in trs:
            date = tr.find('th', class_ = 'nw')
            name = tr.find('a')
            tds = tr.find_all('td')
            if tds and len(tds) >= 3:
                type = tds[2]
                holiday = {
                    "date": mdy_to_ymd(f"{date.text}, {cur_year}"),
                    "holiday_name": name.text,
                    "type": type.text
                }
                holidays.append(holiday)
        
    return holidays

def main():
    for country in countries:
        save_path = f"{root_path}/{country}_holidays.csv"
        with open(save_path, 'w', newline='', encoding='UTF-8') as f:
            w = csv.writer(f)
            w.writerow(["date", "holiday_name", "type"])

            holidays = get_holidays(start_year, end_year, country)
            for holiday in holidays:
                w.writerow([holiday["date"], holiday["holiday_name"], holiday["type"]])
        
        print(f"save file to {save_path}")
    print()

if __name__ == "__main__":
    main()