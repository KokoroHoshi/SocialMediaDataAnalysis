from __future__ import print_function

import datetime
import os.path
import pandas as pd

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

# If modifying these scopes, delete the file token.json.
SCOPES = ['https://www.googleapis.com/auth/calendar.readonly']

ZH_TW_CALENDAR_ID = "zh-tw.taiwan#holiday@group.v.calendar.google.com"
TW_CALENDAR_ID = "en.taiwan#holiday@group.v.calendar.google.com"
JP_CALENDAR_ID = "en.japanese#holiday@group.v.calendar.google.com"
UK_CALENDAR_ID = "en.uk#holiday@group.v.calendar.google.com"
USA_CALENDAR_ID = "en.usa#holiday@group.v.calendar.google.com"
ID_CALENDAR_ID = "en.indonesian#holiday@group.v.calendar.google.com"
AU_CALENDAR_ID = "en.australian#holiday@group.v.calendar.google.com"

client_secrets_file = "./secrets/google_calendar_credentials.json"
token_file = "./secrets/google_calendar_token.json"

save_path = "./data/Google Calendar/holidays.csv"
# only can get 2022-2023
start_date = (2018, 1, 1)
end_date = (2024, 1, 1)

def main():
    """Shows basic usage of the Google Calendar API.
    Prints the start and name of the next 10 events on the user's calendar.
    """
    creds = None
    # The file token.json stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists(token_file):
        creds = Credentials.from_authorized_user_file(token_file, SCOPES)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                client_secrets_file, SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open(token_file, 'w') as token:
            token.write(creds.to_json())
            
    holidays = get_hoilday(creds, UK_CALENDAR_ID, start_date, end_date, max_results=365)
    data = pd.DataFrame(holidays)
    print(data)
    pd.DataFrame(data).to_csv(save_path, index=False)

def get_hoilday(creds, calendar_id, start_date= None, end_date= None, max_results= 10):
    try:
        service = build('calendar', 'v3', credentials=creds)

        # Call the Calendar API
        now = datetime.datetime.utcnow().isoformat() + 'Z'  # 'Z' indicates UTC time
        print(now)
        
        if start_date is None or end_date is None:
            events_result = service.events().list(calendarId=calendar_id, timeMin=now,
                                                maxResults=max_results, singleEvents=True,
                                                orderBy='startTime').execute()
        else:
            start_time = datetime.datetime(start_date[0], start_date[1], start_date[2], 0, 0, 0).isoformat() + 'Z'
            end_time = datetime.datetime(end_date[0], end_date[1], end_date[2], 0, 0, 0).isoformat() + 'Z'
            events_result = service.events().list(calendarId=calendar_id,timeMin=start_time,
                                                timeMax=end_time, maxResults=max_results, singleEvents=True,
                                                orderBy='startTime').execute()
        events = events_result.get('items', [])

        if not events:
            print('No upcoming events found.')
            return

        # Prints the start and name of the next 10 events
        result = []
        for event in events:
            start = event['start'].get('dateTime', event['start'].get('date'))
            # print(start, event['summary'])
            holiday_data = {
                'date' : start,
                'holiday name' : event['summary']
            }
            result.append(holiday_data)
        return result
        

    except HttpError as error:
        print('An error occurred: %s' % error)


if __name__ == '__main__':
    main()