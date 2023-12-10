# !pip install --upgrade google-api-python-client

import os
import json
import pandas as pd
from googleapiclient.discovery import build

# channel id
CALLIOPE_MORI_ID = "UCL_qhgtOy0dy1Agp8vkySQg"
HOLOLIVE_ID = "UCJFZiqLMntJufDCHc6bQixg"

api_service_name = "youtube"
api_version = "v3"
api_key_file = "./secrets/youtube.json"

# save_csv_path = "./data/YouTube/video_details.csv"
save_csv_path = "./data/YouTube/test.csv"

cur_channel_id = HOLOLIVE_ID

def main():  
    if os.path.exists(api_key_file):
        with open(api_key_file) as f:
            API_KEY = json.load(f)["API_KEY"]
    else:
        print("Not found api key.")
        return

    youtube = build(api_service_name, api_version, developerKey=API_KEY)

    # get the playlist id contains all videos in the channel
    print()
    print("getting playlist_id...")
    playlist_id = get_channel_playlistId(youtube, cur_channel_id)
    print(f"got playlist id: {playlist_id}\n")
    
    print("getting video_ids...")
    video_ids = get_video_ids(youtube, playlist_id, max_results=50, pages_limit=-1)
    print(f"got {len(video_ids)} video_ids!\n")
    
    print("getting video_details...")
    video_details = get_video_details(youtube, video_ids)
    print(f"got {len(video_details)} video_details!\n")
    
    data = pd.DataFrame(video_details)
    print("show top 5 data:")
    print(data.head())
    print()
    
    pd.DataFrame(data).to_csv(save_csv_path, index=False)
    print(f"saved to '{save_csv_path}'.")

def get_channel_playlistId(youtube, channel_id):
    request = youtube.channels().list(
        part = "contentDetails",
        id = channel_id,
    )

    response = request.execute()

    return response['items'][0]['contentDetails']['relatedPlaylists']['uploads']

def get_video_ids(youtube, playlist_id, max_results= 5, pages_limit= -1):
    if max_results > 50:
        max_results = 50
    if max_results < 0:
        max_results = 0

    request = youtube.playlistItems().list(
        part="contentDetails",
        playlistId=playlist_id,
        maxResults = max_results
    )

    response = request.execute()

    video_ids = []

    for i in range(len(response["items"])):
        video_ids.append(response["items"][i]["contentDetails"]["videoId"])

    next_page_token = response.get("nextPageToken")
    cur_page = 1

    more_pages = True

    while more_pages:
        if next_page_token is None or (cur_page >= pages_limit and pages_limit != -1):
            more_pages = False
        else:
            request = youtube.playlistItems().list(
                part= "contentDetails",
                playlistId= playlist_id,
                maxResults= max_results,
                pageToken= next_page_token
            )

            response = request.execute()

            for i in range(len(response["items"])):
                video_ids.append(response["items"][i]["contentDetails"]["videoId"])

            next_page_token = response.get("nextPageToken")
            cur_page += 1

    return video_ids

def get_video_details(youtube, video_ids):
    all_video_stats = []

    for i in range(0, len(video_ids), 50):
        request = youtube.videos().list(
            part= "id, snippet, statistics, status, contentDetails, liveStreamingDetails, topicDetails, localizations",
            id= ",".join(video_ids[i:i+50])
        )

        response = request.execute()

        for video in response["items"]:
            try:
                video_stats = dict(
                            id = video.get("id"),
                            privacyStatus = video.get("status", {}).get("privacyStatus"),
                            title = video.get("snippet", {}).get("title"),
                            publishedDate = video.get("snippet", {}).get("publishedAt"),
                            duration = video.get("contentDetails", {}).get("duration"),
                            tags = video.get("snippet", {}).get("tags", []),
                            views = video.get("statistics", {}).get("viewCount"),
                            likes = video.get("statistics", {}).get("likeCount"),
                            comments = video.get("statistics", {}).get("commentCount"),
                            topicCategories = video.get("topicDetails", {}).get("topicCategories", []),
                            liveStreamActualStartTime = video.get("liveStreamingDetails", {}).get("actualStartTime"),
                            liveStreamActualEndTime = video.get("liveStreamingDetails", {}).get("actualEndTime"),
                            liveStreamScheduledStartTime = video.get("liveStreamingDetails", {}).get("scheduledStartTime"),
                            localizations = list(video.get("localizations", {}).keys())
                        )
            except KeyError as e:
                print(e)

            all_video_stats.append(video_stats)

    return all_video_stats

if __name__ == '__main__':
    main()