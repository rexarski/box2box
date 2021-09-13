import tweepy
from tweepy import OAuthHandler
from tweepy import Stream
from tweepy.streaming import StreamListener
import pandas as pd

# All 4 keys are in my TwitterCodesFile.txt and are comma sep.
# Also don't forget to add this file to .gitignore
filename="TwitterAPIAuth.txt"
with open(filename, "r") as FILE:
    keys=[i for line in FILE for i in line.split(',')]

consumer_key, consumer_secret, access_token, access_secret = keys

auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_secret)

FILE.close()

api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)

def get_nba_players():

    members = []
    count = 0
    print("Getting players info ...")

    for member in tweepy.Cursor(api.list_members, list_id='17852612').items():
        try:
            data = [member.name, member.screen_name, member.location, member.description,
                member.url, member.followers_count, member.friends_count, member.listed_count,
                member.created_at, member.favourites_count, member.statuses_count, member.id]
            data = tuple(data)
            members.append(data)
            count += 1
            print("Player number: ", count)
        except tweepy.TweepError as e:
            print(e.reason)
            continue
        except StopIteration:
            break

    df = pd.DataFrame(members, columns = ['name','screen_name', 'location', 'description',
        'url', 'followers_count', 'friends_count', 'listed_count', 'created_at', 'favourites_count',
        'statuses_count', 'id'])
    df.to_csv(path_or_buf = '../data/nba-tweets/player-accounts.csv', mode='w', index=False)

def get_list_timeline(listid='17852612', mode='initial', sinceid='1344871882156371968'):

    # if mode == 'update', we need to set sinceid to the last/latest tweet id in the csv file

    if mode == 'initial':
        print("Initiating tweet scraping ...")
    elif mode == 'update':
        print("Updating scraping with latest tweets ...")
        # filename="../data/nba-tweets/player-tweets.csv"
        # with open(filename, "r") as FILE:
        #     for i, line in enumerate(FILE):
        #         if i == 1:
        #             previous_tweet_id = line.split(',')[1]
        #             break
        # FILE.close()
        # maxid = str(int(previous_tweet_id) + 1)

    tweets = []
    count = 0
    batch = 0

    while batch <= 50:
        scraper_cursor = tweepy.Cursor(api.list_timeline, list_id=listid, since_id=sinceid,
        include_rts=False).items()

        for tweet in scraper_cursor:
            try:
                data = [tweet.created_at, tweet.id, tweet.text, tweet.user._json['screen_name'], tweet.user._json['name'], tweet.user._json['created_at'], tweet.entities['urls']]
                data = tuple(data)
                tweets.append(data)
                # tweets.insert(0, data) # defacto reverse the list
                count += 1
                print("Tweet number: ", count)
            except tweepy.TweepError as e:
                print(e.reason)
                continue
            except StopIteration:
                break

        batch += 1

        df = pd.DataFrame(tweets, columns = ['created_at','tweet_id', 'tweet_text', 'screen_name', 'name',
            'account_creation_date', 'urls'])

        if mode == 'initial':
            if batch == 1:
                df.to_csv(path_or_buf = '../data/nba-tweets/player-tweets.csv', mode='w', index=False)
            else:
                df.to_csv(path_or_buf = '../data/nba-tweets/player-tweets.csv', mode='a', index=False, header=False)
        elif mode == 'update':
            df.to_csv(path_or_buf = '../data/nba-tweets/player-tweets.csv', mode='a', header=False, index=False)


get_nba_players()
get_list_timeline(mode='initial')
# get_list_timeline(mode='update')