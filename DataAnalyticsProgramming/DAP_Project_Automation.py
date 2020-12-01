import pandas as pd
import pymongo
import json
import os
import kaggle as kg
import psycopg2
from sqlalchemy import create_engine
import numpy as np
#import for visualization
import plotly.graph_objects as go
import matplotlib.pyplot as plt
import seaborn as sns
import nltk # preprocessing text
import string # for finding punctuation in text
import re # regular expression
from textblob import TextBlob
nltk.download()

#pip install textblob
#pip install kaggle
#pip install textblob
#python -m textblob.download_corpora


#Downloading unzipig the Data in the json formats
def data_download_api():
    os.environ['KAGGLE_USERNAME'] = 'jonathanmendes'
    os.environ['KAGGLE_KEY'] = 'e048b725665b5068dc3fa30896fdbf5a'
    kg.api.authenticate()
    kg.api.dataset_download_files(dataset="yelp-dataset/yelp-dataset", path='C:\\Users\\jonat\\OneDrive\\Desktop\\DAP_Project\\API', unzip=True)
    print("Data downloaded and unzipped successfully")

#Mongodb connection and Dataload from python in json format to mongodb 
def dataload_mongodb():
    dapclient = pymongo.MongoClient("mongodb://localhost:27017/")
    mydb = dapclient["DAPdatabase"] #Creation of database object
    #load Business data into mongodb
    data_collection = mydb["business_data"]  
    with open('C:\\Users\\jonat\\OneDrive\\Desktop\\DAP_Project\\API\\yelp_academic_dataset_business.json','r',encoding='utf-8') as i:
        for lines in i:
            lines = json.loads(lines.strip())
            data_collection.insert_one(lines)
    print("Business json data loaded successfully in mongodb")
    #load Checkin data into mongodb
    data_collection = mydb["checkin_data"]  
    with open('C:\\Users\\jonat\\OneDrive\\Desktop\\DAP_Project\\API\\yelp_academic_dataset_checkin.json','r',encoding='utf-8') as j:
        for lines in j:
            lines = json.loads(lines.strip())
            data_collection.insert_one(lines)
    print("checkin json data loaded successfully in mongodb")
    #load Review data into mongodb
    data_collection = mydb["review_data"]  
    with open('C:\\Users\\jonat\\OneDrive\\Desktop\\DAP_Project\\API\\yelp_academic_dataset_review.json','r',encoding='utf-8') as k:
        for lines in k:
            lines = json.loads(lines.strip())
            data_collection.insert_one(lines)
    print("review json data loaded successfully in mongodb")
    dapclient.close()

#Reading Data from MongoDb to Python 
#Data Structring and Cleaning in python
#Postgres connection, create DB, Tables and insert values in Postgres 
def postmongodb_prepostgres():
    #Mongodb connection and Dataload from python in json format to mongodb 
    dataread_mongodb()
    
#Mongodb connection and Dataload from python in json format to mongodb 
def dataread_mongodb():
    dapclient = pymongo.MongoClient("mongodb://localhost:27017/")
    mydb = dapclient["DAPdatabase"] #Creation of database object
    businessdata = mydb["business_data"]
    checkindata = mydb["checkin_data"]
    reviewdata=mydb["review_data"]
    #Retrieve all data from the collection_data collection
    jsonbusinessdata=businessdata.find()
    jsoncheckindata=checkindata.find()
    jsonreviewdata=reviewdata.find()
    dataclean_prepostgres(jsonbusinessdata,jsoncheckindata,jsonreviewdata)
    
#Data Structring and Cleaning in python
def dataclean_prepostgres(jsonbusinessdata,jsoncheckindata,jsonreviewdata):
    #Structuring the data-json to dataframes using panda
    dfbusiness=pd.DataFrame(jsonbusinessdata)
    dfcheckin=pd.DataFrame(jsoncheckindata)
    dfreview=pd.DataFrame(jsonreviewdata)
    
    #Data Cleaning
    #Dropping unwanted columns from tables business--_id,hours,is_open, checkin--_id, review--_id
    dfbusinessfiltered=dfbusiness.drop(columns=['_id', 'hours','is_open'])
    dfcheckinfiltered=dfcheckin.drop(columns=['_id'])
    dfreviewfiltered=dfreview.drop(columns=['_id'])
    #Removing NA's
    dfbusinessfiltered=dfbusinessfiltered.dropna()
    dfcheckinfiltered=dfcheckinfiltered.dropna()
    dfreviewfiltered=dfreviewfiltered.dropna()
    #null checks
    dfbusinessfiltered.isnull().count()
    dfcheckinfiltered.isnull().count()
    dfreviewfiltered.isnull().count()
    #print('The number of null rows in business:'+dfbusinessfiltered.isnull().count()+' checkin:'+dfcheckinfiltered.isnull().count()+' review:'+dfreviewfiltered.isnull().count())

    #Filtering the categories and picking restaurants #Label-location based indexer for selection by label.
    dfbusinessfiltered=dfbusinessfiltered.loc[dfbusinessfiltered['categories'].str.startswith('Restaurants',na=False)] 
    dfcheckinfiltered=dfcheckinfiltered.loc[dfcheckinfiltered['business_id'].isin(dfbusinessfiltered['business_id'])] 
    dfreviewfiltered=dfreviewfiltered.loc[dfreviewfiltered['business_id'].isin(dfbusinessfiltered['business_id'])]
    #After analysing the chekinid data it is understood that there are no checkins registered for a few restaurants hence
    #filtering out the business and review data to keep it synchronised  
    dfbusinessfiltered=dfbusinessfiltered.loc[dfbusinessfiltered['business_id'].isin(dfcheckinfiltered['business_id'])]
    dfreviewfiltered=dfreviewfiltered.loc[dfreviewfiltered['business_id'].isin(dfbusinessfiltered['business_id'])]
    #converting the list/dict into a string for insertion into posgress
    dfbusinessfiltered['attributes'] = list(map(lambda x: json.dumps(x), dfbusinessfiltered['attributes']))
    #Adding a total number of checkins for a restaurant column 
    dfcheckinfiltered['checkins']=dfcheckinfiltered['date'].str.count(',')+1 
    #merging the business and checkin using inner join
    dfmergebusinesscheckin = pd.merge(dfbusinessfiltered, dfcheckinfiltered, how='inner', on=['business_id'])
    #Creating a table for states wwith maximum 5 stars review ratings by merging Business, reviews data and using group by, order by and count as in sql
    dfdapstateswithmax5stars = dfbusinessfiltered[['business_id','state']] \
                        .merge(dfreviewfiltered[['business_id','stars']][dfreviewfiltered.stars==5], on='business_id') \
                        .groupby('state',as_index=False)['stars'].count() \
                        .sort_values('stars',ascending=False) \
                        .rename(columns={'stars':'n5stars'}) 
    #Creting a table to find the number of reviews given by an average user                 
    review_count = dfreviewfiltered[['user_id','review_id']] \
        .groupby('user_id',as_index=False)['review_id'].count() \
        .rename(columns={'review_id':'review_count'})
    dfdapavg_user_review_count =  review_count[review_count.review_count <= 40]
 
                        
    dataload_postgres(dfbusinessfiltered,dfcheckinfiltered,dfreviewfiltered,dfmergebusinesscheckin,dfdapstateswithmax5stars,dfdapavg_user_review_count)

#Postgres connection, create DB, Tables and insert values in Postgres 
def dataload_postgres(dfbusinessfiltered,dfcheckinfiltered,dfreviewfiltered,dfmergebusinesscheckin,dfdapstateswithmax5stars,dfdapavg_user_review_count):
    # Establish postgres database connection 
    try:
        dbConnection = psycopg2.connect(
        user = "postgres",
        password = "password",
        host = "127.0.0.1",
        port = "5432",
        database = "postgres")
        dbConnection.set_isolation_level(0) # AUTOCOMMIT
        dbCursor = dbConnection.cursor()
        dbCursor.execute("CREATE DATABASE DAPProject;")    # Create a database called "DAPProject"
        dbCursor.close()
    except (Exception, psycopg2.Error) as dbError :
        print ("Error while connecting to PostgreSQL", dbError)
    finally:
        if(dbConnection):
            dbConnection.close() 
    #create table with the colun data  types as in dataframe and insert into postgresusing sqlalchemy
    engine = create_engine('postgresql://postgres:password@127.0.0.1:5432/dapproject')
    #chunksize used to insert in batches of 10000 to free up memory
    dfbusinessfiltered.to_sql('business',engine,if_exists='replace',chunksize=10000)
    dfcheckinfiltered.to_sql('checkin',engine,if_exists='replace',chunksize=10000)
    dfreviewfiltered.to_sql('review',engine,if_exists='replace',chunksize=10000)
    dfmergebusinesscheckin.to_sql('businesscheckin',engine,if_exists='replace',chunksize=10000)
    dfdapstateswithmax5stars.to_sql('stateswithmax5stars',engine,if_exists='replace',chunksize=10000)
    dfdapavg_user_review_count.to_sql('avguserreviewcount',engine,if_exists='replace',chunksize=10000)
    
#data pulled from postgres for visualizations    
def dataread_postgres_forvisualization():
    #create table with the colun data  types as in dataframe and insert into postgresusing sqlalchemy
    engine = create_engine('postgresql://postgres:password@127.0.0.1:5432/dapproject')
    #pulling data from posgres12
    dfdapvisuals_business_checkin = pd.read_sql_query('select * from businesscheckin',con=engine)
    dfdapvisuals_reviews = pd.read_sql_query('select * from review',con=engine)
    dfdapvisuals_business = pd.read_sql_query('select * from business',con=engine)
    dfdapvisuals_checkin = pd.read_sql_query('select * from checkin',con=engine)
    dfdapvisuals_stateswithmax5stars = pd.read_sql_query('select * from stateswithmax5stars',con=engine)
    dfdapvisuals_avg_user_review_count = pd.read_sql_query('select * from avguserreviewcount',con=engine)
    #Visualization funcions
    max_checkins_inaz_visualization(dfdapvisuals_business_checkin)
    state_with_max_5stars_reviews(dfdapvisuals_stateswithmax5stars)
    total_reviews_given_byavg_user(dfdapvisuals_avg_user_review_count)
    review_rating_distribution(dfdapvisuals_business_checkin)
    city_withmaxrestaurants(dfdapvisuals_business_checkin)
    city_popularity_heatmap(dfdapvisuals_business_checkin)
    different_restaurant_states(dfdapvisuals_business_checkin)
    sentiment_polarity(dfdapvisuals_reviews,dfdapvisuals_business_checkin)

#visualizations for restaurants in AZ with max checkins
def max_checkins_inaz_visualization(dfdapvisuals_business_checkin):
    dfcheckinrestaurant=dfdapvisuals_business_checkin[(dfdapvisuals_business_checkin['state']=='AZ')][:20]
    mergerc=dfcheckinrestaurant.sort_values(by="checkins", ascending=False)
    plt.style.use('seaborn-talk')
    rc = mergerc[['checkins']].plot(kind='bar',figsize=(15,10),legend=False, fontsize=12, color=[plt.cm.Paired(np.arange(len(dfdapvisuals_business_checkin)))])
    rc.set_xticklabels(mergerc.name, rotation=90)
    rc.set_xlabel("Restaurant Names",fontsize=10)
    rc.set_ylabel("Number of Checkins",fontsize=10)
    plt.show()

#visualizations for states with maximum 5 stars
def state_with_max_5stars_reviews(dfdapvisuals_stateswithmax5stars):
    labels = dfdapvisuals_stateswithmax5stars['state']
    values = dfdapvisuals_stateswithmax5stars['n5stars']
    # pull is given as a fraction of the pie radius
    fig = go.Figure(data=[go.Pie(labels=labels, values=values, pull=[0.1, 0, 0, 0])])
    fig.update_layout(title_text="5 star reviews by state")
    fig.show()

#visualizations for total reviews given by an average user
def total_reviews_given_byavg_user(dfdapvisuals_avg_user_review_count):
    plt.figure(figsize=(12,5))
    ax=sns.kdeplot(dfdapvisuals_avg_user_review_count[('review_count')],shade=True,color='g')
    plt.title("Total Reviews given by an Average User",fontsize=18)
    plt.xlabel('Total number of reviews given ', fontsize=15)
    plt.ylabel('User Metric', fontsize=15)
    plt.show()

#visualizations for distributon of star review ratings by restaurants 
def review_rating_distribution(dfdapvisuals_business_checkin):
    #Get the distribution of the ratings
    x = dfdapvisuals_business_checkin['stars'].value_counts()
    y = x.sort_index()
    z = ['1 star','1.5 star','2 star','2.5 star','3 star','3.5 star','4 star','4.5 star','5 star']
    fig = go.Figure(data=[go.Pie(labels=z, values=y, hole=.3)])
    fig.update_layout(title_text="Rating Distribution of Restaurants")
    fig.show()

#City with maximum number of restaurants
def city_withmaxrestaurants(dfdapvisuals_business_checkin):
    sns.set(style="whitegrid")
    x=dfdapvisuals_business_checkin['city'].value_counts()
    x=x.sort_values(ascending=False)
    x=x.iloc[0:25]
    plt.figure(figsize=(16,4))
    sns.set_color_codes("pastel")
    ax = sns.barplot(x.index, x.values, alpha=0.8)
    plt.title("Which city has the most restaurants?")
    locs, labels = plt.xticks()
    plt.setp(labels, rotation=90)
    plt.ylabel('Number of Restaurants', fontsize=12)
    plt.xlabel('City', fontsize=12)
    #adding the text labels
    rects = ax.patches
    labels = x.values
    for rect, label in zip(rects, labels):
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width()/2, height + 5, label, ha='center', va='bottom')
    plt.show()

#Heatmap of Toronto and LasVegas which are the cities with most restaurants based on popularity
def city_popularity_heatmap(dfdapvisuals_business_checkin):    
    #Pull all the ratings data
    ratings=dfdapvisuals_business_checkin[['latitude','longitude','stars','review_count']]
    # Popularity column Created using stars*no_of_reviews
    ratings['popularity']=ratings['stars']*ratings['review_count'] 
    f, (ax1, ax2) = plt.subplots(1, 2, figsize=(15,7))
    #random point within LasVegas
    lat = 36.207430
    lon = -115.268460
    #Modificatons to get the right position
    lon_min, lon_max = lon-0.3,lon+0.5
    lat_min, lat_max = lat-0.4,lat+0.5
    #subset for LasVegas
    ratings_data_vegas=ratings[(ratings["longitude"]>lon_min) &\
                        (ratings["longitude"]<lon_max) &\
                        (ratings["latitude"]>lat_min) &\
                        (ratings["latitude"]<lat_max)]
    #Facet scatter plot
    ratings_data_vegas.plot(kind='scatter', x='longitude', y='latitude',
                    color='red', 
                    s=.5, alpha=.9, subplots=True, ax=ax1)
    ax1.set_title("Las Vegas")
    ax1.set_facecolor('grey')    
    #Random point within toronto
    lat = 43.653225
    lon = -79.383186
    #Modificatons to get the right position
    lon_min, lon_max = lon-0.3,lon+0.5
    lat_min, lat_max = lat-0.4,lat+0.5
    #subset for Toronto
    ratings_data_toronto=ratings[(ratings["longitude"]>lon_min) &\
                        (ratings["longitude"]<lon_max) &\
                        (ratings["latitude"]>lat_min) &\
                        (ratings["latitude"]<lat_max)]
    #plot Toronto
    ratings_data_toronto.plot(kind='scatter', x='longitude', y='latitude',
                    color='orange', 
                    s=.5, alpha=.8, subplots=True, ax=ax2)
    ax2.set_title("Toronto")
    ax2.set_facecolor('grey')
    f.show()

#Visualization of states by number of restaurants in a decening order by highest first
def different_restaurant_states(dfdapvisuals_business_checkin):
    x=dfdapvisuals_business_checkin['state'].value_counts()
    x=x.sort_values(ascending=False)
    x=x.iloc[0:25]
    fig = go.Figure(data=go.Scatter(x=x.index, y=x.values, mode='lines'))
    fig.show()

#Visualization to break down reviews of most reviewed restaurants
def sentiment_polarity(dfdapvisuals_reviews,dfdapvisuals_business_checkin):
    dfdapvisuals_reviews['name'] = dfdapvisuals_reviews['business_id'].map(dfdapvisuals_business_checkin.set_index('business_id')['name'])
    top_restaurants = dfdapvisuals_reviews.name.value_counts().index[:20].tolist()#Taking Only top 20 occurances restaurants
    dfdapvisuals_reviews = dfdapvisuals_reviews.loc[dfdapvisuals_reviews['name'].isin(top_restaurants)]
    def preprocess(x):
        x = re.sub('[^a-z\s]', '', x.lower())                  # remove noise
        x = [w for w in x.split() if w not in set(stopwords)]  # remove stopwords
        return ' '.join(x) # then join the text again
    # Finding out which stopwords need to removed as per english stopwords.
    i = nltk.corpus.stopwords.words('english')
    #  remove punctuations
    j = list(string.punctuation)
    stopwords = set(i).union(j)
    dfdapvisuals_reviews['text_clear'] = dfdapvisuals_reviews['text'].apply(preprocess)
    def sentiment(x):
        sentiment = TextBlob(x)
        return sentiment.sentiment.polarity
    dfdapvisuals_reviews['senti_polarity'] = dfdapvisuals_reviews['text_clear'].apply(sentiment)
    # sort the values in ascending order 
    dfdapvisuals_reviews.groupby(dfdapvisuals_reviews.name)[['useful','funny', 'cool']].mean().sort_values('useful',ascending=True).plot(kind='barh', figsize=(15, 14),width=0.7)
    # set y ticks font size to 18 for easy readable purposes.
    plt.yticks(fontsize=18)
    plt.title('Reviews by relevancy',fontsize=28)
    plt.ylabel('Restaurants names', fontsize=18)
    plt.xlabel('Number of reviews', fontsize=18)
    # set the y ticks
    plt.yticks(fontsize=10)
    plt.legend(fontsize=22)
    #display the the plot
    plt.show()  
    
def main():
    #Downloading unzipig the Data in the json formats
    data_download_api()
    #MongoDB connection and Dataload from python in json format to mongodb 
    dataload_mongodb()
    #Reading Data from MongoDb to Python 
    #Data Structring and Cleaning in python
    #Postgres connection, create DB, Tables and insert values into Postgres 
    postmongodb_prepostgres()
    #data pulled from postgres for visualizations
    dataread_postgres_forvisualization()

if __name__ == "__main__":
    main() 