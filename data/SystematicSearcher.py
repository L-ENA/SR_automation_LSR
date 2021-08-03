# -*- coding: utf-8 -*-
"""
Created on Wed Feb 12 17:16:47 2020

@author: Lena Schmidt, lena.schmidt@bristol.ac.uk
"""

from tqdm import tqdm
import lxml.etree as ET
import pandas as pd
import pickle
from timeit import default_timer as timer
import os

import re
#import xml.dom.minidom
import random

from lxml.etree import tostring
from itertools import chain

import scrape as arxivscraper#import the scrape script from the local directory (make sure that the file is present!)
from RISparser import readris#for validation only##attribute!
from RISparser.config import TAG_KEY_MAPPING

from regex_helper import cluster_1, cluster_2, cluster_NOT_titles, cluster_NOT_abstracts

    
random.seed( 30 )



def scrape_arxiv(start='2005-01-01', arxiv_repo="cs"):
    
    print("Scraping the "+arxiv_repo +" arXiv... Depending on the starting date this can take a while. ")
    
    def getRecords(start):
        scraper = arxivscraper.Scraper(category=arxiv_repo, date_from=start)#usage of "until" is discouraged, see: https://arxiv.org/help/oa/index @ Datestamps section
        output = scraper.scrape()
        
        
        cols = output[0].keys()
        df = pd.DataFrame(output,columns=cols)
        
        return df
        
    
    #untilX='2010-01-10'
    
    
    df = getRecords(start)
    
    print('Done scraping arxiv from {}, found {} records'.format(start, df.shape[0]))
    
    
    df.to_pickle('data/arxiv-dump.pkl')
    df.to_csv('data/arxiv-dump.csv')#note that the IDs are automatically converted to floats, which means that leading or trailing zeros are removed. This does not happen when pickeling, pls use pickle file if IDs are of interest

def read_dblp():
    #
    #This method reads your local dblp file. Obtain it via: https://dblp.uni-trier.de/faq/How+can+I+download+the+whole+dblp+dataset
    #Also obtain "dblp.dtd"from the same place
    #
    #The method then parses the dblp xml and extracts the information that we will search.
    #
    #Method needs to be run only once for each dump, as it takes a long time. A pickle file will be exported in the end, and all subsequent methods will simply use this pickle file
    
    frac = 0.05#ony for testing, uncomment manually below if needed
    if os.path.exists("data/dblp.xml"):
        print("success")

    if os.path.exists("data/dblp.xml"):
        pass
    else:
        print("File dblp.xml not found in current directory. Please download it (and dblp.dtd) from University of Trier: https://dblp.uni-trier.de/faq/How+can+I+download+the+whole+dblp+dataset")
        return
    
    dtd = ET.DTD("data/dblp.dtd") #pylint: disable=E1101
    
    # get an iterable
    context = ET.iterparse("data/dblp.xml", events=('start', 'end'), load_dtd=True, #pylint: disable=E1101
        resolve_entities=True) 
    
    
    
    # turn it into an iterator
    context = iter(context)
    
    # get the root element
    event, root = next(context)
    
    mystr= ET.tostring(root)
    mystr = mystr.splitlines()
    #for stri in mystr:
        #print(stri)
    
    n_records_parsed = 0
    
    dblp_record_types_for_publications = ('article', 'inproceedings', 'proceedings', 'book', 'incollection',
        'phdthesis', 'masterthesis', 'www')
    years=[]
    titles=[]
    IDs=[]
    dates=[]
    dois=[]
    authors=[]
    no_yr = 0
    no_ttl = 0
    
    children=total = len(root.getchildren())
    
    
    for event, elem in tqdm(context, position=0, leave=True):
        if event == 'end' and elem.tag in dblp_record_types_for_publications:
            #if random.random()< frac:
                t = [title.text for title in elem.findall('title')]
                if ( len(t) >0 and t[0] != "Home Page"):#exclude the home-page entries, they have no content. Exclude empty titles, there is nothing to search
                    
                    if(len(t)==1):
                        
                        if t[0]==None:#dealing with sub/superscript and other issues, simply taking all text content
                            title_str = ""
                            for title in elem.findall('title'):
                                for c in title.getchildren():
                                    if c.text == None:
                                        c.text=""
                                    if c.tail == None:
                                        c.tail=""    
                                        
                                    title_str = title_str + c.text +c.tail
                                    
                            t[0]= title_str      
                            
                        titles.append(t[0])
                        
                    
                    else:
                        print("Multiple titles found")
                        print(t)
                        titles.append(" ".join(tit for tit in t) )
                    
                    
                    yr = [year.text for year in elem.findall('year')]
                    if(len(yr)==0):
                        print("No year for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                        years.append('2020')
                        
                        no_yr += 1
                    elif (len(yr)==1):
                        years.append(yr[0])
                        #print("Convert {}, {} to {}, {} ".format(yr[0], type(yr[0]), int(yr[0]), type(int(yr[0]))))
                        
                    else:
                        print("Multiple years for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                        print(yr)
                        
                        years.append(yr[0])
                        
                    doi = [d.text for d in elem.findall('ee')]
                    if(len(doi)==0):
                        #print("No DOI for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                        dois.append('n/a')
                        
                        
                    elif (len(doi)==1):
                        dois.append(doi[0])
                        #print("Convert {}, {} to {}, {} ".format(yr[0], type(yr[0]), int(yr[0]), type(int(yr[0]))))
                        
                    else:
                        #print("Multiple dois for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                        #print(doi)
                        
                        dois.append(" ; ".join(d for d in doi))
                        
                    
                    
                    key = elem.attrib['key']
                    
                    
                    
                    IDs.append(key)
                    
                    
                    date = elem.attrib['mdate']
                    
                    dates.append(date)
                    
                    

                    pub_authors = []
                    for author in elem.findall('author'):
                        if author.text is not None:
                            pub_authors.append(author.text)
                    authors.append("; ".join(pub_authors))


        
                
    
                elem.clear()
                root.clear()
        
                n_records_parsed += 1
    print("No. of records parsed: {}".format(n_records_parsed))
    print("No. titles: {}".format(len(titles)))
    print("No. years: {}".format(len(years)))
    print("No. dates: {}".format(len(dates)))
    print("No. IDs: {}".format(len(IDs)))
    print("No. DOIs: {}".format(len(dois)))
    print("No. authors: {}".format(len(authors)))
    
    print("No title: {}, No Year: {}".format(no_ttl, no_yr))
    doi=[]

    for d in dois:#need to clean links to get doi
        if "; " not in d:
            if "https://doi.org/" in d:#only 1 entry
                d = d.replace("https://doi.org/", "")
                doi.append(d)
            else:
                doi.append("")
        else:#there is a list of links
            #print(d)
            result=""
            for x in d.split(" ; "):
                if "https://doi.org/" in x:#try to squeeze doi from every link
                    result = x.replace("https://doi.org/", "")
            #print(result)
            doi.append(result)#appen the retrieved doi, or empty string if none found



    
    df = pd.DataFrame(list(zip(IDs, titles, dois , doi,authors, years, dates)),
                   columns =['ID', 'Title','DOI_link',"DOI","Authors", 'Year', 'Date'])
    
    #####################################some processing
    dates=df["Date"].tolist()#get list
    print("Converting Date Strings to Floats...")
    new_dates=[float(re.sub(r"(\d{4})-(\d{2})-(\d{2})",r"\1.\2",d)) for d in dates]#retain year and month, make a float-like String and convert it to actual number
    new_years = [float(y) for y in years]
    df["Date_Float"]=new_dates
    df["Year_Float"]=new_years
    
    #####################################saving the df
    
    df.to_pickle('data/dblp.pickle')
    
    #print(df.head())
    
def read_arxiv(begin_scraping = '2020-02-10', arxiv_repo="cs"):
    #
    #Method to read an arxiv scrape. If there is no scrape yet, it will make one with respect to the parameters given above. 
    #begin_scraping: the date from which to scrape. '2020-02-10' will scrape from 10th Feb 2020 till the present date.
    #arxiv_repo="cs" will search the computer science part of the arXiv. Any other arxive can be chosen: e.g. 'math' or 'stat' for maths or statistics: https://arxiv.org/
    #
    #The scrape will be saved in the local working directory
    
    if os.path. exists('data/arxiv-dump.pkl'):
        df = pd.read_pickle('data/arxiv-dump.pkl')
    else:
        #begin_scraping = '2005-01-01'
        #10th of feb 2020!
        print("No local arxiv dump found. Scraping publications from date: "+ begin_scraping + ". Scraping arXiv repository: "+ arxiv_repo)
        scrape_arxiv(start= begin_scraping, arxiv_repo="cs")#scrapes all records that were published or changed after set start date. 
        df = pd.read_pickle('data/arxiv-dump.pkl')
        
    
    
    df.columns=['Title', 'ID', 'Abstract', 'Categories', 'DOIs', 'Created', 'Updated',
       'Authors', 'Affiliation', 'Url']#rename columns
    
    new_dates=[float(re.sub(r"(\d{4})-(\d{2})-(\d{2})",r"\1.\2",d)) for d in df['Created'].tolist()]#get only "publication" dates
    df["Date_Float"]=new_dates
    
    ti_abs = [entry[0] + " " + entry[1] for entry in zip(df['Title'].tolist(), df['Abstract'].tolist())]#make tiabs as searchable entity. sadly, arxiv has no keywords
    df["TiAbs"]=ti_abs
    
    #print('DF shape: {}. Dates length: {}'.format(df.shape, len(new_dates)))
    #print(df['Date_Float'])
    df.to_pickle('data/arxiv.pickle')
    
    
def load_pickled_db(db_fname="dblp.pickle"):
    print('Loading information from: {}'.format(db_fname))
    start = timer()
    df = pd.read_pickle(db_fname)
    end = timer()
    print('Time to read database information: {} seconds.'.format(int(end - start)))
    
    
    return df

def search_df(df, date_max=2020.03, date_min=2005.00, field="TiAbs", name=""):
    #
    #Method that implements the systematic search. Parameters define restrictions for the publication date. These have to be givin in Float form as above.
    #"field" parameter defines which variables will be searched. TiAbs is a concattenation of titles and abstracts, and can be searched for example with data from the arxiv, but not dblp as dblp onlt stores titles
    #
    #
    
    
    num_old= df.shape[0]
    ##############################################################################Drop by date###
    
    df_copy = df.copy()
    
    if 'Year_Float' in df.columns:
        df = df[ (df['Date_Float'] >= date_min) & (df['Year_Float'] >= date_min) & (df['Date_Float'] <= date_max) ]#retain only date values 
    else:
        df = df[ (df['Date_Float'] >= date_min) & (df['Date_Float'] <= date_max) ]#retain only date values, publication year is not available per se in in arxiv dump
    df_copy.drop(df.index, inplace = True)
    
    #df_copy.to_csv("excluded_dates.csv")
    print('Droppped {} abstracts out of {} due to publication date constraints.\nTotal number of entries to search now: {}'.format(num_old-df.shape[0],num_old, df.shape[0]))
    
    ########################################################################Drop by excluded expressions###
    #Systematic search: search using your regex clusters from file regex_helper.py
    #comment this code if you dont want the "NOT" search
    num_old= df.shape[0]
 	
    def regex_filter_titles(val):
        #if val:
            #print(type(val))
            for reg in cluster_NOT_titles:
                res = re.search(reg,val.strip())#get rid of genetics related search results
                if res:
                    #print(val)
                    return False#found bad string
            
            return True#keep entry becasue NOT search did not find a 'forbidden' string 
    def regex_filter_abstracts(val):
        #if val:
            #print(type(val))
            for reg in cluster_NOT_abstracts:
                res = re.search(reg,val.strip())#get rid of genetics related search results
                if res:
                    #print(val)
                    return False#found bad string
            
            return True#keep entry becasue NOT search did not find a 'forbidden' string     
        #else:
            #print("Title is empty: {}".format(val))
            #return False
        
    df_copy = df.copy()
    
    if "Title" in df.columns:
        df = df[df["Title"].apply(regex_filter_titles)]
        print("    ...excluding titles...")
    if "Abstract" in df.columns:   
        df = df[df["Abstract"].apply(regex_filter_abstracts)]
        print("    ...excluding abstracts...")
    
    df_copy.drop(df.index, inplace = True)
    print("Dropped {} records via the NOT search in titles/abstracts.\nCurrent nr. of records: {}".format(df_copy.shape[0],df.shape[0]))
    #df_copy.to_csv("excluded_via_NOT-search.csv")
    
    #####################################################################################################################
    #Systematic search: search using your regex clusters from file regex_helper.py
    #
    ####################################
    print("Searching {} records via the systematic regex search.".format(df.shape[0]))
    df_copy = df.copy()
    
    def regex_filter_c1(val):#filter for cluster 1. Returns True as soon as the first regex matches. If there is no match then the function returns a False, leading the record to be dropped
        
        found = False
        
        for reg in cluster_1:
            
            result = re.search(reg, val.strip(), re.IGNORECASE)
            if result:
                found= True
                #print("Found: {}\n Pattern: {}".format(val, reg))
                break
            
        if found:
            return True
        else:
            #print(val.strip())
            #print("------------")
            return False
        
        
    df = df[df[field].apply(regex_filter_c1)]#apply function defined above    
    df_copy.drop(df.index, inplace = True)
    print("Dropped {} records via the first search cluster.\nCurrent nr. of records: {}".format(df_copy.shape[0],df.shape[0]))
    #df_copy.to_csv("excluded_first.csv")
    #################################################Search each cluster: second cluster of terms
    df_copy = df.copy()
    def regex_filter_c2(val):
        
        found = False
        
        for reg in cluster_2:
            
            result = re.search(reg, val.strip())
            if result:
                found= True
                #print("Found: {}\n Pattern: {}".format(val, reg))
                break
            
        if found:
            return True
        else:
            return False
    
    df = df[df[field].apply(regex_filter_c2)]    
    df_copy.drop(df.index, inplace = True)
    print("Dropped {} records via the second search cluster.\nCurrent nr. of records: {}.\nWriting results to disk.".format(df_copy.shape[0],df.shape[0]))
    #df_copy.to_csv("excluded_second.csv")
    ##################
    #Add code for a third - nth cluster if needed. 
    
    
    ###############################save results
    df.to_csv("data/results_{}.csv".format(name))
    
    
    
def validate(path):
    print("Validating on the basis of data from: "+ path)
    with open(path) as fp:
        entries = list(readris(fp))
        
        
    
    titles = [entry['primary_title'] if 'primary_title' in entry else "" for entry in entries ]    
    #print(len(titles))
    
    abstracts = [entry['abstract'] if 'abstract' in entry else "" for entry in entries ]    
    #print(len(abstracts))
    
    keywords = [" ".join(entry['keywords']) if 'keywords' in entry else "" for entry in entries ]#joining so that list becomes sentence    
    
    IDs= [entry['id'] if 'id' in entry else "" for entry in entries ]
    #print(len(keywords))
    
    Date_Float = [float(entry['publication_year'].replace("//","")) if 'publication_year' in entry else "2020" for entry in entries ]
    #print(len(Date_Float))
    
    ti_abs_kw = [entry[0] + " " + entry[1] + " " + entry[2] for entry in zip(titles, abstracts, keywords)]#make tiabs as searchable entity
    
    #print(keywords[1])
    df = pd.DataFrame(list(zip(IDs, titles, Date_Float, abstracts, keywords, ti_abs_kw)), 
                   columns =['ID', 'Title', 'Date_Float', 'Abstract', "Keywords", "tiAbsKw"]) 
    
    print("Validating {} search results...".format(len(titles)))
    search_df(df, date_max=2020.03, date_min=2005.00, field="tiAbsKw")



#validate("C:\\Users\\xf18155\\OneDrive - University of Bristol\\MyFiles-Migrated\\Documents\\SR automation review\\Search\\ris (6).ris")    