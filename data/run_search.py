# -*- coding: utf-8 -*-
"""
Created on Mon Dec 16 15:50:58 2019

@author: Lena Schmidt, lena.schmidt@bristol.ac.uk

a conda spec file to create an environment with all necessary libraries (win - 64) is provided in the Git repo. 

NB: if you want to search the dblp, please download the database and its dtd file and unzip it in your working directory: https://dblp.uni-trier.de/faq/How+can+I+download+the+whole+dblp+dataset

If you want to search the arxiv, a dump will be automatically created. Dates from when to scrape are defined in the optional parameter of the read_arxiv(begin_scraping = '2020-02-10', arxiv_repo ="cs") method: here scraping starts on thr 10th of Feb 2020, and we scrape the cs arxiv

With many thanks to Mahdi Sadjadi (sadjadi.seyedmahdi[AT]gmail[DOT]com) for the arxiv scraping code: https://github.com/Mahdisadjadi/arxivscraper
"""

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
# import xml.dom.minidom
import random

from lxml.etree import tostring
from itertools import chain

import scrape as arxivscraper  # import the scrape script from the local directory (make sure that the file is present!)
from RISparser import readris  # for validation only##attribute!
from RISparser.config import TAG_KEY_MAPPING

from regex_helper import cluster_1, cluster_2, cluster_NOT_titles, cluster_NOT_abstracts

random.seed(30)


def scrape_arxiv(start='2005-01-01', arxiv_repo="cs"):
    print("Scraping the " + arxiv_repo + " arXiv... Depending on the starting date this can take a while. ")

    def getRecords(start):
        scraper = arxivscraper.Scraper(category=arxiv_repo,
                                       date_from=start)  # usage of "until" is discouraged, see: https://arxiv.org/help/oa/index @ Datestamps section
        output = scraper.scrape()

        cols = output[0].keys()
        df = pd.DataFrame(output, columns=cols)

        return df

    # untilX='2010-01-10'

    df = getRecords(start)

    print('Done scraping arxiv from {}, found {} records'.format(start, df.shape[0]))

    df.to_pickle('arxiv-dump.pkl')
    # df.to_csv(
    #     'arxiv-dump.csv')  # note that the IDs are automatically converted to floats, which means that leading or trailing zeros are removed. This does not happen when pickeling, pls use pickle file if IDs are of interest


def read_dblp(min_df_year=1950):
    #
    # This method reads your local dblp file. Obtain it via: https://dblp.uni-trier.de/faq/How+can+I+download+the+whole+dblp+dataset
    # Also obtain "dblp.dtd"from the same place
    #
    # The method then parses the dblp xml and extracts the information that we will search.
    #
    # Method needs to be run only once for each dump, as it takes a long time. A pickle file will be exported in the end, and all subsequent methods will simply use this pickle file

    frac = 0.05  # ony for testing, uncomment manually below if needed
    if os.path.exists('dblp.xml'):
        print("success")

    if os.path.exists("dblp.xml"):
        pass
    else:
        print(
            "File dblp.xml not found in current directory. Please download it (and dblp.dtd) from University of Trier: https://dblp.uni-trier.de/faq/How+can+I+download+the+whole+dblp+dataset")
        return

    dtd = ET.DTD("dblp.dtd")  # pylint: disable=E1101

    # get an iterable
    context = ET.iterparse("dblp.xml", events=('start', 'end'), load_dtd=True,  # pylint: disable=E1101
                           resolve_entities=True)

    # turn it into an iterator
    context = iter(context)

    # get the root element
    event, root = next(context)

    mystr = ET.tostring(root)
    mystr = mystr.splitlines()
    # for stri in mystr:
    # print(stri)

    n_records_parsed = 0

    dblp_record_types_for_publications = ('article', 'inproceedings', 'proceedings', 'book', 'incollection',
                                          'phdthesis', 'masterthesis', 'www')
    years = []
    titles = []
    IDs = []
    dates = []
    dois = []
    authors = []
    no_yr = 0
    no_ttl = 0

    children = total = len(root.getchildren())

    for event, elem in tqdm(context, position=0, leave=True):
        if event == 'end' and elem.tag in dblp_record_types_for_publications:
         #if random.random()< frac:
            t = [title.text for title in elem.findall('title')]
            if (len(t) > 0 and t[
                0] != "Home Page"):  # exclude the home-page entries, they have no content. Exclude empty titles, there is nothing to search

                if (len(t) == 1):

                    if t[0] == None:  # dealing with sub/superscript and other issues, simply taking all text content
                        title_str = ""
                        for title in elem.findall('title'):
                            for c in title.getchildren():
                                if c.text == None:
                                    c.text = ""
                                if c.tail == None:
                                    c.tail = ""

                                title_str = title_str + c.text + c.tail

                        t[0] = title_str

                    titles.append(t[0])


                else:
                    print("Multiple titles found")
                    print(t)
                    titles.append(" ".join(tit for tit in t))

                yr = [year.text for year in elem.findall('year')]
                if (len(yr) == 0):
                    print("No year for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                    years.append('2024')

                    no_yr += 1
                elif (len(yr) == 1):
                    years.append(yr[0])
                    # print(yr[0])
                    # print("Convert {}, {} to {}, {} ".format(yr[0], type(yr[0]), int(yr[0]), type(int(yr[0]))))

                else:
                    print("Multiple years for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                    print(yr)

                    years.append(yr[0])

                doi = [d.text for d in elem.findall('ee')]
                if (len(doi) == 0):
                    # print("No DOI for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                    dois.append('n/a')


                elif (len(doi) == 1):
                    dois.append(doi[0])
                    # print("Convert {}, {} to {}, {} ".format(yr[0], type(yr[0]), int(yr[0]), type(int(yr[0]))))

                else:
                    # print("Multiple dois for entry: https://dblp.org/rec/{}.html".format(elem.attrib['key']))
                    # print(doi)

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
    print("Nr. of records parsed: {}".format(n_records_parsed))
    print("Nr. titles: {}".format(len(titles)))
    print("Nr. years: {}".format(len(years)))
    print("Nr. dates: {}".format(len(dates)))
    print("Nr. IDs: {}".format(len(IDs)))
    print("Nr. DOIs: {}".format(len(dois)))
    print("Nr. authors: {}".format(len(authors)))

    print("No title: {}, No Year: {}".format(no_ttl, no_yr))
    doi = []

    for d in dois:  # need to clean links to get doi
        if "; " not in d:
            if "https://doi.org/" in d:  # only 1 entry
                d = d.replace("https://doi.org/", "")
                doi.append(d)
            else:
                doi.append("")
        else:  # there is a list of links
            # print(d)
            result = ""
            for x in d.split(" ; "):
                if "https://doi.org/" in x:  # try to squeeze doi from every link
                    result = x.replace("https://doi.org/", "")
            # print(result)
            doi.append(result)  # appen the retrieved doi, or empty string if none found

    df = pd.DataFrame(list(zip(IDs, titles, dois, doi, authors, years, dates)),
                      columns=['ID', 'Title', 'DOI_link', "DOI", "Authors", 'Year', 'Date'])

    #####################################some processing
    dates = df["Date"].tolist()  # get list
    print("Converting Date Strings to Floats...")
    new_dates = [float(re.sub(r"(\d{4})-(\d{2})-(\d{2})", r"\1.\2", d)) for d in
                 dates]  # retain year and month, make a float-like String and convert it to actual number
    new_years = [float(y) for y in years]
    df["Date_Float"] = new_dates
    df["Year_Float"] = new_years



    df=df[df["Year_Float"] >= min_df_year]

    #####################################saving the df

    df.to_pickle('dblp.pickle')

    # print(df.head())


def read_arxiv(begin_scraping='2020-02-10', arxiv_repo="cs",use_existing=False):
    #
    # Method to read an arxiv scrape. If there is no scrape yet, it will make one with respect to the parameters given above.
    # begin_scraping: the date from which to scrape. '2020-02-10' will scrape from 10th Feb 2020 till the present date.
    # arxiv_repo="cs" will search the computer science part of the arXiv. Any other arxive can be chosen: e.g. 'math' or 'stat' for maths or statistics: https://arxiv.org/
    #
    # The scrape will be saved in the local working directory

    if os.path.exists('arxiv-dump.pkl') and use_existing:
        df = pd.read_pickle('arxiv-dump.pkl')
    else:
        # begin_scraping = '2005-01-01'
        # 10th of feb 2020!
        print(
            "No local arxiv dump found, or not using it due to parameter use_existing=False. Scraping publications from date: " + begin_scraping + ". Scraping arXiv repository: " + arxiv_repo)
        scrape_arxiv(start=begin_scraping,
                     arxiv_repo="cs")  # scrapes all records that were published or changed after set start date.
        df = pd.read_pickle('arxiv-dump.pkl')

    df.columns = ['Title', 'ID', 'Abstract', 'Categories', 'DOIs', 'Created', 'Updated',
                  'Authors', 'Affiliation', 'Url']  # rename columns

    new_dates = [float(re.sub(r"(\d{4})-(\d{2})-(\d{2})", r"\1.\2", d)) for d in
                 df['Created'].tolist()]  # get only "publication" dates
    df["Date_Float"] = new_dates

    ti_abs = [entry[0] + " " + entry[1] for entry in zip(df['Title'].tolist(), df[
        'Abstract'].tolist())]  # make tiabs as searchable entity. sadly, arxiv has no keywords
    df["TiAbs"] = ti_abs

    # print('DF shape: {}. Dates length: {}'.format(df.shape, len(new_dates)))
    # print(df['Date_Float'])
    df.to_pickle('arxiv.pickle')


def load_pickled_db(db_fname="dblp.pickle"):
    print('Loading information from: {}'.format(db_fname))
    start = timer()
    df = pd.read_pickle(db_fname)
    end = timer()
    print('Time to read database information: {} seconds.'.format(int(end - start)))

    return df


def search_df(df, date_max=2020.03, date_min=2005.00, field="TiAbs", name=""):
    #
    # Method that implements the systematic search. Parameters define restrictions for the publication date. These have to be givin in Float form as above.
    # "field" parameter defines which variables will be searched. TiAbs is a concattenation of titles and abstracts, and can be searched for example with data from the arxiv, but not dblp as dblp onlt stores titles
    #
    #

    num_old = df.shape[0]
    ##############################################################################Drop by date###

    df_copy = df.copy()

    if 'Year_Float' in df.columns:
        df = df[(df['Date_Float'] >= date_min) & (df['Year_Float'] >= date_min) & (
                    df['Date_Float'] <= date_max)]  # retain only date values
    else:
        df = df[(df['Date_Float'] >= date_min) & (df[
                                                      'Date_Float'] <= date_max)]  # retain only date values, publication year is not available per se in in arxiv dump
    df_copy.drop(df.index, inplace=True)

    # df_copy.to_csv("excluded_dates.csv")
    print(
        'Droppped {} abstracts out of {} due to publication date constraints.\nTotal number of entries to search now: {}'.format(
            num_old - df.shape[0], num_old, df.shape[0]))

    ########################################################################Drop by excluded expressions###
    # Systematic search: search using your regex clusters from file regex_helper.py
    # comment this code if you dont want the "NOT" search
    num_old = df.shape[0]

    def regex_filter_titles(val):
        # if val:
        # print(type(val))
        for reg in cluster_NOT_titles:
            res = re.search(reg, val.strip())  # get rid of genetics related search results
            if res:
                # print(val)
                return False  # found bad string

        return True  # keep entry becasue NOT search did not find a 'forbidden' string

    def regex_filter_abstracts(val):
        # if val:
        # print(type(val))
        for reg in cluster_NOT_abstracts:
            res = re.search(reg, val.strip())  # get rid of genetics related search results
            if res:
                # print(val)
                return False  # found bad string

        return True  # keep entry becasue NOT search did not find a 'forbidden' string

    # else:
    # print("Title is empty: {}".format(val))
    # return False

    df_copy = df.copy()

    if "Title" in df.columns:
        df = df[df["Title"].apply(regex_filter_titles)]
        print("    ...excluding titles...")
    if "Abstract" in df.columns:
        df = df[df["Abstract"].apply(regex_filter_abstracts)]
        print("    ...excluding abstracts...")

    df_copy.drop(df.index, inplace=True)
    print("Dropped {} records via the NOT search in titles/abstracts.\nCurrent nr. of records: {}".format(
        df_copy.shape[0], df.shape[0]))
    # df_copy.to_csv("excluded_via_NOT-search.csv")

    #####################################################################################################################
    # Systematic search: search using your regex clusters from file regex_helper.py
    #
    ####################################
    print("Searching {} records via the systematic regex search.".format(df.shape[0]))
    df_copy = df.copy()

    def regex_filter_c1(
            val):  # filter for cluster 1. Returns True as soon as the first regex matches. If there is no match then the function returns a False, leading the record to be dropped

        found = False

        for reg in cluster_1:

            result = re.search(reg, val.strip(), re.IGNORECASE)
            if result:
                found = True
                # print("Found: {}\n Pattern: {}".format(val, reg))
                break

        if found:
            return True
        else:
            # print(val.strip())
            # print("------------")
            return False

    df = df[df[field].apply(regex_filter_c1)]  # apply function defined above
    df_copy.drop(df.index, inplace=True)
    print("Dropped {} records via the first search cluster.\nCurrent nr. of records: {}".format(df_copy.shape[0],
                                                                                                df.shape[0]))
    ###df_copy.to_csv("dblp_excluded_first.csv")
    #################################################Search each cluster: second cluster of terms
    df_copy = df.copy()

    def regex_filter_c2(val):

        found = False

        for reg in cluster_2:

            result = re.search(reg, val.strip(), re.IGNORECASE)
            if result:
                found = True
                # print("Found: {}\n Pattern: {}".format(val, reg))
                break

        if found:
            return True
        else:
            return False

    df = df[df[field].apply(regex_filter_c2)]
    df_copy.drop(df.index, inplace=True)
    print(
        "Dropped {} records via the second search cluster.\nCurrent nr. of records: {}.\nWriting results to disk.".format(
            df_copy.shape[0], df.shape[0]))
    ###df_copy.to_csv("dblp_excluded_second.csv")
    ##################
    # Add code for a third - nth cluster if needed.

    ###############################save results
    df.to_csv("results_{}.csv".format(name))


def validate(path):
    print("Validating on the basis of data from: " + path)
    with open(path) as fp:
        entries = list(readris(fp))

    titles = [entry['primary_title'] if 'primary_title' in entry else "" for entry in entries]
    # print(len(titles))

    abstracts = [entry['abstract'] if 'abstract' in entry else "" for entry in entries]
    # print(len(abstracts))

    keywords = [" ".join(entry['keywords']) if 'keywords' in entry else "" for entry in
                entries]  # joining so that list becomes sentence

    IDs = [entry['id'] if 'id' in entry else "" for entry in entries]
    # print(len(keywords))

    Date_Float = [float(entry['publication_year'].replace("//", "")) if 'publication_year' in entry else "2020" for
                  entry in entries]
    # print(len(Date_Float))

    ti_abs_kw = [entry[0] + " " + entry[1] + " " + entry[2] for entry in
                 zip(titles, abstracts, keywords)]  # make tiabs as searchable entity

    # print(keywords[1])
    df = pd.DataFrame(list(zip(IDs, titles, Date_Float, abstracts, keywords, ti_abs_kw)),
                      columns=['ID', 'Title', 'Date_Float', 'Abstract', "Keywords", "tiAbsKw"])

    print("Validating {} search results...".format(len(titles)))
    search_df(df, date_max=2020.03, date_min=2005.00, field="tiAbsKw")


import os
import requests
import gzip
import shutil

def update_acl_dblp_dumps():

    print('Getting acl data...')
    acl_url='https://aclanthology.org/anthology+abstracts.bib.gz'
    acl_target='acl.bib.gz'
    r = requests.get(acl_url, allow_redirects=True)
    print('Writing acl data...')
    open(acl_target, 'wb').write(r.content)

    print('Getting dblp data...')
    dblp_url = 'https://dblp.uni-trier.de/xml/dblp.xml.gz'
    dblp_target='dblp.xml.gz'
    r = requests.get(dblp_url, allow_redirects=True)
    print('Writing dblp data')
    open(dblp_target, 'wb').write(r.content)

    dblp_extracted='dblp.xml'
    acl_extracted = 'acl.bib'

    print('Extracting data...')
    with gzip.open(acl_target, 'rb') as f_in:
        with open(acl_extracted, 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)
    with gzip.open(dblp_target, 'rb') as f_in:
        with open(dblp_extracted, 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)




import bibtexparser

def read_acl(min_date=1920.11):
    print('Reading ACL file')
    with open('acl.bib', encoding='utf-8') as bibtex_file:
        bib_database = bibtexparser.bparser.BibTexParser(common_strings=True).parse_file(bibtex_file)

    print(bib_database.entries[0:10])

    ID=[]
    Title=[]
    DOI_link=[]
    Url=[]
    Abstract=[]
    DOI=[]
    Authors=[]
    Year=[]
    Date=[]
    Date_Float=[]
    Year_Float=[]
    TiAbs=[]

    num_found=0
    no_year_found=0
    for e in bib_database.entries:
        try:
            year=int(e['year'])
        except:
            print('Pubyear not found')
            year=2044
            no_year_found+=1

        month = e.get('month', '').lower()
        month_float=0
        no_month_found=0

        if month=='jan':
            month_float = 0.01
        elif month=='feb':
            month_float = 0.02
        elif month=='mar':
            month_float = 0.03
        elif month=='apr':
            month_float = 0.04
        elif month=='may':
            month_float = 0.05
        elif month=='jun':
            month_float = 0.06
        elif month=='jul':
            month_float = 0.07
        elif month=='aug':
            month_float = 0.08
        elif month=='sep':
            month_float = 0.09
        elif month=='oct':
            month_float = 0.10
        elif month=='nov':
            month_float = 0.11
        elif month=='dec':
            month_float = 0.12
        else:
            month_float=0.13#higher float so that it is caught under min_date

            no_month_found+=1
        date=year+month_float

        if date>min_date:
            num_found+=1
            Year.append(year)
            Date.append(str(date))
            Date_Float.append(date)
            Year_Float.append(float(year))
            ID.append(e.get('ID', ''))
            ti=e.get('title', '').replace('{\\','').replace('{','').replace('}','').replace('{\\\"','')
            Title.append(ti)

            Url.append(e.get('url', ''))
            ab=e.get('abstract', '').replace('{\\','').replace('{','').replace('}','').replace('{\\\"','')
            Abstract.append(ab)
            TiAbs.append('{} {}'.format(ti,ab).strip())

            doi=e.get('doi', '')
            DOI.append(doi)
            if len(doi)>1 and 'https://' not in doi and 'www.' not in doi and 'http://' not in doi:
                DOI_link.append('https://doi.org/{}'.format(doi))
            else:
                DOI_link.append('')

            Authors.append(e.get('author', '').replace('  and\n', '; ').replace('{\\','').replace('{','').replace('}','').replace('{\\\"',''))

    df=pd.DataFrame(columns=  [
    'ID',
    'Title',
    'DOI_link',
    'Url',
    'Abstract',
    'TiAbs',
    'DOI',
    'Authors',
    'Year',
    'Date',
    'Date_Float',
    'Year_Float'])

    df['ID']=ID
    df['Title']=Title
    df['TiAbs'] = TiAbs
    df['DOI_link']=DOI_link
    df['Url']=Url
    df['Abstract']=Abstract
    df['DOI']=DOI
    df['Authors']=Authors
    df['Year']=Year
    df['Date']=Date
    df['Date_Float']=Date_Float
    df['Year_Float']=Year_Float

    df.to_pickle('acl.pickle')
    print('Found {} acl entries'.format(num_found))

max_date=2023.05
min_date=2021.08
last_updated='2020-08-02'#for arxiv scraping

update_acl_dblp_dumps()# acl and dblp are online databases that are not very well searchable in a systematic or living way, but this method pulls dumps from the mose tu to date versions
print('Dealing with dblp data')
read_dblp(min_df_year=2021)#read the dblp export dump##is uncommented because reading and parsing the whole database takes a while#NB: if you want to search the dblp, please download the database and its dtd file and unzip it in your working directory: https://dblp.uni-trier.de/faq/How+can+I+download+the+whole+dblp+dataset
my_df = load_pickled_db('dblp.pickle')#25 45 secs
print('Selected database contains the following (searchable) fields: {}'.format(my_df.columns))
search_df(my_df, date_max=max_date, date_min=min_date, field='Title', name="dblp")#dblp has only titles unfortunately#search the specified fild with the pre-defined systematic search and save results to spreadsheet

#
print("Dealing with arxiv data")
read_arxiv(begin_scraping = last_updated)#reading the local arxiv dump. creates a dump if none is found. Pass optional parameter to define date from which to scrape and refer to method for more info
my_df = load_pickled_db('arxiv.pickle')
print('Selected database contains the following (searchable) fields: {}'.format(my_df.columns))
search_df(my_df, date_max=max_date, date_min=min_date, field='TiAbs', name="arxiv")#search Titles and abstracts, in records between 2005 and March 2020

read_acl()
my_df = load_pickled_db('acl.pickle')
print(list(my_df.columns))
search_df(my_df, date_max=max_date, date_min=min_date, field='TiAbs', name="acl")

#ssearch.validate("C:\\Users\\xf18155\\OneDrive - University of Bristol\\MyFiles-Migrated\\Documents\\SR automation review\\Search\\ris (6).ris")