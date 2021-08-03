import pandas as pd
from RISparser import readris
filepath = 'Eppi_ris.txt'
Title=[]
ID=[]
Abstract=[]

DOIs=[]
Created=[]

Authors=[]
Categories=[]

Url=[]


with open(filepath, 'r', encoding='utf-8') as bibliography_file:
    entries = readris(bibliography_file)

    counter=0
    for e in entries:
        Categories.append('')
        counter+=1
        Title.append(e.get('primary_title',''))
        ID.append(counter)
        Abstract.append(e.get('abstract',''))

        do=e.get('doi','')
        DOIs.append(do)
        if do != '':
            Url.append('https://doi.org/{}'.format(do))
        else:
            Url.append(e.get('url',''))

        Created.append(e.get('year',''))

        Authors.append('; '.join(e.get('first_authors',[])))





#
df= pd.DataFrame(columns=['Title', 'ID', 'Abstract',  'DOIs', 'Created', 'Authors', 'Url', 'Categories'])
df['Title']=Title
df['ID']=ID
df['Abstract']=Abstract
df['DOIs']=DOIs
df['Created']=Created
df['Authors']=Authors
df['Url']=Url
df['Categories']=Categories

df.to_csv('eppi_MA.csv', index=False)




