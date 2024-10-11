import pandas as pd
import json
#############Parameters for User
mycsvpath=r'C:\Users\lena.schmidt\Documents\SR automation review\Update_2\includes2024_mapper.csv'#infile
myoutpath=r'C:\Users\lena.schmidt\Documents\SR automation review\Update_2\includes2024.json'#outfile
mapping_vars=["Entities mined","Data extraction approach", "Target data", "Scope of mining", "Granularity of extraction", "Validation scores", "Year"]#name of columns that have codes that should be transferred to map
delimit_str=","#if there are multiple codes in a column, specify the delimiter so that they may be splitted
nlreplace=";"#if there are random newlines (mostly in authors field, they get replaced with this
#####################Other Variables

def get_metadata(row, i):
    citation_vars = ["ItemId", "Title", "ParentTitle", "ShortTitle", "DateCreated", "CreatedBy",
                     "DateEdited", "EditedBy", "Year", "Month", "StandardNumber", "City", "Country",
                     "Publisher", "Institution", "Volume", "Pages", "Edition", "Issue", "Availability",
                     "URL", "OldItemId", "Abstract", "Comments", "TypeName", "Authors", "ParentAuthors",
                     "DOI", "Keywords", "ItemStatus", "ItemStatusTooltip",
                     "QuickCitation"]  # all these fields are required otherwise refs won't be downloadable from map. If there is no fitting column, an empty string "" will be added

    mycols=row.keys()
    refdict = {}
    refdict["Codes"] = []
    refdict["Outcomes"] = []
    if 'ItemId' not in mycols:
        refdict["ItemId"]=i#need to assign ID if not specified by user
        citation_vars.remove("ItemId")

    for cvar in citation_vars:#iterate through all required fields and fill them if possible from the row data
        if cvar in mycols:
            mv=str(row[cvar]).replace("\n", "; ").strip()  #
            if mv.endswith(nlreplace):
                mv=mv[:-1]
            refdict[cvar] =mv
        else:
            refdict[cvar] =""
    return refdict

def custom_json(mycsvpath, myoutpath):
    df=pd.read_csv(mycsvpath).fillna("")

    reflist = []  # contains dicts that have a list of codes and metadata
    attributeslist = []  # so many attributes lists
    attributes={k:{} for k in mapping_vars}#collecting all codes and assigning new ids to them as they are added to the dicts
    cnt=1#attributte ID counter

    for i, row in df.iterrows():##############################parse each reference (one row is one reference)
        extraction = []
        myattributes = []
        for mvar in mapping_vars:
            thisvar=str(row[mvar])#get the codes, split and strip whitespaces
            thiscodes=thisvar.split(delimit_str)
            thiscodes=[s.strip() for s in thiscodes]
            if "" in thiscodes:
                thiscodes.remove("")
            for c in thiscodes:
                if c not in attributes[mvar].keys():#assign code to the relevant attribute dict, every time a new code is discovered it gets a new ID and the counter is incremented. A list of attributes is saved for each reference
                    attributes[mvar][c] = cnt
                    myattributes.append(cnt)
                    cnt += 1
                else:
                    myattributes.append(attributes[mvar][c])

        #print(myattributes)


        refdict = get_metadata(row, i)#get metadata
        #print(refdict)

        codelist = []  ########
        for att in myattributes:#########################################################reformat thre reference-level attribute list to be correct format
            codelist.append({"AttributeId": att, "ItemAttributeFullTextDetails": []})
        refdict["Codes"] = codelist#assigne codes to ref
        reflist.append(refdict)#add reference to reflist
        # print(refdict)
        # print("-------------------------")

    for key, value in attributes.items():  ########################################## reformat global-level attributes list to correct format
        thisattributes = []
        for k, v in value.items():
            thisattributes.append({"AttributeId": v,
                                   "AttributeName": k})
        adict = {"AttributeId": cnt,
                 "AttributeName": key,
                 "Attributes": {
                     "AttributesList": thisattributes
                 }
                 }
        attributeslist.append(adict)
        cnt += 1#also these parent level attributes need ids so we need to increment

    ##########################################Yayyy, just need to fill the vars into the json tremplate :)
    final_json = {
        "CodeSets": [
            {
                "SetName": "Mapping tool",
                "Attributes": {
                    "AttributesList": attributeslist
                }
            }
        ],
        "References": reflist
    }

    with open(myoutpath, 'w', encoding='utf-8') as f:
        json.dump(final_json, f, ensure_ascii=False, indent=4)

custom_json(mycsvpath,myoutpath)
