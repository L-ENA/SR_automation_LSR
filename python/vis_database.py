import pandas as pd
import plotly.express as px
import numpy as np
import datetime
import re
def read_db(path):
    df = pd.read_csv(path)
    print(df.head())
    return df

def make_bar(df, x_val, y_val, ti, lab_x,lab_y, path):
    fig = px.bar(df, x=x_val, y=y_val, template="simple_white",
                 title=ti,
                 labels={
                     "x": lab_x,
                     "y": lab_y,
                 },
                 barmode='stack',color_continuous_scale='viridis'
                 )

    # fig.add_bar(includes_fulltexts, x=years.index, y=years.values)
    fig.show()
    fig.write_image("{}_bar_{}.png".format(path, datetime.date.today()))
    fig.write_html("{}_bar_{}.html".format(path, datetime.date.today()))

def make_area(df, x_val, y_val, ti, lab_x,lab_y, path):
    fig = px.area(df, x=x_val, y=y_val, template="simple_white",
                 title=ti,
                 labels={
                     "x": lab_x,
                     "y": lab_y,
                 }
                 )


    fig.show()
    fig.write_image("{}_area_{}.png".format(path, datetime.date.today()))
    fig.write_html("{}_area_{}.html".format(path, datetime.date.today()))



def viz_ratings(includes_fulltexts, category_code, category_name, path):
    years = includes_fulltexts['q48'].value_counts().sort_index()
    title="{} in the included references".format(category_name)
    df = pd.DataFrame(columns=['Assessment', 'Year', 'Count'])
    for year in years.index:

        for answer in ["Yes", "No"]:
            subset = includes_fulltexts[(includes_fulltexts['q48'] == year) & (includes_fulltexts[category_code] == answer)]
            df = df.append({'Assessment': answer, 'Year': year, 'Count': subset.shape[0]}, ignore_index=True)
    df['Year']=[int(xx) for xx in df['Year']]
    fig = px.bar(df, x='Assessment', y='Count', template="simple_white",
                 title=title,
                 labels={
                     "x": 'Assessment',
                     "y": 'Count',
                 },
                 barmode='stack', color="Year", color_continuous_scale='Viridis'
                 )
    fig.update_traces(marker_line_color='rgb(0,0,0)',
                      marker_line_width=1)
    fig.show()
    category_name=category_name.replace(" ", "_")
    fig.write_image("{}_{}_{}.png".format(path, category_name,datetime.date.today()))
    fig.write_html("{}_{}_{}.html".format(path, category_name,datetime.date.today()))

def viz_categories(includes_fulltexts, category_code, category_name, path):
    years = includes_fulltexts['q48'].value_counts().sort_index()
    title="{} in the included references by year".format(category_name)
    data = includes_fulltexts[category_code].tolist()

    names = []

    for d in data:
        if type(d) != float:
            for method in d.split(", "):
                m=method.strip()
                if m != "":
                    names.append(method)
    names = pd.Series(data=names).value_counts().sort_index()
    print(names)
    df = pd.DataFrame(columns=['Assessment', 'Year', 'Count'])

    for year in years.index:

        for answer in names.index:
            #print(answer)
            subset = includes_fulltexts[(includes_fulltexts['q48'] == year) & (includes_fulltexts[category_code].str.contains(re.escape(answer[:12])))]
            df = df.append({'Assessment': answer, 'Year': year, 'Count': subset.shape[0]}, ignore_index=True)
    df['Year'] = [int(xx) for xx in df['Year']]
    fig = px.bar(df, x='Assessment', y='Count', template="simple_white",
                 title=title,
                 labels={
                     "x": 'Assessment',
                     "y": 'Count',
                 },
                 barmode='stack', color="Year"
                 )
    fig.update_traces(marker_line_color='rgb(0,0,0)',
                  marker_line_width=1)

    fig.update_layout(
        font=dict(

            size=25
            )
    )
    fig.show()
    category_name=category_name.replace(" ", "_")
    fig.write_image("{}_{}_{}.png".format(path, category_name,datetime.date.today()))
    fig.write_html("{}_{}_{}.html".format(path, category_name,datetime.date.today()))

def viz_pie(includes_fulltexts, category_code, category_name, path):
    #years = includes_fulltexts['q48'].value_counts().sort_index()
    title="{} in the included references".format(category_name)
    data=includes_fulltexts[category_code].tolist()

    names = []

    for d in data:
        if type(d) != float:
            for method in d.split(", "):
                m=method.strip()
                if m != "":
                    names.append(m)
    names=pd.Series(data=names).value_counts().sort_index()

    print(names)


    fig = px.pie(df, values=names.values, names=names.index, color_discrete_sequence=px.colors.sequential.RdBu, title=title)
    fig.update_traces(texttemplate='%{label}<br>n=%{value}')
    fig.update_traces(marker_line_color='rgb(0,0,0)',
                      marker_line_width=1)
    fig.update_layout(showlegend=False)
    fig.update_layout(
        autosize=False,
        width=1200,
        height=1500,
        font=dict(

            size=17
        )
    )

    fig.show()

    #
    category_name=category_name.replace(" ", "_")
    fig.write_image("{}_{}_{}.png".format(path, category_name,datetime.date.today()))
    fig.write_html("{}_{}_{}.html".format(path, category_name,datetime.date.today()))

def append_text(lines,includes_fulltext, title, item):

    lines.append("\n")
    lines.append(title)
    content=""
    for i,row in includes_fulltext.iterrows():
        if type(row[item]) != float:#if row is not empty then append the content
            content="{} -;- {} ({})".format(content, row[item], row['ID'])
    lines.append(content)
    print(content)
    lines.append("\n")

    return lines

def append_percentage(lines,includes_fulltext, title, item):##todo, finish output
    lines.append("\n")


    results= includes_fulltext[item].value_counts()
    total= results.values.sum()
    try:
        positive= results[results.index.get_loc('Yes')]
        percentage=round(positive/total, 2)
        lines.append('{}: {} out of {} positive ({}%)'.format(title, positive, total, percentage))
    except:
        lines.append('{}: {}'.format(title, 'Error for detecting positive answers, might be 0 due to filtering for updated data..'))

    lines.append("\n")
    return lines
def write_text(includes_fulltext):
    lines=[]
    lines.append("Analysis on the {}".format(datetime.date.today()))

    lines.append('-----DATA EXTRACTION:------\n')
    lines = append_text(lines, includes_fulltext, "2.2 Other outcome metrics and comments:", "q4")
    lines = append_text(lines, includes_fulltext, "7.1 More mined fields/coments:", "q10")
    lines = append_text(lines, includes_fulltext, "9. Author's conclusions:", "q47")

    lines.append('-----OTHER ASSESSMENTS:------\n')
    lines = append_percentage(lines, includes_fulltext, "1. sources for data reported:", "q14")
    lines = append_text(lines, includes_fulltext, "1.1 Describe the data source:", "q15")

    lines = append_percentage(lines, includes_fulltext, "2. Is there a description of the dataset used and of its characteristics?", "q17")
    lines = append_text(lines, includes_fulltext, "2.1 What exactly is described:", "q18")

    lines = append_percentage(lines, includes_fulltext,
                              "3. Were the data pre-processed prior to data extraction?", "q19")
    lines = append_text(lines, includes_fulltext, "3.1 If pre-processing techniques were applied to the data, how are they described:", "q20")

    lines = append_percentage(lines, includes_fulltext, "4. Is there a description of the algorithms used?", "q21")
    lines = append_text(lines, includes_fulltext, "4.1 Please specify, eg. are hyperparameters described:", "q22")

    lines = append_text(lines, includes_fulltext, "6.1 Links to implemented applications:", "q25")

    lines = append_percentage(lines, includes_fulltext, "8. Is there a justification/ an explanation of the model assessment?", "q28")
    lines = append_text(lines, includes_fulltext, "8.1 How is assessment described, eg. error analysis or real-life eval:", "q29")

    lines = append_percentage(lines, includes_fulltext,
                              "10. Does the assessment include any information about trade-offs, eg. recall and precision (also known as sensitivity and positive predictive value)? See bar chart also:", "q31")#difference in wording in protocols
    lines = append_text(lines, includes_fulltext,
                        "10.1 Please describe the trade-off:", "q32")

    lines = append_percentage(lines, includes_fulltext, "11. Is the use of third-party frameworks reported and are they accessible?", "q33")
    lines = append_text(lines, includes_fulltext, "11.1 Please specify framework:", "q34")

    lines = append_percentage(lines, includes_fulltext,
                              "12. Does the dataset or assessment measure provide a possibility to compare to other tools in same domain?", "q35")
    lines = append_text(lines, includes_fulltext,
                        "12.1 Please specify comparability:", "q36")

    lines = append_percentage(lines, includes_fulltext, "13. Is the influence of both visible and hidden variables in the dataset discussed?See bar graph. ", "q37")
    lines = append_text(lines, includes_fulltext, "1.1 Please specify:", "q38")

    lines = append_percentage(lines, includes_fulltext,
                              "14. Is the process of avoiding over- or underfitting described? N/A for regex/rule-base as they are 'overfitted' by design?", "q39")
    lines = append_text(lines, includes_fulltext,
                        "14.1 please specify:", "q40")

    lines = append_percentage(lines, includes_fulltext,
                              "Adaptibility to different formats beyond train or test",
                              "q42")
    lines = append_text(lines, includes_fulltext,
                        "1Adaptibility please specify:", "q43")

    lines = append_text(lines, includes_fulltext,
                        "17. Does the paper describe caveats/challenges for using the method, or, for using automated data extraction in general?", "q44")

    with open('figures/auto_results_{}.txt'.format(datetime.date.today()), 'w', encoding="utf-8") as file:
        file.writelines(lines)
    #print(lines)



# bar 12. 3comparability in terms of entities or assessment to other tools
# 12.1 description of comparability

# 13.1 description of hidden variables

# bar 14 overfitting
# 14.1 description of overfitting

# 17 vaveats, maybe word cloud
def make_prisma(includes_abstracts, includes_fulltexts, df):
    extra_info=pd.read_csv('extra_info.csv')
    #duplicates= pd.read_csv('dedupe_report.csv')
    reasons=includes_abstracts["exclusion_reason"].value_counts(ascending=False)
    reasons["Not fitting target entities or data"]=reasons["Not fitting target entities or data"]-1

    excluded_text="\n".join(["{}: {}".format(index, reasons[index]) for index in reasons.index])



    dupes_removed=extra_info['duplicates_base'][0]#add number of lines from dedupe report once system is live
    added_records = extra_info['hand_search'][0]


    included_abs=includes_abstracts.shape[0]
    included_full = includes_fulltexts.shape[0]
    all_screened= df.shape[0]

    flow="Records identified through database searching (n= {})\nAdditional records identified through other sources (n= {})\n\nRecords after duplicates removed (n = {})\n\nAbstracts screened (n = {})\nAbstracts excluded (n = {})\n\nFull-text articles assessed for eligibility (n= {})\nFull texts excluded for the following reasons:{}\n\nArticles included in living review (n={})".format(all_screened+dupes_removed, added_records, all_screened, all_screened, all_screened-included_abs,included_abs, excluded_text,included_full)
    print(flow)
def make_plots(df):
    ##df[df['ids'].str.contains('ball', na = False)]
    ##df[(df['year'] > 2012) & (df['reports'] < 30)]
    ##df[df['expert_decision'] > 2012]

    includes_abstracts= df[(df['expert_decision'] == 'Exclude') | (df['expert_decision'] == 'Include')].sort_values('q48',ascending=True)
    includes_fulltexts= df[df['expert_decision'] == 'Include'].sort_values('q48',ascending=True)
    print(includes_abstracts.shape)
    print(includes_fulltexts.shape)
    ########################################Year plots
    years=includes_fulltexts['q48'].value_counts().sort_index()
    years_added=np.cumsum(years.values)
    print(years.values)
    print(years.index)

    make_bar(df, years.index, years.values, "Number of included references by year of publication", "Year", "Number of references", "figures/year")
    make_area(df, years.index, years_added, "Cumulative SR data extraction literature", "Year", "Number of references", "figures/year")
    viz_ratings(includes_fulltexts, 'q16', 'Data availability', 'figures/bar')
    viz_ratings(includes_fulltexts, 'q24', 'Code availability', 'figures/bar')
    viz_ratings(includes_fulltexts, 'q26', 'App or user interface availability', 'figures/bar')
    viz_ratings(includes_fulltexts, 'q21', 'Algorithm description availability', 'figures/bar')
    viz_ratings(includes_fulltexts, 'q33', 'Third-party frameworks discussed', 'figures/bar')
    viz_pie(includes_fulltexts, 'q1', 'System architectures', 'figures/pie')
    viz_categories(includes_fulltexts, 'q1', 'System architectures', 'figures/bar')
    viz_pie(includes_fulltexts, 'q7', 'System input', 'figures/pie')
    viz_pie(includes_fulltexts, 'q8', 'System output', 'figures/pie')
    viz_pie(includes_fulltexts, 'q6', 'Target text for data extraction', 'figures/pie')
    viz_pie(includes_fulltexts, 'q9', 'Target entity for data extraction', 'figures/pie')

    viz_categories(includes_fulltexts, 'q11', 'Granularity of data extraction', 'figures/bar')
    viz_categories(includes_fulltexts, 'q3', 'Assessment metrics', 'figures/bar')#check consistency, add other tag
    viz_categories(includes_fulltexts, 'q5', 'Document type mined', 'figures/bar')
    viz_pie(includes_fulltexts, 'q30', 'Basic results reported', 'figures/pie')
    viz_categories(includes_fulltexts, 'q23', 'Reporting of hardware or computing time', 'figures/bar')

    viz_pie(includes_fulltexts, 'q31', 'Description of trade-offs in precision and recall', 'figures/pie')
    viz_categories(includes_fulltexts, 'q37', 'Description of hidden variables', 'figures/bar')
    viz_pie(includes_fulltexts, 'q37', 'Description of hidden variables', 'figures/pie')
    viz_pie(includes_fulltexts, 'q41', 'Splitting train from test data', 'figures/pie')
    viz_categories(includes_fulltexts, 'q42', 'Evaluation on multiple domains or corpora', 'figures/bar')#check consistency, if only discussion or actual evaluation
    viz_categories(includes_fulltexts, 'q45', 'Sources of funding described', 'figures/bar')
    viz_categories(includes_fulltexts, 'q46', 'Conflict of interest described', 'figures/bar')

    viz_categories(includes_fulltexts, 'LLM application', 'Strategy of LLM application', 'figures/bar')
    viz_categories(includes_fulltexts, 'LLM Runs', 'Repeated runs for reproducibility', 'figures/bar')
    viz_categories(includes_fulltexts, 'LLM Prompt', 'Prompts described and available', 'figures/bar')




    ########################quality plots

    #write_text(includes_fulltexts)

    #make_prisma(includes_abstracts, includes_fulltexts, df)#extra info file
print('reAD')
#df = read_db('all_screened_2023.csv')
df = read_db(r'C:\Users\lena.schmidt\Documents\SR automation review\Update_2\includes2024_cleaner.csv')

print('plots')
make_plots(df)
