# -*- coding: utf-8 -*-
"""
Created on Tue Feb 11 10:49:37 2020

@author: Lena Schmidt, lena.schmidt@bristol.ac.uk

This file contains regular expressions which define a systematic search. Lines in each cluster will be connected via "OR" searches, while clusters themselves are connected via "AND". Performance is best if your first cluster is the cluster that is likely to produce the least hits. the "NOT" cluster defines terms that must not occur in any searched entity, similar to the NOT search in ovid. 
"""
cluster_NOT_titles = [
        r"\b([Gg]enes?|[Gg]enome|[Gg]enetic [Ee]xpression\w?)\b",
        r"\b[Gg]ene\w* [Ee]xpression\b"
    ]
cluster_NOT_abstracts = [
        r"\b[Gg]ene\w* [Ee]xpression\b"
    ]

cluster_1 = [
        r'\b([Ss]ystematic [Rr]eview)\b',
        r'\b[Rr]eview [Ll]iterature\b',
        r'\bClinical Trial [Ll]iterature\b',
        r'\bclinical research articles?\b',
        r'\babstracts of randomi[sz]ed clinical trials?\b',

        r'\b([Ss]ystematic\w*)( (\w+)){0,4} ([Rr]eview\w*|[Oo]verview\w*)\b|\b([Rr]eview\w*|[Oo]verview\w*)( (\w+)){0,4} ([Ss]ystematic\w*)\b',
        r'\b([Cc]omprehensive|[Ee]vidence|[Rr]esearch|[Ll]iterature) ([Rr]eview\w?|[Ss]ynthesis\w*)\b|\b([Rr]eview\w?|[Ss]ynthesis\w*) ([Cc]omprehensive|[Ee]vidence|[Rr]esearch|[Ll]iterature)\b',
        r'\b([Ii]ntegrative [Rr]esearch [Rr]eview\w*|[Rr]esearch [Ii]ntegration|[Ss]coping [Rr]eview)\b',
        r'\b([Qq]uantitativ\w*|[Mm]ethodologic\w*) ([Rr]eview|[Oo]verview|[Ss]ynthesis)\b|\b([Rr]eview|[Oo]verview|[Ss]ynthesis) ([Qq]uantitativ\w*|[Mm]ethodologic\w*)\b',
        r'\b([Aa]rticle\w?|[Cc]itation\w?|[Dd]ocument\w?|[Rr]ecord\w?)( (\w+)){0,2} [Ii]dentif\w*\b|\b[Ii]dentif\w*( (\w+)){0,2} ([Aa]rticle\w?|[Cc]itation\w?|[Dd]ocument\w?|[Rr]ecord\w?)\b',
        r'\b([Aa]bstracts|[Aa]rticle\w?|[Cc]itation\w?|[Dd]ocument\w?|[Rr]ecord\w?|[Tt]ask\w?)( (\w+)){0,2} [Ss]creen\w*\b|\b[Ss]creen\w*( (\w+)){0,2} ([Aa]bstracts|[Aa]rticle\w?|[Cc]itation\w?|[Dd]ocument\w?|[Rr]ecord\w?|[Tt]ask\w?)\b',
        r'\b([Aa]bstracts|[Aa]rticle\w?|[Cc]itation\w?|[Dd]ocument\w?|[Rr]ecord\w?|[Tt]ask\w?)( (\w+)){0,2} [Rr]etriev\w*\b|\b[Rr]etriev\w*( (\w+)){0,2} ([Aa]bstracts|[Aa]rticle\w?|[Cc]itation\w?|[Dd]ocument\w?|[Rr]ecord\w?|[Tt]ask\w?)\b',
        r'\b[Pp]([Ii]|[Ee])[Cc][Oo](s|-)?\b',
        r'\b[Ss]creening [Pp]rioriti.ation\b',
        r'\b(([Ss]creening [Rr]eferences)|([Rr]eference(s)? [Ss]creening))\b',
        r'\b((([Ss]creening|[Ss]canning) [Rr]eferences)|([Rr]eference(s)? ([Ss]creening|[Ss]canning)))\b',
        r'\b(([Cc]itation\w? [Mm]anag\w*)|([Mm]anag\w* [Cc]itation\w?))\b',


        r'\b[Ff]iltering [Ss]tudies\b',
        r'\b(PICOs?)( (\w+)){0,3} (classif\w*|detect\w*|retrieve\w*|element\w?|predict\w*|extract\w*)\b|\b(classif\w*|detect\w*|retrieve\w*|element\w?|predict\w*|extract\w*)( (\w+)){0,3} (PICOs?)\b',

        r'\b(((([Cc]linical )?[Tt]rial)|RCT|[Rr]andomi[sz]ed [Cc]ontrolled [Tt]rials?) ([Ll]iterature|[Dd]ocuments?|[tT]exts?))\b',
        r'\b([Pp]ub[Mm]ed) [Aa]bstracts?\b',
        r'\b[Mm]edical [Jj]ournal [Aa]bstracts?\b',
        r'\b[Ee]vidence[ -][Bb]ased[ -][Mm]edicine\b'
        ]

cluster_2 = [
        r'\b[Aa]utomat\w*( (\w+)){0,2} ([Ss]creening|[Rr]etrieval)\b',
        r'\b[Aa]utomat(ed|ing|ion) [Cc]lassification\b',
        r'\b[Aa]utomat(ed|ing|ion) extraction\b',
        r'\b[Ii]nformation [Rr]etrieval\b',
        r'\bStatistical Relational Learning\b',
        r'\b([Aa]utomat\w*( (\w+)){0,3} [Dd]ata( (\w+)){0,3} [Ee]xtract\w*)|([Aa]utomat\w*( (\w+)){0,3} [Ee]xtract\w*( (\w+)){0,3} [Dd]ata)\b',
        r'\b([Aa]utomation)\b',
        r'\b(automat(ed|ing|ion)|semi[- ]?automat(ed|ing|ion))\b',
        r'[Dd]ata[- ]?[Mm]ining',
        r'(\b(([Dd]ata|[Ll]iterature|[Tt]ext)[ -]?([Mm]ine.?|[Mm]ining))\b)|(\b(([Mm]ine.?|[Mm]ining)[ -]([Dd]ata|[Ll]iterature|[Tt]ext)?)\b)',
        r'\b(datamin\w*|textmin\w*)\b',
        r'(\b((text\w*|sequence\w?) classif\w*)\b)|(\b(classif\w* (text\w*|sequence\w?))\b)',
        r'(\b(text\w*[- ]?analys\w*)\b)|(\b(analys\w*[- ]?text\w*)\b)',
        r'\b([Mm]achine[ -]?[Ll]earning|[Dd]eep[ -]?[Ll]earning|[Ss]upervised [Mm]achine[ -]?[Ll]earning|[Uu]nsupervised [Mm]achine[ -]?[Ll]earning|[Nn]eural [Nn]etworks?)\b',
        r'\b([Mm]achine[ -]?[Ll]earn\w*)\b',
        r'\b([Ll]earning [Aa]lgorithm\w*)\b',
        r'\b((([Aa]ctive|[Dd]eep|[Ss]upervised|[Ss]emi-supervised|[Tt]ransfer|[Uu]nsupervised) [Ll]earning)|([Ll]earning ([Aa]ctive|[Dd]eep|[Ss]upervised|[Ss]emi-supervised|[Tt]ransfer|[Uu]nsupervised)))\b',
        r'\b[Cc]lustering [Tt]ool\b',
        r'\b(([Aa]rtificial|[Dd]eep) [Nn]eural [Nn]etwork\w?)|([Nn]eural [Nn]etwork\w? ([Aa]rtificial|[Dd]eep))\b',
        r'\b([Aa]rtificial[- ]?[Ii]ntelligence|[Aa][Ii])\b',
        r'\b([Aa]rtificial[- ]?[Ii]ntelligence|[Aa][Ii])\b',
        r'\b[Nn]atural [Ll]anguage [Pp]rocessing\b',
        r'\b([Nn]atural [Ll]anguage [Pp]rocessing)|[nN][Ll][Pp]\b',
        r'\b[Pp]redictive [Mm]odelling\b',
        r'\b[Qq]uestion [Aa]nswering|[Rr]eading [Cc]omprehension\b',
        r'\b[Nn]amed [Ee]ntity [Rr]ecognition\b',
        r'\b[kK]nowledge [Bb]ases\b',
        r'[kK]nowledge[- ]?[Bb]ase( (\w+)){0,50} (expert|database)|(expert|database)( (\w+)){0,50} [kK]nowledge[- ]?[Bb]ase',
        r'\b[Oo]ntolog(y|ies)\b',
        r'\b[sS]upport [Vv]ector [Mm]achine\b',
        r'\b([sS]upport [Vv]ector [Mm]achine\w?|[Ss][Vv][Mm])\b',
        r'\b([Tt]erm [Rr]ecognition|[Rr]egex\w?|[Rr]egular [Ee]xpression\w?)\b',
        r'\b[Tt]ext [Cc]luster\w*\b',
        r'\b([Ww]ord)( (\w+)){0,2} [Ff]requency( (\w+)){0,2} analys\w*\b|\b([Ww]ord)( (\w+)){0,2} analys\w*( (\w+)){0,2} ([Ww]ord)( (\w+)){0,2}  [Ff]requency\b|\banalys\w*( (\w+)){0,2} [Ww]ord( (\w+)){0,2} [Ff]requency\b',
        r'\bBERT|(Bidirectional Encoder Representations( (\w+)){0,2} Transformer\w?)\b',
        r'\b([Cc]onvolutional [Nn]eural [Nn]etwork\w?|CNN)\b',
        r'\b([Cc]onditional [Rr]andom [Ff]ields?|CRF)\b',
        r'\b[Dd]ecision [Tt]rees?\b',
        r'\b[Dd]ecision [Tt]ree\w?\b',
        r'\b[Dd]ocument [Cc]luster\w*\b',
        r'\b[Gg]enetic [Aa]lgorithms?\b',
        r'\b([Ll]atent [Dd]irichlet [Aa]llocation|[Tt]opic [Mm]odel\w*)\b',
        r'\b([Ll]long [Ss]hort-[Tt]erm [Mm]emory|LSTMs?|[Bb][Ii]-LSTMs?)\b',
        r'\b([Mm]aximum [Ee]ntropy [Cc]lassif\w*|[Mm]ax[- ]?[Ee]nt)\b',
        r'\b([Mm]ulti[- ]?[Ll]ayer[- ]?[Pp]erceptrons?)\b',
        r'\b([Nn]a[iï]ve [Bb]ayes)\b',
        r'\b([Rr]adial [Bb]asis [Ff]unction [Nn]etwork|RBFN)\b',
        r'\b([Rr]andom [Ff]orest)\b',
        r'\b([Rr]ecurr(ing|ent) [Nn]eural [Nn]etwork\w?)\b',
        r'\b([Rr]ecurr(ing|ent) [Nn]eural [Nn]etwork\w?|RNN)\b',
        r'\b[Rr]ule[- ]?[Bb]ased [Ll]earning\b',
        r'\b(SCIBERT|ALBERT|DistilBERT|SpanBERT|RoBERTa|XLNet|Transformer-XL)\b',
        r'\b([Ss]tatistical [Rr]elational [Ll]earning[- ][Bb]ased [Aa]pproach|[Kk]-?[lL]og)\b',
        r'\b([Ww]ord [Ee]mbedding|[Ww]ord2[Vv]ec)\b'
    ]


#print(cluster_1)
