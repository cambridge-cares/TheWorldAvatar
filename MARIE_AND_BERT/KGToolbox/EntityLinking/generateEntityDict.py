'''
Generate entity files for BLINK-NEL from pubchem csv
Add extra description from wikdata


'''


#LINK pubchem ID to wiki ID, generate pubchem.json style file
import csv, json
compounds_dict = {}

with open('../data/description.json', 'rb') as f:
    description = json.load(f)

def searchDescription(id):
    for line in description:
        if line['id'] == id:
            return line['text']
    return None

with open('../data/compounds_pubchem.json', 'rb') as f:
    compounds = json.load(f)
    for item in compounds:
        idx = item['pubchem']
        compounds_dict[idx] = item

with open('../data/compounds_pubchem_aliase.json', 'rb') as f:
    altlabels = json.load(f)


def searchAltLabel(id):
    for line in altlabels:
        if line['pubchem'] == id:
            return line['altLabel_list'].split('@')
    return []


def getbyid(id):
    return compounds_dict[id] if id in compounds_dict else None

def toolong(name):
    return True if len(name) >= 100  else False

#{"entity":"http://www.wikidata.org/entity/Q48318","entityLabel":"urea","pubchem":"1176","sitelink":"https://en.wikipedia.org/wiki/Urea"}


entries = []




with open('../data/pubchem.csv') as f:
    lines = csv.reader(f)
    next(lines)

    idxline = 0
    for line in lines:
        idx = line[0].replace('CID', '')
        formula = line[12]
        ipuacName = line[7]
        smile = line[15]
        jitem = getbyid(idx)
        entry = {}
        entry['idx'] = idx
        #entry['smile'] = smile
        #entry['iupac'] = ipuacName
        alias = searchAltLabel(idx)
        alias.append(formula)
        alias.append(smile)

        if jitem:
            entry['entity'] = jitem['entityLabel']
            entry['title'] = jitem['entityLabel']
            if not toolong(ipuacName):
                alias.append(ipuacName)
            if searchDescription(idx):
                entry['text'] = searchDescription(idx)
            else:
                entry['text'] = ''

        else:
            jitem = {}
            entry['entity'] = ipuacName
            entry['title'] = ipuacName
            entry['text'] = ''
        if entry['entity'] and not toolong(entry['entity']) :
            entries.append(entry)
            for alia in alias:
                aliasentry = entry.copy()
                aliasentry['entity'] = alia
                aliasentry['title'] = alia
                if entry['entity'] and not toolong(entry['entity']):
                    entries.append(aliasentry)
        #else:
        #    print(entry['entity'])
        idxline = idxline + 1
        if idxline == 5000 or int(idx)==5282 :
            break


#Write output to file
with open('pubchem5000withSMILE_asentity.jsonl', 'w') as outfile:
    for entry in entries:
        json.dump(entry, outfile)
        outfile.write('\n')