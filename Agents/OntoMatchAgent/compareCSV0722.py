import readAlignment
import csv
from nltk.metrics.distance import jaro_similarity,edit_distance,jaro_winkler_similarity

reader = readAlignment.AReader('Test0908GEOValueMatcherOnly.owl')
reader.readAlignment(0.6)
def getName(i):
    strs = i.split('_')
    name = ''
    for i in range(1, len(strs)-1):
        name = name +' '+strs[i]

    name = name.replace('http://www.theworldavatar.com/kb/powsys/gppdb/','').replace('http://www.theworldavatar.com/kb/powsys/dukes/','').replace('_',' ').replace('DE','').strip()
    return name
#read csv

def getID(i):
    i = i.replace('http://www.theworldavatar.com/kb/powsys/gppd/','').replace('http://www.theworldavatar.com/kb/powsys/kwl/','').strip()
    i = i.replace('http://www.theworldavatar.com/kb/powsys/gppd/','').replace('http://www.theworldavatar.com/kb/powsys/kwl/','').strip()
    strs = i.split('_')
    return strs[0]

def compareName(a1, a2):
    s = jaro_winkler_similarity(a1, a2)
    if 1.0 - s < 0.000001:
        return True
    return False


type1counterA = 0
type2counterA = 0
type3counterA = 0

truth = []
with open('testFiles/csv/scores_kwl_20210908.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    for row in csv_reader:
        #idx_1,idx_2,link,type_score,primary_fuel_1,primary_fuel_2,name_score,name_1,name_2,unit_name_1,owner_score,owner_1,owner_2,cap_score,capacity_mw_1,capacity_mw_2

        if row[2] is '1' or row[2] is '2' or row[2] is '3':
            #truth.append((row[7], row[8], row[2],row[0],row[1]))
            truth.append(( row[2],row[0],row[1]))

            if row[2] is '1':
                type1counterA = type1counterA+1
            if row[2] is '2':
                type2counterA = type2counterA+1
            if row[2] is '3':
                type3counterA = type3counterA+1
real_list = []
truth_list = []
id_list = []
type1counter = 0
type2counter = 0
type3counter = 0
for m in reader.a.map:
    #for e1, e2, match,id1,id2 in truth:
    for match,id1,id2 in truth:
        if compareName(getID(m[0]), id1) and compareName(getID(m[1]), id2):
            real_list.append(m)
            id_list.append((id1,id2))
            truth_list.append((match,id1,id2))
            if match is '1':
                type1counter = type1counter+1
            if match is '2':
                type2counter = type2counter+1
            if match is '3':
                type3counter = type3counter+1
            #print(m[0]+" "+m[1]+' '+e1+' '+e2)

def notInList(v):
    if v in real_list:
        return False
    return True

def notInTruth(v):
    if v in truth_list:
        return False
    return True

found = len(reader.a.map)
print('precision:' + str(len(real_list)/found))
print('recall:'+str(len(real_list)/len(truth)))
print('overlap:'+str(len(real_list))+'match1: '+str(type1counter)+'match2: '+str(type2counter)+'match3: '+str(type3counter))
print('csv'+str(len(truth))+'match1: '+str(type1counterA)+'match2: '+str(type2counterA)+'match3: '+str(type3counterA))


false_neg = list(filter(notInTruth, truth))
false_pos = list(filter(notInList, reader.a.map))


print("num of false_neg:"+str(len(false_neg)))
print("num of false_pos:"+str(len(false_pos)))
print("false_pos:")









#print("false_pos:")

wpCount = 0
for i in false_pos:
    if 'windpark' in i[0].lower():
        wpCount = wpCount+1
    print(i)
print(wpCount)



'''
with open('Test0831GeoBugFixedStringThre6.csv','w') as out:
    csv_out=csv.writer(out)
    csv_out.writerows(false_neg)
'''