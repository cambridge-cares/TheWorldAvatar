import csv,re,json

clean_list = [] 
with open('./results/003.csv','rb') as f:
    reader = csv.DictReader(f)
    counter = 0
    for row in reader:
        print(row)
        counter += 1
        # remove all the items without a proper label e.g. Q12345678
        if not re.match(r'Q[0-9]+', row['itemLabel']):
            clean_list.append(row)
    f.close()

# clean_list = [x for x in reader if re.match(r'Q[0-9]+', x.itemLabel)]
print(len(clean_list), 'out of', counter, 'are selected')
print(clean_list)

with open('./results/003_clean', 'w') as f1:
    f1.write(json.dumps(clean_list))
    f1.close()








    
