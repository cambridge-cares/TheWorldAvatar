import csv
import json
import sys
import itertools

def readGst(filepath,heightnum, polnum):
    with open(filepath) as f:
        reader = csv.reader(f, delimiter=',')
        first = True
        outBHeight = []
        for idxH in range(0, heightnum):
            outBHeight.append([])#init empty arr
            for idxP in range(0, polnum):
                outBHeight[idxH].append({})


        for row in reader:
            
            if first:#skip first line
                first = False
                continue
            

            for idxH in range(0, heightnum):

                y, x, cons = row[5], row[4], row[7+polnum*idxH: 7+polnum*(idxH+1)] 
                for idxP in range(0, polnum):
                    if y not in outBHeight[idxH][idxP]:
                        outBHeight[idxH][idxP][y] = []
                    outBHeight[idxH][idxP][y].append(cons[idxP])    


        result = [[list(itertools.chain.from_iterable(ymap.values()))  for ymap in outBP] for outBP in outBHeight]


        


    with open ('tst.json', 'w') as f:
        f.write(json.dumps(result))
    #print(json.dumps(result))       
    return json.dumps(result)            

if __name__ == "__main__":#test
    filepath = sys.argv[1]
    h = int(sys.argv[2])
    p = int(sys.argv[3])
    result = readGst(filepath,h,p)
    print(result)
