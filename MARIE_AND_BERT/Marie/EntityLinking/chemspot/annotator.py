from py4j.java_gateway import JavaGateway
#link to java module
#read from the list
#process one by one and write to output (save ckg also)
from util.EntityDict import load_entity_dict
import fuzzyset

# launch py4j gateway to lib-jar and expose it as a service
gateway = JavaGateway.launch_gateway(jarpath='chemspot/bin/py4j-0.10.9.5.jar', classpath='chemspot/bin/chemspot.jar', javaopts=['-Xmx16G'], java_path='C:/Program Files/Java/jdk1.8.0_111/jre/bin/java.exe', die_on_exit=True, use_shell=False)
# create instance of the class of interest within the jar

ChemSpotFactory = gateway.jvm.de.berlin.hu.chemspot.ChemSpotFactory()
tagger = ChemSpotFactory.createChemSpot("chemspot/bin/dict.zip", "chemspot/bin/ids.zip", "chemspot/bin/multiclass.bin")
def tag(text):
    raw = tagger.tag(text)
    mentions = []
    if len(raw)>0:
        for mention in raw:
            mentions.append((mention.getStart(), mention.getEnd(), mention.getText()))
        return mentions[0]
    #Currently assume one entity only
    else:
        return fallback_search(text)

def fallback_search(text):
    entitynames = load_entity_dict("data/pubchem/pubchem500.jsonl", True)
    entityset= fuzzyset.FuzzySet()
    for e in entitynames:
        entityset.add(e.lower())
    tokens = text.lower().strip().split()
    bestscore = 0
    midx = None
    #First scan for single token
    for idx,token in enumerate(tokens):
        score, entry = entityset.get(token)[0]
        if score > bestscore:
            bestscore, midx = score, idx
    #second scan for multiple token
    mstart = midx
    mend = midx+1
    tokens_len = len(tokens)
    possible_groups = [(midx, midx+2), (midx, midx+3), (midx-1, midx+1), (midx-2, midx+1),(midx-1, midx+2)]
    bestentry = tokens[midx]
    for start, end in possible_groups:
        start = max(0, start)
        end = min(tokens_len, end)
        tokeng = ' '.join(tokens[start:end])
        result = entityset.get(tokeng)
        if result is None:
            print('failed: token:{} text:{}'.format(tokeng, text))
            continue
        score, entry = result[0]
        if score >= bestscore:
            bestscore, bestentry = score, tokeng
            mstart, mend = start, end
    return mstart, mend, bestentry

if __name__=='__main__':
    #read a test file
    rawdata = "ner failed for what are all the functional groups in phenylisocyanate"
    tagged = fallback_search(rawdata)
    print(tagged)
    #model.infer(testdata)