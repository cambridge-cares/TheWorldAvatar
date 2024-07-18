from py4j.java_gateway import JavaGateway
# link to java module
# read from the list
# process one by one and write to output (save ckg also)
from Marie.EntityLinking.util.EntityDict import load_entity_dict
from Marie.Util.location import  PY4J_JAR_PATH, CHEMSPOT_JAR_PATH, JAVA_PATH,\
    DICT_ZIP_PATH, IDS_ZIP_PATH, MULTICLASS_BIN_PATH, PUBCHEM500_JSONL_PATH

import fuzzyset

# launch py4j gateway to lib-jar and expose it as a service





class Annotator():
    def __init__(self):
        gateway = JavaGateway.launch_gateway(jarpath=PY4J_JAR_PATH, classpath=CHEMSPOT_JAR_PATH,
                                            javaopts=['-Xmx16G'], java_path= JAVA_PATH,
                                            die_on_exit=True, use_shell=False)
    # create instance of the class of interest within the jar

        ChemSpotFactory = gateway.jvm.de.berlin.hu.chemspot.ChemSpotFactory()
        self.tagger = ChemSpotFactory.createChemSpot(DICT_ZIP_PATH, IDS_ZIP_PATH, MULTICLASS_BIN_PATH)

    def tag(self, text):
        raw = self.tagger.tag(text)
        if len(raw) > 0:
            maxlength = 0
            for mention in raw:
                newlength = mention.getEnd()-mention.getStart()
                if newlength > maxlength:
                    maxlength = newlength
                    truemention = (mention.getStart(), mention.getEnd(), mention.getText())
            return truemention
        # Currently assume one entity only
        else:
            return self.fallback_search(text)

    def fallback_search(self, text):
        entitynames = load_entity_dict(PUBCHEM500_JSONL_PATH, True)
        entityset = fuzzyset.FuzzySet()
        for e in entitynames:
            entityset.add(e.lower())
        tokens = text.lower().strip().split()
        bestscore = 0
        midx = None
        # First scan for single token
        for idx, token in enumerate(tokens):
            score, entry = entityset.get(token)[0]
            if score > bestscore:
                bestscore, midx = score, idx
        # second scan for multiple token
        mstart = midx
        mend = midx + 1
        tokens_len = len(tokens)
        possible_groups = [(midx, midx + 2), (midx, midx + 3), (midx - 1, midx + 1), (midx - 2, midx + 1),
                           (midx - 1, midx + 2)]
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


if __name__ == '__main__':
    # read a test file
    rawdata = "ner failed for what are all the functional groups in phenylisocyanate"
    #tagged = fallback_search(rawdata)
    #print(tagged)
    # model.infer(testdata)
