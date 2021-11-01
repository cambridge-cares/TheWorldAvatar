def check1IfNew(radius1):
    for item in radius1:
        for mop_refPair in mopProvenance:
            if item in mop_refPair.keys():
                provenance = mopProvenance['MOPReference']
                print(item+"____________"+provenance)
            else:
                provenance = "Not in KG // New"
                print(item+"____________"+provenance)

                    results.append(result, mopProvenance)
