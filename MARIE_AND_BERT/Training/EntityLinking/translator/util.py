import json
def loadjsonl(filepaths):
    items = []
    if type(filepaths) is not list:
        filepaths = [filepaths]
    for filepath in filepaths:
        with open(filepath, 'r') as f:
            for line in f.readlines():
                items.append(json.loads(line))

    ids = list(range(len(items)))
    texts = [item["text"] for item in items]
    if "label" in items[0]:
        labels = [item["label"] for item in items]
    else:
        labels = None
    return ids, texts, labels
