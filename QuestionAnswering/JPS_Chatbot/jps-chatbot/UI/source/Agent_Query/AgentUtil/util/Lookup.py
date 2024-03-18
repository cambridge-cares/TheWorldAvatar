from rapidfuzz import process, fuzz


def find_nearest_match(term, keys):
    if term is None:
        return None, None
    term = term.strip()
    rst = process.extractOne(term, keys, scorer=fuzz.ratio)
    key = rst[0]
    score = rst[1]
    return key, score


