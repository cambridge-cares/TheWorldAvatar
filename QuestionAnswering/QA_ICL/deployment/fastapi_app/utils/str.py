def camel_case_split(text: str):
    start_idx = [i for i, e in enumerate(text) if e.isupper()] + [len(text)]
    start_idx = [0] + start_idx
    return [text[x:y] for x, y in zip(start_idx, start_idx[1:])]
