def advance_idx_to_kw(text: str, kw: str, idx: int=0):
    while idx < len(text) and not text.startswith(kw, idx):
        idx += 1
    return idx


def advance_idx_thru_space(text: str, idx: int=0):
    while idx < len(text) and text[idx].isspace():
        idx += 1
    return idx


def advance_idx_to_space(text: str, idx: int=0):
    while idx < len(text) and not text[idx].isspace():
        idx += 1
    return idx
