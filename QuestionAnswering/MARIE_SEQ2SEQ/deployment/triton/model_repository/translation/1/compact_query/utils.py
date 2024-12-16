def advance_ptr_to_kw(text: str, kw: str, ptr: int=0):
    while ptr < len(text) and not text.startswith(kw, ptr):
        ptr += 1
    return ptr


def advance_ptr_thru_space(text: str, ptr: int=0):
    while ptr < len(text) and text[ptr].isspace():
        ptr += 1
    return ptr


def advance_ptr_to_space(text: str, ptr: int=0):
    while ptr < len(text) and not text[ptr].isspace():
        ptr += 1
    return ptr


def remove_terminal_chars(text: str):
    i = len(text)
    while i > 0:
        if text[i - 1] in ".,;?!-":
            i -= 1
        elif text[i - 2: min(len(text), i)] == "'s":
            i -= 2
        elif text[i - 3: min(len(text), i)] == "'ll":
            i -= 3
        else:
            break
    return text[:i]
