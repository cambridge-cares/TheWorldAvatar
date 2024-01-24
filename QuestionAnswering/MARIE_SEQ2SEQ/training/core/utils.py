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
