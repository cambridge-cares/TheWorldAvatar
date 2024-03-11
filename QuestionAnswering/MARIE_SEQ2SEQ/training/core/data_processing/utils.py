def replace_multi(text: str, mapper: dict):
    for k, v in mapper.items():
        text = text.replace(k, v)
    return text

def replace_list_element(lst: list, old, new):
    found = False
    for i, val in enumerate(lst):
        if val == old:
            lst[i] = new
            found = True
        if found:
            break