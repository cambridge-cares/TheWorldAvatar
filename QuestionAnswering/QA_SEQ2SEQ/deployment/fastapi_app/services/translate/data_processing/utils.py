def replace_multi(text: str, mapper: dict):
    for k, v in mapper.items():
        text = text.replace(k, v)
    return text