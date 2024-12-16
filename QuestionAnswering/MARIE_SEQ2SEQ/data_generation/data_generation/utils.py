def add_space_and_lower(string: str):
    chars = []
    for i, char in enumerate(string):
        if char.isupper():
            if i > 0:
                chars.extend([" ", char.lower()])
            else:
                chars.append(char.lower())
        else:
            chars.append(char)
    return "".join(chars)
