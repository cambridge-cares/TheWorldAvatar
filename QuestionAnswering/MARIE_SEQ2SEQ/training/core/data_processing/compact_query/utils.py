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
