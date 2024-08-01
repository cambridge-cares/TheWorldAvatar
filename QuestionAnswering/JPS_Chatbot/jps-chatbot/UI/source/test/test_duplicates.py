def remove_duplicated(uris):
    temp = []
    result = []
    for uri in uris:
        if uri not in temp:
            result.append(uri)
        temp.append(uri)
    return result

uris = ['1234', '2222', '2222', '1234', '323232', '1234', '1234', 'dfddf', 'dfddf', '2222', '2323']
uris = remove_duplicated(uris)
print(uris)
