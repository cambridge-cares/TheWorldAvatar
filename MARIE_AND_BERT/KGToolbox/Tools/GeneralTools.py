def split_iri(iri):
    if "#" in iri:
        return iri.split("#")[-1]

    elif "/" in iri:
        return iri.split("/")[-1]
    else:
        return iri


def update_dict_with_list(key, value, dictionary):
    if key in dictionary:
        dictionary[key] += [value]
    else:
        dictionary[key] = [value]
    return dictionary
