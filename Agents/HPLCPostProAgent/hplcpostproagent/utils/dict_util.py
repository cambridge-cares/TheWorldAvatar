from collections.abc import Mapping
# see https://stackoverflow.com/a/63543967
def deep_update(d1, d2):
    if all((isinstance(d, Mapping) for d in (d1, d2))):
        for k, v in d2.items():
            d1[k] = deep_update(d1.get(k), v)
        return d1
    return d2
