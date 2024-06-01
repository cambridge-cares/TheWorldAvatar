def deep_update(source: dict, destination: dict):
    for key, value in source.items():
        if isinstance(value, dict):
            # get node or create one
            node = destination.setdefault(key, {})
            deep_update(value, node)
        else:
            destination[key] = value
