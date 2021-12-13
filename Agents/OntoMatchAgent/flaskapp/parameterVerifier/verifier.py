import os
def verifyRelativePathExists(path, root = None):
    if root is None:
        ap = os.path.abspath(path)
    else:
        ap = os.path.join(root, path)
    print(ap)
    if not os.path.exists(ap):
        raise Exception("invalid parameter: "+path)
    return True


def verifyChoice(param, list):
    if param in list:
        return True
    raise Exception("invalid parameter: " + param)
