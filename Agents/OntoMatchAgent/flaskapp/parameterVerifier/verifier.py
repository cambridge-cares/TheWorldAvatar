import os
from  ontomatch.utils.blackboard import LOCAL_BLACKBOARD_DIR
def verifyRelativePathExists(path, root = None):
    if root is None:#Do this for handles
        root = os.getcwd()
        onelevelUp = os.path.split(root)
        print(onelevelUp)
        ap = os.path.normpath(os.path.join(onelevelUp[0], path))
    else:
        ap = os.path.normpath(os.path.join(root, path))
    print(ap)
    #if not os.path.exists(ap):
    #    raise Exception("invalid parameter: "+path)
    return True


def verifyChoice(param, list):
    if param in list:
        return True
    raise Exception("invalid parameter: " + param)
