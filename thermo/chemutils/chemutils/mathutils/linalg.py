import numpy as np
import math

def getXYZPointsDistance(p1, p2):
    return np.sqrt(np.sum((p1-p2)**2, axis=0))

def findClosestXYZPoints(p1List, p2List):
    numPoints = len(p1List)
    closestPoints = [None]*numPoints

    for i, p1 in enumerate(p1List):
        minDist = 1e9
        for j, p2 in enumerate(p2List):
            dist = getXYZPointsDistance(p1,p2)
            if dist < minDist:
                minDist = dist
                closestPoints[i] = j
    return closestPoints

def getPlaneAngle(a1,b1,c1):
    a = np.array(a1)
    b = np.array(b1)
    c = np.array(c1)

    ba = a - b
    bc = c - b

    cosine_angle = np.dot(ba, bc) / (np.linalg.norm(ba) * np.linalg.norm(bc))
    angle = np.arccos(cosine_angle)
    angle = np.degrees(angle)
    return angle

def getDihedralAngle(p0,p1,p2,p3):
    """Praxeolitic formula
    1 sqrt, 1 cross product"""
    p0 = np.array(p0)
    p1 = np.array(p1)
    p2 = np.array(p2)
    p3 = np.array(p3)


    b0 = -1.0*(p1 - p0)
    b1 = p2 - p1
    b2 = p3 - p2

    # normalize b1 so that it does not influence magnitude of vector
    # rejections that come next
    b1 /= np.linalg.norm(b1)

    # vector rejections
    # v = projection of b0 onto plane perpendicular to b1
    #   = b0 minus component that aligns with b1
    # w = projection of b2 onto plane perpendicular to b1
    #   = b2 minus component that aligns with b1
    v = b0 - np.dot(b0, b1)*b1
    w = b2 - np.dot(b2, b1)*b1

    # angle between v and w in a plane is the torsion angle
    # v and w may not be normalized but that's fine since tan is y/x
    x = np.dot(v, w)
    y = np.dot(np.cross(b1, v), w)
    return np.degrees(np.arctan2(y, x))

def findDuplicatesInList(inputList, relTol=1e-05, absTol=1e-08):
    dupl = []
    for i, value1 in enumerate(inputList):
        for j, value2 in enumerate(inputList[i+1:]):
            ans = math.isclose(value1, value2, rel_tol=relTol, abs_tol=absTol)
            if ans: dupl.append((i,i+j+1))
    return dupl