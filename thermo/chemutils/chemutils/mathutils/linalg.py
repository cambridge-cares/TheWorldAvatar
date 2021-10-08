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

def findDuplicatesInList(inputList, relTol=1e-05, absTol=1e-08):
    dupl = []
    for i, value1 in enumerate(inputList):
        for j, value2 in enumerate(inputList[i+1:]):
            ans = math.isclose(value1, value2, rel_tol=relTol, abs_tol=absTol)
            if ans: dupl.append((i,i+j+1))
    return dupl