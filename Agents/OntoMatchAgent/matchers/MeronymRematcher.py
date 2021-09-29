#todo: multi:edit,jaro,qgram
from nltk.metrics.distance import jaro_similarity,edit_distance,jaro_winkler_similarity
import math
from itertools import chain, combinations
from alignment import Alignment




#TODO: you should combine S and T together!
class MeronymRematcher():
    #Find meronyms by rules
    #rule: group {same name, same hasAddress}, add union to capacity
    def __init__(self, es, alignmentList):
        self.S, self.T = es
        self.a = alignmentList

    def rematch(self):
        #newAlignList, same format
        #TODO: add relationship type to alignment.render
        newAs = set()
        sgroups, tgroups = self.groupingByMatch()
        print("finished grouping")
        for idx, (tId,slist) in enumerate(sgroups.items()):
            newAList = self.findMeronymIngroup(slist, [(tId,)])
            for multiA in newAList:#translate multiMatch to group of single matches
                sList, tList = multiA
                for idxS in sList:
                    newAs.add((idxS, tId, 1))#TODO: what to do with rematching value score
        for idx, (sId, tlist) in enumerate(tgroups.items()):
            newAList = self.findMeronymIngroup(tlist, [(sId,)])
            for multiA in newAList:#translate multiMatch to group of single matches
                sList, tList = multiA
                for idxT in tList:
                        newAs.add((sId, idxT, 1))#TODO: what to do with rematching value score
        newAlignList = Alignment(list(newAs))
        return newAlignList



    #TODO: alternatively
    def groupingByMatch(self):
        allIdS = {idS for idS, idT, v in self.a.map}
        id2GroupS = {}
        id2GroupT = {}
        #find all T matched to this S
        for idS, idT, v in self.a.map:
            if idS not in id2GroupT:  #
                id2GroupT[idS] = list()
                id2GroupT[idS].append(idT)
            else:
                id2GroupT[idS].append(idT)
            if idT not in id2GroupS:  #
                id2GroupS[idT] = list()
                id2GroupS[idT].append(idS)
            else:
                id2GroupS[idT].append(idS)

        id2GroupS = {mId:ml for (mId,ml) in id2GroupS.items() if len(ml)> 1}
        id2GroupT = {mId:ml for (mId,ml) in id2GroupT.items() if len(ml)> 1}

        return id2GroupS, id2GroupT




    def groupingByName(self):
        #find groups of objects by name
        #filter out distinct idS, and, idT
        allIdS = {idS for idS, idT, v in self.a.map}
        allIdT = {idT for idS, idT, v in self.a.map}
        SGroups = self.findStringGroups(list(allIdS), self.S)
        TGroups = self.findStringGroups(list(allIdT), self.T)
        return SGroups, TGroups


        #discard set smaller than one

    #find list of meronym alignment in a single group
    def findMeronymIngroup(self, sIds, tIds):
        #find all alignments to each entity
        CAPA_THRE = 0.01
        if len(sIds)>1:
            sCandidates = self.powerset(sIds)
        else:
            sCandidates = sIds
        if len(tIds)>1:
            tCandidates= self.powerset(tIds)
        else:
            tCandidates = tIds
        MeronnymAs = []

        for sCandidate in list(sCandidates):
            for tCandidate in list(tCandidates):
                sSum = sum([self.getDesignedCapacity(sid, self.S) for sid in sCandidate])
                tSum = sum([self.getDesignedCapacity(tid, self.T) for tid in tCandidate])
                if abs(sSum - tSum) <= CAPA_THRE:
                    MeronnymAs.append([sCandidate, tCandidate])
        return MeronnymAs


    def getDesignedCapacity(self, id, onto):
        # property should be design capacity
        #return raw value
        for propertyObj, valueObj in onto.valueMap.map[id]:
            if "designCapacity" in propertyObj:
                return valueObj.value

    def powerset(self,iterable):
        "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
        s = list(iterable)
        return chain.from_iterable(combinations(s, r) for r in range(2, len(s) + 1))


    #for a list of id, an ontology, group them by similar names
    def findStringGroups(self, idList, onto):
        THRESHOLD = 0.9
        Item2GroupId= {}
        groupIndex = 0
        groups = []
        for item1Id in idList:
            item1Name = onto.individualNames[item1Id]
            for item2Id in idList:
                if item2Id > item1Id:
                    item2Name = onto.individualNames[item2Id]
                    if jaro_winkler_similarity(item1Name , item2Name)>THRESHOLD:
                        if item1Id not in Item2GroupId:#new group
                            Item2GroupId[item1Id] = groupIndex
                            groupIndex = groupIndex + 1
                        if item2Id not in Item2GroupId:
                            Item2GroupId[item2Id] = Item2GroupId[item1Id]


        for gId in range(0, groupIndex+1):
            thisGroup = [itemId for itemId, groupid in Item2GroupId.items() if groupid == gId]
            groups.append(thisGroup)
        return groups


