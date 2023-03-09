import math
import itertools
class Polygon:

    def __init__(self, points, hminmax):
        self.points = points
        #print(self.points)
        self.signedArea = self.__calArea()
        self.area = math.fabs(self.signedArea)
        self.perimeter = self.__calPerimeter()
        self.perimax = self.__calPeriMax()
        #print (self.perimax[0])
        self.centroid = self.__calCentroid(self.signedArea)
        self.type = self.__circularTest(self.area,self.perimeter)
        self.angle = self.__calAngle(self.perimax) if self.type is 0 else 0
        self.length = self.perimax[0] if self.type is 0 else 1
        self.width = self.area/self.perimax[0] if self.type is 0 else 0
        self.diameter =self.__calDiameter(self.area) if self.type is 1 else 0
        self.hmin, self.hmax = hminmax
        self.height= self.hmax - self.hmin


    

    def __calArea(self):
        result = 0
        for i in range(len(self.points) - 1):
            x0, y0 = self.points[i]
            x1 , y1 = self.points[i+1]
            result+= x0*y1 - x1*y0
        return 0.5 * result

    def __calPerimeter(self):
        result = 0
        for i in range(len(self.points) - 1):
            x0, y0 = self.points[i]
            x1 , y1 = self.points[i+1]
            dx, dy = x0 - x1, y0 - y1
            result += math.sqrt(dx*dx + dy * dy)

        return result

    def __calPeriMax(self):
        pmax ,pointsmax = 0,None
        for i in range(len(self.points) - 1):
            x0, y0 = self.points[i]
            x1 , y1 = self.points[i+1]
            dx, dy = x0 - x1, y0 - y1
            e = math.sqrt(dx*dx + dy * dy)
            if e>pmax:
                pmax = e
                pointsmax = (x0, y0, x1, y1)
        return(pmax,)+pointsmax

    def __calCentroid(self, area):
        cx, cy = 0, 0
        for i in range(len(self.points) - 1):
            x0, y0 = self.points[i]
            x1 , y1 = self.points[i+1]
            cx = cx + (x0+x1)*(x0*y1-x1*y0)
            cy = cy + (y0+y1)*(x0*y1-x1*y0)
        return (cx/6/area, cy/6/area)  



    def __circularTest(self, area, perimeter):
        T = math.fabs(4*22/7*area/perimeter**2)
        if T<=1 and T >=0.9:
            return 1
        else:
            return 0


    def __calAngle(self, perimax):
        pmax, x0, y0, x1, y1 = perimax
        xCnt, yCnt, x2, y2 = (x1, y1, x0, y0) if x0>x1 else (x0, y0, x1, y1)

        ymod = yCnt + pmax
        xmod = xCnt
        return (math.atan2(ymod - yCnt, xmod - xCnt) - math.atan2(y2 - yCnt, x2 - xCnt))/math.pi * 180

    def __calDiameter(self, area):
        return math.sqrt(area*4*7/22)

    @staticmethod
    def combineBaseMulti(otherfuckers):
        '''
        return a new polygon based on height 
        '''
        minAll = min(tuple(part.hmin for part in otherfuckers))
        allBase = tuple(part for part in otherfuckers if part.hmin is minAll)
        if len(allBase) is 1:
            return allBase[0] #this part is base

        else:
            allPGrps = tuple(part.points for parts in a)
            maxAll = max(tuple(part.hmin for part in otherfuckers))
            #construct a new combined polygon
            return Polygon(itertools.chain(*allPGrps), (minAll, maxAll))





    
