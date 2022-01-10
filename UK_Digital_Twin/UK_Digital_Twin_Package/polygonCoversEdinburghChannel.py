###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 10 Jan 2022                   #
###################################################

from shapely.geometry import shape
from shapely.geometry.polygon import Polygon

"""The polygon which can cover the sea channel around Edinburgh"""
EdinburghChannelPolygonGeoJson : dict = {
        "type": "Polygon",
        "coordinates": [
          [
            [
              -2.5927734375,
              56.27843607650187
            ],
            [
              -2.96630859375,
              56.2235043883283
            ],
            [
              -3.7161254882812496,
              56.07050243614236
            ],
            [
              -3.7380981445312496,
              55.975335084865094
            ],
            [
              -3.00750732421875,
              55.926124639200474
            ],
            [
              -2.537841796875,
              55.990700524726485
            ],
            [
              -2.5927734375,
              56.27843607650187
            ]
          ]
        ]
      }
  
EdinburghChannelPolygonShapely : Polygon = shape(EdinburghChannelPolygonGeoJson)


"""The polygon which can cover the north coast of Edinburgh channel"""
EdinburghChannelNorthGeoJson : dict =  {
        "type": "Polygon",
        "coordinates": [
          [
            [
              -1.7413330078125,
              57.468589192089354
            ],
            [
              -2.1478271484375,
              57.436080586253084
            ],
            [
              -3.5650634765625,
              56.45338475677308
            ],
            [
              -3.3453369140625,
              56.01987804345656
            ],
            [
              -2.63671875,
              56.18225387824831
            ],
            [
              -1.7413330078125,
              57.468589192089354
            ]
          ]
        ]
      }
EdinburghChannelNorthShapely : Polygon = shape(EdinburghChannelNorthGeoJson)

"""The polygon which can cover the south coast of Edinburgh channel"""
EdinburghChannelSouthGeoJson : dict =  {
        "type": "Polygon",
        "coordinates": [
          [
            [
              -3.372802734375,
              56.01066647040695
            ],
            [
              -3.3837890625,
              55.88147363004733
            ],
            [
              -1.9830322265624998,
              55.866064809810105
            ],
            [
              -2.4609375,
              56.0965557505683
            ],
            [
              -2.7850341796875,
              56.08736247495096
            ],
            [
              -3.372802734375,
              56.01066647040695
            ]
          ]
        ]
      }
EdinburghChannelSouthShapely : Polygon = shape(EdinburghChannelSouthGeoJson)

if __name__ == '__main__':
    print(type(EdinburghChannelNorthShapely), EdinburghChannelNorthShapely)
    print(type(EdinburghChannelSouthShapely), EdinburghChannelSouthShapely)