###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 10 Jan 2022                   #
###################################################

from shapely.geometry import shape
from shapely.geometry.polygon import Polygon, LineString

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
              -2.076416015625,
              57.27310364731209
            ],
            [
              -2.7410888671875,
              56.77379848097407
            ],
            [
              -3.53759765625,
              56.42301656505151
            ],
            [
              -3.4332275390625,
              56.01066647040695
            ],
            [
              -2.5872802734374996,
              56.1883678647531
            ],
            [
              -2.0159912109375,
              57.043718234032625
            ],
            [
              -2.076416015625,
              57.27310364731209
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
              -3.40301513671875,
              56.00222059842409
            ],
            [
              -3.06243896484375,
              55.9238163647307
            ],
            [
              -2.0819091796875,
              55.89533634081856
            ],
            [
              -2.43896484375,
              56.09195938707178
            ],
            [
              -2.80426025390625,
              56.082765014169944
            ],
            [
              -3.40301513671875,
              56.00222059842409
            ]
          ]
        ]
      }
EdinburghChannelSouthShapely : Polygon = shape(EdinburghChannelSouthGeoJson)

"""The complementary border between England and Wales"""
complementaryBorderGeoJson = { 
    "type": "LineString",
    "coordinates": [
    [
      -2.669520244831496,
      51.74272427131478
    ],
    [
      -2.6597,
      51.6181
    ]
  ]
}
    
complementaryBorderShapely : LineString = shape(complementaryBorderGeoJson)
if __name__ == '__main__':
    # print(type(EdinburghChannelNorthShapely), EdinburghChannelNorthShapely)
    # print(type(EdinburghChannelSouthShapely), EdinburghChannelSouthShapely)    
    print(type(complementaryBorderShapely), complementaryBorderShapely)