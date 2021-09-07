from .ontoagent_generator import OntoAgentGenerator

agent = {
    "question_templates":
        ['[%s](attribute) of (species)'],
    "http_url": "http://somewhereincmcl.com/thermal",
    "outputs": [
        {
            "data_name": "HeatCapacityAtConstPressure",
            "data_type": "http://fake_concept_for_heat_capacity",
            "is_array": False,
            "ner_label": "attribute",
            "hasQualifier": ["temperature", "pressure"]
        },
        {
            "data_name": "InternalEnergy",
            "data_type": "http://fake_concept_for_heat_capacity",
            "is_array": False,
            "ner_label": "attribute",
            "hasQualifier": ["temperature", "pressure"]
        },
        {
            "data_name": "enthalpy",
            "data_type": "http://fake_concept_for_heat_capacity",
            "is_array": False,
            "ner_label": "attribute",
            "hasQualifier": ["temperature", "pressure"]
        },        {
            "data_name": "entropy",
            "data_type": "http://fake_concept_for_heat_capacity",
            "is_array": False,
            "ner_label": "attribute",
            "hasQualifier": ["temperature", "pressure"]
        },        {
            "data_name": "GibbsEnergy",
            "data_type": "http://fake_concept_for_heat_capacity",
            "is_array": False,
            "ner_label": "attribute",
            "hasQualifier": ["temperature", "pressure"]
        }

    ],
    "inputs": [
        {
            "data_name": "species",
            "data_type": "http://fake_concept_for_species",
            "is_array": False,
            "ner_label": "species"
        }
    ],

    "qualifiers": [
        {
            "data_name": "temperature",
            "data_type": "http://fake_concept_for_temperature",
            "is_array": False,
            "ner_label": "qualifier"
        },
        {
            "data_name": "pressure",
            "data_type": "http://fake_concept_for_pressure",
            "is_array": False,
            "ner_label": "qualifier"
        }
    ]
}

# we have a bunch of outputs

og = OntoAgentGenerator('Thermo_Agent')
og.create_instance(agent)
og.this_agent.save('Thermo_Agent.owl', format='rdfxml')


'''
{
  "result": {
    "RequestedTPPointData": {
      "RequestedTemperature": {
        "value": 298.15,
        "unit": "K"
      },
      "RequestedPressure": {
        "value": 101325,
        "unit": "Pa"
      },
      "Enthalpy": {
        "value": 40.12919928553181,
        "unit": "kJ/mol"
      },
      "InternalEnergy": {
        "value": 37.65024238805556,
        "unit": "kJ/mol"
      },
      "Entropy": {
        "value": 213.63981741937604,
        "unit": "J/mol/K"
      },
      "GibbsEnergy": {
        "value": -23.600142754295863,
        "unit": "kJ/mol"
      },
      "HeatCapacityAtConstPressure": {
        "value": 37.07228128068592,
        "unit": "J/mol/K"
      },
      "HeatCapacityAtConstVolume": {
        "value": 28.75781910568592,
        "unit": "J/mol/K"
      }
    },
    "RequestedTrangeData": {
      "RequestedTemperature": {
        "value": [
          298.15,
          300,
          400,
          500,
          600,
          700,
          800,
          900,
          1000,
          1200,
          1500,
          1700,
          2000,
          2500,
          3000,
          3500,
          4000,
          4500,
          5000
        ],
        "unit": "K"
      },
      "RequestedPressure": {
        "value": 101325,
        "unit": "Pa"
      },
      "Enthalpy": {
        "value": [
          40.12919928553181,
          40.19786011080941,
          44.122427745582854,
          48.40745396254705,
          52.98341513544712,
          57.799040782079246,
          62.81429008207218,
          67.99640193002644,
          73.31813390564186,
          84.29375431554108,
          101.35240203484366,
          112.99644772707006,
          130.73564669650025,
          160.773742500908,
          191.16940755451378,
          221.77926965357764,
          252.52683172725426,
          283.36787763540224,
          314.27517912104935
        ],
        "unit": "kJ/mol"
      },
      "InternalEnergy": {
        "value": [
          37.65024238805556,
          37.7035214583094,
          40.796642875582855,
          44.25022287504705,
          47.99473783044712,
          51.97891725957925,
          56.162720342072184,
          60.513385972526436,
          65.00367173064186,
          74.31639970554109,
          88.88070877234365,
          98.86186202957008,
          114.10672234650025,
          139.987587063408,
          166.22602102951376,
          192.67865204107764,
          219.26898302725425,
          245.95279784790227,
          272.7028682460494
        ],
        "unit": "kJ/mol"
      },
      "Entropy": {
        "value": [
          213.63981741937604,
          213.86939516666317,
          225.13189271316213,
          234.6802969375613,
          243.015979141759,
          250.43497836812128,
          257.12920315305456,
          263.23108993466576,
          268.8369140234005,
          278.8369220206171,
          291.51725416228476,
          298.8031374532295,
          308.4109575697515,
          321.81269998009543,
          332.89477766561293,
          342.33117344149866,
          350.54237118285533,
          357.8072955363442,
          364.32001005984375
        ],
        "unit": "J/mol/K"
      },
      "GibbsEnergy": {
        "value": [
          -23.600142754295863,
          -23.99579138526439,
          -45.974106601115125,
          -68.987416083025,
          -92.89183824175797,
          -117.58205428311364,
          -142.97662696323772,
          -169.01007784939728,
          -195.62822327134148,
          -250.4418838934988,
          -336.08764393895774,
          -395.15493930451083,
          -486.30515475016836,
          -644.0316153332876,
          -807.8432549030734,
          -976.7628884292077,
          -1150.0804256184983,
          -1327.2574464692693,
          -1507.8720869460835
        ],
        "unit": "kJ/mol"
      },
      "HeatCapacityAtConstPressure": {
        "value": [
          37.07228128068592,
          37.15558495422239,
          41.182858662008265,
          44.402875833328736,
          47.03217113233416,
          49.21421801246244,
          51.03642652076958,
          52.560548842148236,
          53.83645014386073,
          55.80630714377928,
          57.760155968975624,
          58.63680825619734,
          59.561608056772656,
          60.49852507601483,
          61.03885029226233,
          61.376124670444796,
          61.599849257848405,
          61.755487120662906,
          61.86795926580555
        ],
        "unit": "J/mol/K"
      },
      "HeatCapacityAtConstVolume": {
        "value": [
          28.75781910568592,
          28.84112277922239,
          32.86839648700827,
          36.08841365832873,
          38.71770895733417,
          40.899755837462436,
          42.72196434576958,
          44.24608666714824,
          45.521987968860735,
          47.491844968779276,
          49.44569379397563,
          50.32234608119734,
          51.24714588177265,
          52.18406290101483,
          52.724388117262336,
          53.06166249544479,
          53.28538708284841,
          53.4410249456629,
          53.55349709080556
        ],
        "unit": "J/mol/K"
      }
    },
    "NasaPolynomialsData": {
      "LowTemperature": {
        "value": 298.15,
        "unit": "K"
      },
      "MidTemperature": {
        "value": 1000,
        "unit": "K"
      },
      "HighTemperature": {
        "value": 5000,
        "unit": "K"
      },
      "RequestedPressure": {
        "value": 101325,
        "unit": "Pa"
      },
      "EnthalpyRef": {
        "value": null,
        "unit": "J/mol"
      },
      "EnthalpyRefTemp": {
        "value": 298.15,
        "unit": "K"
      },
      "LowTemperatureCoefficients": [
        2.1535464878389847,
        0.010609101572599724,
        -0.000011687904594577298,
        7.476871173615963e-9,
        -2.0773650358295484e-12,
        3802.2902438670867,
        10.719398716587678
      ],
      "HighTemperatureCoefficients": [
        4.52649655210396,
        0.0029576320993583796,
        -0.0000011966623139444194,
        2.1961882096729956e-10,
        -1.5070794241851975e-14,
        3161.6607104274804,
        -1.3610369862520857
      ],
      "NasaChemkinBlock": "CO2             STHD    C   1O   2          G    298.15   5000.00 1000.00      1\n 2.15354649e+00 1.06091016e-02-1.16879046e-05 7.47687117e-09-2.07736504e-12    2\n 3.80229024e+03 1.07193987e+01 4.52649655e+00 2.95763210e-03-1.19666231e-06    3\n 2.19618821e-10-1.50707942e-14 3.16166071e+03-1.36103699e+00                   4\n"
    }
  }
}
'''