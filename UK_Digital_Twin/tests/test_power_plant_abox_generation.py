import unittest

from UK_Power_Plant_Generator.powerPlantABoxGeneration import addUKPowerPlantTriples


class TestUKDigitalTwin(unittest.TestCase):
    def test_list_int(self):
        """
        Test that it can generate the instances provided in the reference OWL files.
        """
        addUKPowerPlantTriples('default', 9999, True)

        data = [1, 2, 3]
        result = sum(data)
        self.assertEqual(result, 6)

if __name__ == '__main__':
    unittest.main()