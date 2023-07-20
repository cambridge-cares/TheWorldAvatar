import unittest
import sys
import yaml
import os

sys.path.append("..")
from Marie.Util.location import ROOT_DIR, DATA_DIR


def load_config_file():
    with open(os.path.join(ROOT_DIR, "config.yaml")) as f:
        data = yaml.load(f, Loader=yaml.FullLoader)
        return data


class MyTestCase(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super(MyTestCase, self).__init__(*args, **kwargs)
        self.config = load_config_file()
        self.domains = set(self.config["domains"])
        self.domain_root_path = os.path.join(DATA_DIR, self.config["paths"]["domain_path"])
        self.essential_files = set(self.config["essential_files"])
        self.domain_specific_files = set(self.config["domain_specific_files"])

    def test_domains(self):
        """
        Check whether the domain root folder contains all the domains listed in the config file
        :return:
        """
        domain_folders = os.listdir(self.domain_root_path)
        have_all_domains = set(self.domains) <= set(domain_folders)
        self.assertEqual(have_all_domains, True)  # add assertion here

    def test_domain_files(self):
        """
        Check whether each domain folder contains all the essential files for running
        :return:
        """
        for domain in self.domains:
            domain_file_path = os.path.join(self.domain_root_path, domain)
            domain_files = os.listdir(domain_file_path)
            have_all_files = self.essential_files <= set(domain_files)
            self.assertEqual(have_all_files, True)

            domain_specific_files = set([f % domain for f in self.domain_specific_files])
            have_all_files = domain_specific_files <= set(domain_files)
            missing_files = [f for f in domain_specific_files if f not in domain_files]
            print("missing files are: ", missing_files)

            self.assertEqual(have_all_files, True)

if __name__ == '__main__':
    unittest.main()
