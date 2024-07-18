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


config = load_config_file()
domains = config["domains"]
print(domains)
paths = config["paths"]
domain_root_path = os.path.join(DATA_DIR, config["paths"]["domain_path"])
domain_folders = os.listdir(domain_root_path)
contained = set(domains) <= set(domain_folders)
print(contained)

essential_files = config["essential_files"]
print(essential_files)

for domain in domains:
    domain_path = os.path.join(domain_root_path, domain)
    print(domain_path)
    domain_files = os.listdir(domain_path)
    domain_contains_files = set(essential_files) <= set(domain_files)
    if not domain_contains_files:
        print([f for f in set(essential_files) if f not in set(domain_files)])



