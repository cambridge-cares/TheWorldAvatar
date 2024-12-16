"""
The PubchemReader will read the pubchem data in csv form and convert it to a form that
is compatible with the KG embedding library (tsv), where each line represents a triple
"""
import csv
import os
import random

from KGToolbox.Tools import MakeIndex
from Marie.Util.location import DATA_DIR


class PubchemReader:

    def __init__(self):
        self.output_path = os.path.join(DATA_DIR, r'CrossGraph/pubchem/pubchem-train.txt')
        self.pubchem_dir = os.path.join(DATA_DIR, r'CrossGraph/pubchem/pubchem.csv')
        self.line_template = '%s\t%s\t%s\n'

        with open(self.output_path, 'w') as f:
            f.write('')
            f.close()
        with open(self.output_path.replace('-train', '-test'), 'w') as f:
            f.write('')
            f.close()
        with open(self.output_path.replace('-train', '-valid'), 'w') as f:
            f.write('')
            f.close()


    def convert_to_triples(self, csv_reader):
        """
        From the csv_reader, convert the data into triples
        :param csv_reader:
        :return:
        """

        all_values = {}
        all_lines = []
        relation_stoplist = ['fingerprint', 'compound_id']
        fields = csv_reader.fieldnames
        super_counter = 0
        row_counter = 0
        for row in csv_reader:
            row_counter = row_counter + 1
            if row_counter % 1000 == 0:
                print('Row number ', row_counter)
            head_entity = row['compound_id']
            for field_name in fields:
                relation = field_name
                tail_entity = head_entity + '_' + relation
                if relation not in relation_stoplist:
                    all_lines.append(self.line_template % (head_entity, relation, tail_entity))  # the fact triple
                    super_counter = super_counter + 3
                    if super_counter % 10000 == 0:
                        print(super_counter)

        twenty_percent_length = round(len(all_lines) * 0.2)
        train_list = all_lines
        test_list = random.sample(list(all_lines), twenty_percent_length)
        valid_list = random.sample(list(all_lines), twenty_percent_length)
        print(len(train_list), len(valid_list), len(test_list))

        with open(self.output_path, 'a') as f:
            for train_l in train_list:
                f.write(train_l)
            f.close()
        with open(self.output_path.replace('-train', '-test'), 'a') as f:
            for test_l in test_list:
                f.write(test_l)
            f.close()
        with open(self.output_path.replace('-train', '-valid'), 'a') as f:
            for v_l in valid_list:
                f.write(v_l)
            f.close()

    def read_csv_file(self):
        """Read the csv file and get the headers
        Keyword arguments:
        """

        csv_lines = open(self.pubchem_dir).readlines()
        print('Total rows', len(csv_lines))
        file = csv_lines [0:50000]
        csv_reader = csv.DictReader(file)
        return csv_reader

    def main(self):
        self.convert_to_triples(self.read_csv_file())
        MakeIndex.create_indexing(data_dir="CrossGraph/pubchem", dataset_name="pubchem")


if __name__ == '__main__':
    my_pubchem_reader = PubchemReader()
    my_pubchem_reader.main()
