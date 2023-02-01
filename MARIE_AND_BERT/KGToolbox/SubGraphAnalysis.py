import sys
import csv
sys.path.append("")
from Marie.Util.location import DATA_DIR

import os

class SubGraphAnalysis:
    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.subgraph_dir = os.path.join(self.full_dataset_dir, "ontokin_reactions_subgraph_10000_cluster_0")
        self.subgraph_triples_file = os.path.join(self.subgraph_dir, "ontokin_reactions-train.txt")
        if os.path.exists(self.subgraph_triples_file):
            with open(self.subgraph_triples_file) as file:
                triples_string = file.read()
                
            triple_lines = triples_string.strip().split("\n")
            self.triples = []
            for line in triple_lines:
                subject, predicate, obj = line.strip().split("\t") # or line.strip().split(",")
                self.triples.append((subject, predicate, obj))
            # print(self.triples)
        self.unique_reactions = set()
        for _,_,obj in self.triples:
            self.unique_reactions.add(obj)

    def PFI_Calculation(self):
        PFI={}
        for reaction in self.unique_reactions:
            reaction_triples = [(subject, predicate, obj) for subject, predicate, obj in self.triples if obj == reaction]
            cnt=0
            species_count=0
            for sub,_,_ in reaction_triples:
                count=0
                for s, p, o in self.triples:
                    if s==sub:
                        count+=1
                if(count>1):
                    species_count+=(count-1)
                else:
                    species_count+=0
                cnt+=1
            PFI[reaction] = species_count/cnt
            # print(PFI)

        # with open('PFI.csv', 'w', newline='') as file:
        #     writer = csv.writer(file)
        #     writer.writerow(['Key', 'Value'])
        #     for key, value in PFI.items():
        #         writer.writerow([key, value])

if __name__ == "__main__":
    subgraph = SubGraphAnalysis()
    subgraph.PFI_Calculation()
    

       
