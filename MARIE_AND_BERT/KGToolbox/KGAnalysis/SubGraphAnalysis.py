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
        
        self.filtered_triples=[]

        for reaction in self.unique_reactions:
            species = set()
            reaction_triples = [(subject, predicate, obj) for subject, predicate, obj in self.triples if obj == reaction]
            species = {subject for subject, _, _ in reaction_triples}
            if(len(species)==len(reaction_triples)):
                self.filtered_triples.extend(reaction_triples)
        print(len(self.filtered_triples))

    def PFI_Calculation(self):
        PFI={}
        unique_reactions = set()
        for _,_,obj in self.filtered_triples:
            unique_reactions.add(obj)

        for reaction in unique_reactions:
            reaction_triples = [(subject, predicate, obj) for subject, predicate, obj in self.filtered_triples if obj == reaction]
            
            cnt=0
            reaction_count=0
            
            species = set()
            for sub,_,_ in reaction_triples:
                species.add(sub)
            
            for sub in species:
                species_reaction_cnt=0
                
                for s, p, o in self.triples:
                    if s==sub:
                        species_reaction_cnt+=1

                reaction_count+=species_reaction_cnt

                if(cnt==0):
                    min=species_reaction_cnt
                    worst_species=sub

                if(species_reaction_cnt<min):
                    min=species_reaction_cnt
                    worst_species=sub

                cnt+=1

            PFI[reaction] = (reaction_count/cnt, worst_species, min)

        with open('PFI.csv', 'w', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(['Key', 'Value'])
            for key, value in PFI.items():
                writer.writerow([key, value])

if __name__ == "__main__":
    subgraph = SubGraphAnalysis()
    subgraph.PFI_Calculation()
    

       
