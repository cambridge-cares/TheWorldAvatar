import json
import matplotlib.pyplot as plt

class LoadKGIndex:
    def __init__(self):
        self.inverted_index = {}
        self.key_names=[]
        self.key_endpoints=[]
    
    def load_inverted_index(self, file_path):
        with open(file_path, 'r') as f:
            self.inverted_index = json.load(f)
        return self.inverted_index
    
    def get_inverted_index(self):
        return self.inverted_index
    
    def search_concept(self, key_name):
        if key_name in self.inverted_index:
            return self.inverted_index[key_name]
        else:
            return None

    def analyse(self):
        for key_name, endpoints in self.inverted_index.items():
            self.key_names.append(self.get_substring_from_last(key_name))
            self.key_endpoints.append(len(endpoints))
            
    def get_substring_from_last(self,string):
        return string.rsplit('/', 1)[-1].rsplit('#', 1)[-1]
    
    def print_key_value_stats(self):
        key_count = 0
        endpoint_count = 0
        avg_endpoints_per_key = 0.0
        
        print(f"Total number of keys: {len(self.key_names)}")  
        for i in range(len(self.key_names)):
            endpoint_count += self.key_endpoints[i]
            key_count += 1
            print(f"{self.key_names[i]} : {self.key_endpoints[i]}")          
        
        avg_endpoints_per_key = endpoint_count/key_count
        print(f"Total keys:{key_count}; Average endpoints per key: {avg_endpoints_per_key}")
        
    def bar_plt(self):
        # Create bar chart
        plt.figure(figsize=(8, 6))
        plt.bar(self.key_names, self.key_endpoints, color='skyblue')

        # Add title and labels
        plt.title('Key-to-endpoints distribution')
        plt.xlabel('Key')
        plt.ylabel('Key Frequency')

        # Show plot
        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.show()

# usage
if __name__ == "__main__":
    #index_file = "C:/Users/printer_admin/Downloads/KGs/ontokin/inverted_index_170.json"
    index_file = "C:/Users/printer_admin/Downloads/KGs/p2e_invindex.json"
    kgs = LoadKGIndex()
  
    index=kgs.load_inverted_index(index_file)
    # files=kgs.search_concept("http://theworldavatar.com/kb/ontokin/ontokin.owl#Element")
    kgs.analyse()
    kgs.print_key_value_stats()
    kgs.bar_plt()
