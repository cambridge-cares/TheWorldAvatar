import json
import matplotlib.pyplot as plt

class LoadKGIndex:
    def __init__(self):
        self.filecount=0
        self.triplecount=0
        self.inverted_index = {}
        self.class_names=[]
        self.class_files=[]
    
    def load_inverted_index(self, file_path):
        with open(file_path, 'r') as f:
            self.inverted_index = json.load(f)
        return self.inverted_index
    
    def get_inverted_index(self):
        return self.inverted_index
    
    def search_concept(self, class_name):
        if class_name in self.inverted_index:
            return self.inverted_index[class_name]
        else:
            return None

    def analyse(self):
        for class_name, files in self.inverted_index.items():
            self.class_names.append(self.get_substring_from_last(class_name))
            self.class_files.append(len(files))
            
    def get_substring_from_last(self,string):
        return string.rsplit('/', 1)[-1].rsplit('#', 1)[-1]
        
    def bar_plt(self):
        # Create bar chart
        plt.figure(figsize=(8, 6))
        plt.bar(self.class_names, self.class_files, color='skyblue')

        # Add title and labels
        plt.title('Class file distribution')
        plt.xlabel('Class')
        plt.ylabel('Frequency')

        # Show plot
        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.show()

# usage
if __name__ == "__main__":
    #index_file = "C:/Users/printer_admin/Downloads/KGs/ontokin/inverted_index_170.json"
    index_file = "C:/Users/printer_admin/Downloads/KGs/inverted_index.json"
    kgs = LoadKGIndex()
  
    index=kgs.load_inverted_index(index_file)
    # files=kgs.search_concept("http://theworldavatar.com/kb/ontokin/ontokin.owl#Element")
    kgs.analyse()
    kgs.bar_plt()

    class_count = 0
    file_count = 0
    avg_files_per_class = 0.0
    
    for class_name, files in index.items():
        # Access each object's properties
        file_count += len(files)
        class_count += 1
    
    avg_files_per_class = file_count/class_count
    # Print the files where the concept is there
    print(f"Total class:{class_count}; Average files per class: {avg_files_per_class}")
