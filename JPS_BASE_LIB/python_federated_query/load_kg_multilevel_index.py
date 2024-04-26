import json
import matplotlib.pyplot as plt

class LoadKGIndex:
    def __init__(self):
        self.filecount=0
        self.triplecount=0
        self.cp_index = {}
        self.cp_name=[]
        self.cp_files=[]
    
    def load_cp_index(self, file_path):
        with open(file_path, 'r') as f:
            self.cp_index = json.load(f)
        return self.cp_index
    
    def load_concept2property_invindex(self, file_path):
        try:
            # Open the file for reading
            with open(file_path, 'r') as file:
                # Load the JSON data from the file into the index variable
                self.cp_index = json.load(file)
            print("Class-Property multilevel inverted index loaded successfully.")
        except FileNotFoundError:
            print(f"File '{file_path}' not found.")
        except json.JSONDecodeError:
            print(f"Error decoding JSON data from '{file_path}'.")
            
    def get_cp_index(self):
        return self.cp_index
    
    def search_concept(self, class_name):
        if class_name in self.cp_index:
            return self.cp_index[class_name]
        else:
            return None

    # def analyse(self):
    #     for class_name, files in self.cp_index.items():
    #         self.class_names.append(self.get_substring_from_last(class_name))
    #         self.class_files.append(len(files))
    def analyse(self):
        for class_name, prop_dict in self.cp_index.items():
            for prop, files in prop_dict.items():
                class_name_short = self.get_substring_from_last(class_name)
                prop_name_short = self.get_substring_from_last(prop)
                key = f"{class_name_short}-{prop_name_short}"
                self.cp_name.append(key)
                self.cp_files.append(len(files))
    
    def print_key_value_stats(self):
        cp_count = 0
        file_count = 0
        avg_files_per_cp = 0.0
        total_class_prop = 0
        print(f"Total number of class-property key: {len(self.cp_name)}")  
        for i in range(len(self.cp_name)):
            file_count += self.cp_files[i]
            cp_count += 1
            print(f"{self.cp_name[i]} : {self.cp_files[i]}")          
        
        avg_files_per_cp = file_count/cp_count
        print(f"Total class-property:{cp_count}; Average files per class-property: {avg_files_per_cp}")
                    
    def get_substring_from_last(self,string):
        return string.rsplit('/', 1)[-1].rsplit('#', 1)[-1]
        
    def bar_plt(self):
        # Create bar chart
        plt.figure(figsize=(8, 6))
        plt.bar(self.cp_name, self.cp_files, color='skyblue')

        # Add title and labels
        plt.title('`Class-property` vs `number of file` distribution')
        plt.xlabel('Class-property')
        plt.ylabel('File Frequency')

        # Show plot
        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.show()

# usage
if __name__ == "__main__":
    #index_file = "C:/Users/printer_admin/Downloads/KGs/ontokin/cp_index_170.json"
    index_file = "C:/Users/printer_admin/Downloads/KGs/cp_invindex.json"
    kgs = LoadKGIndex()
  
    index=kgs.load_cp_index(index_file)
    # files=kgs.search_concept("http://theworldavatar.com/kb/ontokin/ontokin.owl#Element")
    kgs.analyse()
    kgs.print_key_value_stats()
    kgs.bar_plt()
    