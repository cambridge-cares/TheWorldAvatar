import os
import difflib
import re

class DataExtensionTransformer:
    def __init__(self, file_path1, file_path2):
        self.file_path1 = file_path1
        self.file_path2 = file_path2
        self.validate_paths()

    def validate_paths(self):
        if not os.path.isfile(self.file_path1):
            raise FileNotFoundError(f"The file does not exist: {self.file_path1}")
        if not os.path.isfile(self.file_path2):
            raise FileNotFoundError(f"The file does not exist: {self.file_path2}")

    def read_file(self, file_path):
        with open(file_path, 'r', encoding='utf-8') as file:
            return file.readlines()

    def print_differences(self):
        differences = self.compare_files()
        if differences:
            print("Differences between the files:")
            for line in differences:
                print(line, end='')
        else:
            print("The files are identical.")
    def preprocess_text(text):
        """
        Preprocess the input text by:
        1. Converting to lowercase
        2. Removing special characters
        3. Removing extra spaces
        4. Stripping leading and trailing whitespace
        """
        text = text.lower()
        text = re.sub(r'[^a-z0-9\s]', '', text)
        text = re.sub(r'\s+', ' ', text).strip()
        return text

    def compare_strings(original, generated):
        """
        Compare two strings after preprocessing them.
        """
        original_preprocessed = preprocess_text(original)
        generated_preprocessed = preprocess_text(generated)
        
        return original_preprocessed == generated_preprocessed


# Example usage:
if __name__ == "__main__":
    file_path1 = os.getcwd() + "/Data_prompts/ChatgptOut.txt"
    file_path2 = os.getcwd() + "/Data_prompts/PaperSynthesis.txt"
    print(file_path1)
    comparator = TextFileComparator(file_path1, file_path2)
    comparator.print_differences()



