import PyPDF2
import re
import pandas as pd

# Function to extract text from the PDF
def extract_text_from_pdf(pdf_path):
    with open(pdf_path, 'rb') as file:
        pdf_reader = PyPDF2.PdfReader(file)
        text = ""
        for page_num in range(len(pdf_reader.pages)):
            page = pdf_reader.pages[page_num]
            page_text = page.extract_text()
            # Remove common header and footer patterns (customize these patterns as needed)
            page_text = remove_headers_footers(page_text)
            text += page_text
    return text

# Function to remove headers and footers
def remove_headers_footers(page_text):
    # Example patterns to remove (customise as needed)
    header_footer_patterns = [
        r'POINTS OF INTEREST –  CLASSIFICATION SCHEME',  # header
        r'December 2022',  # header
        r'OFFICIAL', # footer
        r'© Or dnance Survey Ltd 2022', # footer
        r'Page \d+ of \d+',  # page numbers
    ]
    
    for pattern in header_footer_patterns:
        page_text = re.sub(pattern, '', page_text, flags=re.IGNORECASE)
    
    # Optionally remove lines that are too short (likely to be headers/footers)
    cleaned_lines = page_text.split('\n')
    return '\n'.join(cleaned_lines)

# Improved function to extract categories and classes
def extract_categories_classes(text):
    # Split text into lines
    lines = text.split('\n')

    category_dict = {}
    current_category = None
    current_class = None

    for line in lines:
        # Check if the line matches a category pattern
        if re.match(r'^\d{2} ', line):
            current_category = line.strip()
            category_dict[current_category] = []
            current_class = None
        # Check if the line matches a class pattern
        elif re.match(r'^\d{4} ', line):
            current_class = line.strip()
            if current_category:
                current_class = re.sub(r'\s+\d{2}\s+[A-Z].*$', '', current_class)
                parts = re.split(r'(?<!^)(?=\d{4} )', current_class)
                for part in parts:
                    current_class = part.strip()
                    category_dict[current_category].append(current_class)
        # Handle continuation lines (for split categories or classes)
        else:
            if current_class:
                parts = re.split(r'(?=\d{4} )', line.strip())
                count = 0
                part_size = len(parts)
                for part in parts:
                    count += 1
                    current_class += ' ' + part.strip()
                    if count <= 1:
                        # Update the last class in the list
                        category_dict[current_category][-1] = current_class
                    else:
                        category_dict[current_category].append(current_class)
                    if count < part_size:
                        current_class = ''
            elif current_category:
                current_category += ' ' + line.strip()
                # Update the category in the dict
                category_dict[current_category] = category_dict.pop(list(category_dict.keys())[-1])

    return category_dict

# Extract text from the provided PDF
pdf_path = '../Data/input/points-of-interest-classification-schemes-v3.4.pdf'
pdf_text = extract_text_from_pdf(pdf_path)

# Extract categories and classes
category_dict = extract_categories_classes(pdf_text)

# Convert the dictionary to a DataFrame for better visualization
data = []
for category, classes in category_dict.items():
    for cls in classes:
        data.append([category, cls])

df = pd.DataFrame(data, columns=["Category", "Class"])

# Display the DataFrame
print(df)

# Optionally, save the DataFrame to a CSV file
df.to_csv("categories_and_classes.csv", index=False)
