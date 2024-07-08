import fitz  # PyMuPDF

def pdf_to_text(pdf_path, txt_path):
    try:
        # Open the PDF file
        pdf_document = fitz.open(pdf_path)
    except Exception as e:
        print(f"Error opening PDF file: {e}")
        return
    
    text = ""
    
    try:
        # Iterate over each page
        for page_num in range(len(pdf_document)):
            page = pdf_document.load_page(page_num)
            text += page.get_text()
    except Exception as e:
        print(f"Error reading PDF file: {e}")
        return
    
    try:
        # Write the text to a .txt file
        with open(txt_path, 'w', encoding='utf-8') as txt_file:
            txt_file.write(text)
    except Exception as e:
        print(f"Error writing to text file: {e}")
        return
    
    print(f"Text extracted from {pdf_path} and saved to {txt_path}")

# Example usage
pdf_path = 'Data_PDF/li-et-al-2010-ligand-bridging-angle-driven-assembly-of-molecular-architectures-based-on-quadruply-bonded-mo-mo-dimers.pdf'
txt_path = 'Data_PDF/li-et-al-2010-ligand-bridging-angle-driven-assembly-of-molecular-architectures-based-on-quadruply-bonded-mo-mo-dimers.txt'
pdf_to_text(pdf_path, txt_path)