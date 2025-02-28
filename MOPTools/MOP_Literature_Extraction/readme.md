# MOP_Literature_Extraction

## Overview
MOP_Literature_Extraction is a Python-based pipeline designed to process and extract key information from scientific literature related to materials and synthesis procedures. The project leverages machine learning models and knowledge graph techniques to extract and structure data from scientific papers.

## Features
- Extracts synthesis procedures from scientific literature
- Utilizes OpenAI models for text processing
- Integrates with knowledge graphs for structured data storage
- Supports PDF and text-based inputs

## Installation

### Prerequisites
Ensure you have the following installed:
- Python 3.8+
- `pip` package manager

### Setup
Clone the repository and install dependencies:
```bash
$ git clone https://github.com/your-repo/MOP_Literature_Extraction.git
$ cd MOP_Literature_Extraction
$ pip install -r requirements.txt
```

## Usage
Run the main pipeline to process literature:
```bash
$ python main_pipeline.py
```
This will execute the full pipeline including:
1. Converting PDFs to text
2. Extracting synthesis details
3. Uploading data to a knowledge graph

## Module Descriptions

### `utils.py`
Contains utility functions for file handling, API interactions, and text processing.

### `upload.py`
Handles interactions with the knowledge graph, including data upload and query operations.

### `upload_utils.py`
Provides additional helper functions for uploading and structuring extracted data.

### `setup_structure.py`
Manages the directory and file structure needed for processing scientific literature.

### `process_directory.py`
Contains methods for processing input directories, handling file formats, and preparing data for extraction.

### `llm_extraction.py`
Interfaces with OpenAI's language models for text analysis and synthesis extraction.

### `llm_prompts.py`
Defines structured prompts used for querying LLMs.

### `main_pipeline.py`
Executes the entire data processing pipeline, from file conversion to data extraction and uploading.

### `kg_queries.py`
Contains SPARQL queries for interacting with the knowledge graph.

### `json_schemas.py`
Defines JSON schemas for structuring extracted data.

## Configuration
Modify `parameters.py` to customize paths and settings.

## Contributing
Contributions are welcome! Feel free to open an issue or submit a pull request.

## License
This project is licensed under the MIT License. See `LICENSE` for details.

## Contact
For inquiries, contact [your-email@example.com](mailto:your-email@example.com).

