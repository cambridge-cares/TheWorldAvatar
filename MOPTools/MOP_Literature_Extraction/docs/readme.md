## Module Descriptions

### `utils.py`
A collection of utility functions and classes for handling knowledge graph updates, text processing, and PDF conversions.

#### Knowledge Graph Utilities

- **`AboxUpdateConfig`**  
  Defines configuration settings for SPARQL-based knowledge graph updates, including query/update endpoints and authentication credentials.

- **`config_a_box_updates(env_file: str = None) -> AboxUpdateConfig`**  
  Loads and returns the configuration settings for ABox updates from an environment file or system variables.

- **`get_client(name: str) -> PySparqlClient`**  
  Initializes and returns a `PySparqlClient` instance for querying and updating a knowledge graph.

- **`generate_backup(filename: str) -> None`**  
  Executes a SPARQL `CONSTRUCT` query and saves the knowledge graph backup in Turtle format.

#### Text Processing Utilities

- **`read_json_file(file_path: str) -> dict`**  
  Reads a JSON file and returns its contents as a dictionary.

- **`doi_from_path(input_path: str) -> str`**  
  Extracts and formats the DOI (Digital Object Identifier) from a given file path.

- **`extract_bracket_substrings(input_string: str) -> tuple`**  
  Extracts substrings enclosed in square brackets from a given input string.

- **`append_si_to_paper(directory: str) -> None`**  
  Merges supplementary information (`_si.txt`) files into their corresponding main text files.

#### PDF Processing Utilities

- **`PdfConverter`**  
  A class to manage PDF-to-text conversion, including validation and text extraction.

  - **`convert_pdf_to_text() -> str`**  
    Converts a given PDF file to a text file.

  - **`extract_text_from_pdf() -> str`**  
    Extracts text from a PDF file using PyMuPDF (`fitz`).

  - **`save_text_to_file(text: str, output_path: str) -> None`**  
    Saves extracted text to a file.

  - **`read_text_file(file_path: str) -> str`**  
    Reads a text file and returns its contents.

### `upload.py`
The `upload.py` module provides a collection of functions for processing and uploading chemical synthesis data, characterization results, and related metadata to the knowledge graph.

#### **Predefined Ontology Uploads**
These functions ensure that standard concepts related to chemistry, materials, vessels, separation methods, and measurement units are consistently represented in the knowledge graph.

- **`upload_predefined()`**  
  Uploads predefined ontology instances, including chemical elements, vessel types, measurement units, and separation methods, to standardize data representation.

#### **Chemical Input and Output Uploads**
These functions process and upload chemical input data, chemical synthesis steps, and chemical output entities.

- **`upload_inputChem(chemicals, synthesis_client, species_client)`**  
  Processes a list of chemical species and their concentrations, linking them to the knowledge graph.

- **`instantiate_input(chemical_formula, species_name, client_species, client_synthesis)`**  
  Retrieves or instantiates chemical species, checking both the OntoSpecies and OntoSynthesis knowledge graphs.

- **`instantiate_output(ccdc_number, chemical_formula, mop_names, yield_str, client_mop, client_synthesis)`**  
  Retrieves or creates Metal-Organic Polyhedron (MOP) instances and their associated yield values in the knowledge graph.

- **`chemicals_upload(input_path, output_path)`**  
  Reads synthesis procedures from a JSON file and uploads chemical input and output data to the knowledge graph.

#### **Synthesis Step Processing**
These functions structure and upload synthesis steps, linking them to vessels, chemical inputs, and synthesis transformations.

- **`standard_step_upload(standard_input, vessel_list, chemicals_list, synthesis_client, species_client)`**  
  Processes different synthesis steps (e.g., Sonicate, Add, HeatChill, Filter, Stir) and uploads them to the knowledge graph.

- **`upload_steps(input_path, output_path)`**  
  Extracts synthesis steps from a JSON file, associates them with vessels and chemical inputs, and uploads them to the knowledge graph.

#### **Characterization Data Processing**
These functions process and upload analytical data, linking characterization results to synthesized compounds.

- **`elemental_analysis_upload(elemental_analysis, syn_client, chemical_output, molecular_formula, device)`**  
  Parses and uploads elemental analysis data, distinguishing between experimental and analytical measurements.

- **`characterisation_upload(input_path, output_path)`**  
  Extracts characterization data such as elemental analysis, NMR spectra, and IR spectroscopy from a JSON file and links them to chemical transformations in the knowledge graph.

#### **Chemical Building Units and Linkage**
These functions establish links between Chemical Building Units (CBUs) and Metal-Organic Polyhedra (MOPs).

- **`instantiate_cbu(cbu_formula, syn_client, mop_client, species)`**  
  Retrieves or instantiates a Chemical Building Unit (CBU) and links it to a species.

- **`link_cbu(input_path, output_path)`**  
  Reads synthesis procedure data and links CBUs to their corresponding MOPs in the knowledge graph.

#### **Elemental Composition Parsing**
This function extracts elemental weight percentages from text and structures them for knowledge graph storage.

- **`parse_element_string(element_string, syn_client)`**  
  Parses an element composition string and converts it into structured mass fraction data.

#### **Main Function**
- **`main()`**  
  Calls `upload_predefined()` to initialize the knowledge graph with standard ontology instances.

This module ensures that synthesis data, characterization results, and related metadata are systematically structured and uploaded to the knowledge graph.

### `upload_utils.py`
The `upload_utils.py` module provides helper functions for matching and processing ontology entities, structuring extracted data, and preparing it for upload to the knowledge graph.

#### **Ontology Entity Matching**
These functions retrieve ontology instances for elements, separation techniques, atmospheres, vessels, and measurement units, ensuring consistency when uploading synthesis data.

- **`match_element(element_name, client)`**  
  Matches a given chemical element (e.g., Carbon, Oxygen) to its corresponding IRI in the knowledge graph.

- **`match_separation(separation_name, client)`**  
  Matches a separation technique (e.g., centrifuge, column chromatography) to its corresponding IRI.

- **`match_atmosphere(atmosphere, client)`**  
  Matches an atmosphere type (e.g., Nitrogen, Argon, Air) to its corresponding IRI in the knowledge graph.

- **`match_vessel(vessel_name, client)`**  
  Matches a vessel type (e.g., Teflon-lined stainless-steel vessel, round-bottom flask) to its corresponding IRI.

- **`get_unit(unit_name, client)`**  
  Retrieves a unit of measure (e.g., °C, mol, g, hour) and its IRI from the knowledge graph.

#### **Data Extraction and Transformation**
These functions extract and process numerical values, textual annotations, and metadata for proper structuring before upload.

- **`extract_numbers_and_units(text, pattern_type, multiplier_flag=2)`**  
  Extracts numerical values and their corresponding units from a given text string, handling both single values and ranges.

- **`extract_numbers_and_brackets(input_string)`**  
  Extracts numerical values along with any content inside parentheses for structured representation.

- **`remove_na(input_candidate)`**  
  Replaces "N/A" values with an empty string to standardize missing data handling.

- **`update_alt_label(species, species_name)`**  
  Adds alternative labels (names) to a species entity if they are not already included.

- **`replace_character(species_names)`**  
  Cleans up special characters (e.g., curly apostrophes, en dashes) in species names to ensure consistent text representation.

#### **Knowledge Graph Upload Utilities**
These functions facilitate structured data uploads, handling recursive depth and connection management.

- **`push_component_to_kg(instances, client, recursive_depth=-1)`**  
  Uploads a list of component instances to the knowledge graph, handling recursive depth settings.

- **`start_upload(input_path)`**  
  Initializes SPARQL clients for OntoSynthesis, OntoSpecies, and OntoMOP, extracting relevant file path details.

- **`steps_preupload(standard_step, synthesis_client, vessel_list)`**  
  Prepares and standardizes step-related data (e.g., duration, vessel, atmosphere) before uploading it to the knowledge graph.

This module ensures the proper structuring, transformation, and validation of extracted chemical synthesis data before integration into the knowledge graph.

### Setup Structure

The `setup_structure.py` script is responsible for creating a standardized directory structure for organizing data related to a batch process. It ensures that all necessary folders exist before execution.

#### Usage

To execute the script and create the required directory structure, run:

```bash
python setup_structure.py
```

This script depends on the `parameters.py` module, which should define the following variables:

- `FILE_DIR`: The base directory where the structure will be created.
- `BATCH_NAME`: The name of the batch, which will be used to generate specific subdirectories.

#### Directory Structure

When executed, the script will create the following folder structure inside the specified `FILE_DIR`:

```
Data/
│── <BATCH_NAME>_pdf/
│── <BATCH_NAME>_txt/
│── <BATCH_NAME>_cbu/
│── <BATCH_NAME>_preSteps/
│── <BATCH_NAME>_steps/
│── <BATCH_NAME>_extractedProcedure/
│── <BATCH_NAME>_chemicals1/
│── <BATCH_NAME>_characterisation/
```

Each directory serves the following purpose:

- `<BATCH_NAME>_pdf/` - Stores PDF input files.
- `<BATCH_NAME>_txt/` - Contains extracted text from PDFs.
- `<BATCH_NAME>_cbu/` - Holds CBU-related data.
- `<BATCH_NAME>_preSteps/` - Saves pre-processing step data.
- `<BATCH_NAME>_steps/` - Contains main processing step data.
- `<BATCH_NAME>_extractedProcedure/` - Stores extracted procedures.
- `<BATCH_NAME>_chemicals1/` - Holds chemical-related information.
- `<BATCH_NAME>_characterisation/` - Contains characterization data.

#### Dependencies

Ensure that `parameters.py` is properly configured before running the script. It should define:

```python
FILE_DIR = "/path/to/base/directory"
BATCH_NAME = "example_batch"
```

The script utilizes `os.makedirs()` with `exist_ok=True`, meaning it will not raise an error if the directories already exist.

#### Output

On successful execution, the script prints confirmation messages for each created directory:

```
Directory '/path/to/base/directory/Data/example_batch_pdf' created successfully.
Directory '/path/to/base/directory/Data/example_batch_txt' created successfully.
...
```


### `process_directory.py`
The `process_directory.py` module provides functions for processing various types of files in a directory, including text extraction, formatting corrections, and token counting.

#### **Directory Processing**
- **`process_files_in_directory(func, input_dir, output_dir)`**  
  Iterates over files in the `input_dir`, applies the specified function (`func`) to each file, and saves the results in `output_dir`.

#### **File Format Transformations**
- **`transform_xyz_string(file_path: str, output_dir: str) -> None`**  
  Converts a misformatted `.xyz` file where atomic data is in a single line into a properly structured multi-line format.

- **`prepend_line_count(file_path: str, output_dir: str) -> None`**  
  Adds a line count at the beginning of an `.xyz` file to correct formatting issues.

#### **PDF Processing**
- **`extract_text_from_pdf(file_path: str, output_dir: str) -> None`**  
  Extracts text from a PDF file while excluding headers and footers, then saves the extracted text to a new file.

- **`transform_txt_folder(file_dir: str) -> None`**  
  Converts all PDFs in a given directory into plain text format.

#### **Tokenization and Cost Estimation**
- **`count_tokens_and_calculate_cost(file_path: str, output_dir: str) -> dict`**  
  Counts the number of tokens in a text file and estimates processing costs based on a predefined token price.

#### **Synthesis Data Extraction**
- **`extract_synthesis(extraction: str) -> None`**  
  Extracts different aspects of synthesis data from text files, including:
  - **"chemicals"** – Extracts chemical information.
  - **"procedure"** – Extracts full synthesis procedures.
  - **"preSteps"** – Extracts preprocessing steps.
  - **"cbu"** – Extracts chemical building units.
  - **"steps"** – Extracts synthesis steps.
  - **"characterisation"** – Extracts characterization data.

#### **Uploading Extracted Data**
- **`upload_chemcials()`**  
  Uploads extracted chemical data to a knowledge graph.

- **`upload_steps_dir()`**  
  Uploads extracted synthesis step data.

- **`upload_characterisation_dir()`**  
  Uploads extracted characterization data.

- **`link_cbu_dir()`**  
  Links extracted Chemical Building Unit (CBU) data.

This module plays a crucial role in processing input directories, transforming raw data into structured formats, and preparing extracted information for integration into a knowledge graph.

### `llm_extraction.py`
The `llm_extraction.py` module is responsible for extracting structured data from scientific literature using OpenAI's language models. It utilizes predefined prompts and schemas to extract chemical information, synthesis steps, characterization data, and other relevant details from text-based sources.

#### **ChatGPT API Wrapper**
- **`ChatGPTAPI`**  
  A class that provides an interface for interacting with OpenAI's API for both structured and unstructured text generation.
  - **`make_llm_call(message)`**  
    Calls the OpenAI model with a given message and returns a free-form response.
  - **`make_llm_call_structured(prompt, schema)`**  
    Calls the OpenAI model using a structured prompt and predefined schema to generate JSON-formatted responses.

#### **Extraction Functions**
These functions extract specific synthesis-related data from scientific texts and store the results in structured JSON or text files.

- **`extract_chemicals(file_path, output_dir)`**  
  Extracts chemical input and output details, including names, formulas, supplier information, and purities, using a structured JSON schema.

- **`extract_steps(file_path, output_dir)`**  
  Identifies synthesis step types and extracts detailed step-by-step procedures, including chemical additions, heating, filtering, and crystallization steps. Uses a two-step process:  
  1. Identifies relevant step types dynamically.  
  2. Extracts and structures steps using a JSON schema.

- **`extract_pre_steps(file_path, output_dir)`**  
  Extracts preliminary synthesis steps before the main reaction occurs. Outputs structured text-based results.

- **`extract_procedure(file_path, output_dir)`**  
  Extracts complete synthesis procedures in a verbatim format, ensuring that all details are preserved.

- **`extract_cbu(file_path, output_dir)`**  
  Extracts Chemical Building Unit (CBU) information by matching CBUs with equivalent laboratory species. Uses a structured schema to format the extracted data.

- **`extract_characterization(file_path, output_dir)`**  
  Extracts characterization data from synthesis procedures, including NMR, IR, and elemental analysis, and stores it in a structured JSON format.

Each function follows a standard workflow:
1. Reads the synthesis text from a file.
2. Generates an appropriate prompt based on the context.
3. Calls OpenAI’s API using either structured or unstructured methods.
4. Stores the extracted data in the specified output directory.

This module ensures systematic and reliable data extraction from scientific literature, facilitating integration into a structured knowledge graph.


### `llm_prompts.py`
The `llm_prompts.py` module defines structured prompts for querying large language models (LLMs) to extract and categorize synthesis-related information. These prompts help standardize data extraction, ensuring consistency in the knowledge graph integration.

#### **Step Type Identification**
- **`step_types_prompt()`**  
  Generates a structured prompt instructing the LLM to categorize synthesis steps into predefined types. The output is a JSON file indicating which step types are present in a given synthesis procedure.

#### **Synthesis Step Extraction**
- **`step_prompt(doi, dynamic_prompt)`**  
  Constructs a detailed prompt for extracting synthesis steps, categorizing them based on predefined types such as Add, HeatChill, Filter, etc. It incorporates knowledge graph data, ensuring structured output that adheres to specific formatting and nomenclature.

- **`pre_steps_prompt()`**  
  Guides the LLM in extracting and categorizing preliminary synthesis steps. Ensures that steps are broken down into distinct actions, each assigned to a vessel and properly formatted for structured storage.

#### **Chemical and Procedure Summarization**
- **`chemical_prompt(doi)`**  
  Generates a prompt for summarizing synthesis procedures and structuring them into a JSON format. Includes chemical input/output details, supplier names, purities, and synthesis conditions.

- **`procedure_prompt(doi)`**  
  Extracts and formats synthesis procedures verbatim, ensuring that all relevant details are preserved. Assigns Metal-Organic Polyhedron (MOP) formulas and Cambridge Crystallographic Data Centre (CCDC) numbers when possible.

#### **Chemical Building Unit Mapping**
- **`cbu_prompt(doi)`**  
  Matches Chemical Building Units (CBUs) with corresponding laboratory species based on synthesis data. Uses knowledge graph lookups to provide structured assignments.

#### **Characterization Data Extraction**
- **`characterisation_prompt(doi)`**  
  Extracts characterization data, including Nuclear Magnetic Resonance (NMR), Infrared Spectroscopy (IR), and Elemental Analysis. The extracted data is structured into JSON format, ensuring completeness.

#### **Synthesis Text Reformatting**
- **`synthesis_text_prompt(txt)`**  
  Reformats synthesis procedures into a structured format suitable for scientific publications. Groups content into Materials, Procedure, and Characterization sections, ensuring clarity and standardization.


### `main_pipeline.py`
Executes the entire data processing pipeline, from file conversion to data extraction and uploading.

### `kg_queries.py`
The `kg_queries.py` module defines functions for querying knowledge graphs to retrieve chemical synthesis, characterization, and literature data. It interacts with the `OntoSynthesis` and `OntoMOP` knowledge graphs using SPARQL queries.

#### **Characterization Data Retrieval**
- **`query_characterisation(doi)`**  
  Queries the knowledge graph to retrieve characterization data related to a given DOI. The extracted data includes:
  - Yield values and units
  - Fourier Transform Infrared Spectroscopy (FTIR) data
  - Nuclear Magnetic Resonance (NMR) data
  - Elemental analysis data
  - Molecular formula information

#### **Literature and Chemical Information Retrieval**
- **`get_literature(doi)`**  
  Retrieves literature information associated with a given DOI, including MOP formulas and CCDC numbers.

- **`get_input_species(doi)`**  
  Queries and retrieves the chemical species used in a synthesis process linked to a given DOI.

- **`input_for_cbu(doi)`**  
  Extracts Chemical Building Unit (CBU) information, MOP formulas, and species data associated with a given DOI.

- **`query_mop_names(doi)`**  
  Retrieves Metal-Organic Polyhedra (MOP) names (alternative labels) associated with a given DOI.

#### **Synthesis Data Extraction**
- **`query_synthesis_full(doi)`**  
  Queries the knowledge graph to retrieve a complete synthesis record associated with a given DOI. The extracted data includes:
  - Provenance (DOI reference)
  - Chemical outputs and MOP labels
  - Synthesis steps, vessels, and environmental conditions
  - Duration, temperature, concentration, and pressure details
  - Specific synthesis actions (e.g., adding chemicals, filtering, heating)
  - Characterization data such as NMR, FTIR, and elemental analysis

#### **Ontology Matching and Data Querying**
These functions facilitate retrieval and matching of ontology instances in the knowledge graph.

- **`species_querying(client, species_label)`**  
  Retrieves species entities from the `OntoSpecies` knowledge graph based on provided labels.

- **`species_querying_ontosyn(client, species_label)`**  
  Queries the `OntoSynthesis` knowledge graph for species instances using alternative labels.

- **`mop_querying(client, CCDC_number, mop_formula, mop_name)`**  
  Searches for a Metal-Organic Polyhedron (MOP) in the `OntoMOP` knowledge graph using various identifiers (CCDC number, formula, alternative labels).

- **`CBU_querying(client, cbu_formula)`**  
  Queries the knowledge graph for Chemical Building Units (CBUs) based on their molecular formula.

- **`chemicalOutput_querying(client, CCDC_number, mop_formula, mop_name)`**  
  Retrieves chemical output instances linked to a given MOP from the `OntoSynthesis` knowledge graph.

- **`doi_querying(client, doi)`**  
  Queries the knowledge graph to retrieve documents associated with a specific DOI.

- **`transformation_querying(client, mop_name)`**  
  Queries chemical transformations related to a given Metal-Organic Polyhedron (MOP) name.

This module provides a structured approach to extracting and linking knowledge from synthesis literature, facilitating integration into a structured ontology.

### `json_schemas.py`
The `json_schemas.py` module defines structured JSON schemas to ensure consistency in the representation of chemical synthesis procedures, characterizations, and chemical building units (CBUs). These schemas facilitate structured data extraction and validation when working with chemical synthesis data.

#### **Synthesis Step Schema**
- **`step_schema(dynamic_prompt)`**  
  Generates a JSON schema for chemical synthesis procedures based on dynamically identified steps. This ensures the extracted synthesis steps conform to a standardized structure.  
  - Supports various synthesis steps such as:
    - Addition (`Add`)
    - Heating/Cooling (`HeatChill`)
    - Drying (`Dry`)
    - Filtering (`Filter`)
    - Sonication (`Sonicate`)
    - Stirring (`Stir`)
    - Crystallization (`Crystallization`)
    - Dissolving (`Dissolve`)
    - Separation (`Separate`)
    - Evaporation (`Evaporate`)
    - Transferring (`Transfer`)
  - Each step contains specific properties such as vessel type, atmosphere conditions, temperature, duration, and added chemicals.

#### **Adaptive Schema**
- **`adaptive_schema()`**  
  Defines a JSON schema for a chemical synthesis process that lists all possible synthesis step types. This schema ensures only specific boolean properties related to synthesis steps are allowed and required.  
  - Ensures strict adherence to a structured synthesis process.
  - Used to determine which steps are present in a given synthesis.

#### **Chemical Schema**
- **`chemical_schema()`**  
  Defines the structure of chemical synthesis procedures, including input chemicals, output chemicals, and their properties.  
  - Captures details such as:
    - Chemical names and formulas
    - Amounts and suppliers of chemicals
    - Reaction yield and CCDC number

#### **Chemical Building Unit (CBU) Schema**
- **`cbu_schema()`**  
  Defines a structured format for chemical building units used in synthesis procedures.  
  - Stores:
    - CCDC numbers of metal-organic polyhedra (MOPs)
    - Two associated CBUs, including formulas and species names

#### **Characterization Schema**
- **`characterisation_schema()`**  
  Structures characterization data for synthesized products, including:
  - **Nuclear Magnetic Resonance (NMR)**
    - Chemical shifts
    - Solvent used
    - Measurement temperature
  - **Elemental Analysis**
    - Experimental vs. calculated weight percentage
    - Measurement device
    - Chemical formula
  - **Infrared Spectroscopy (IR)**
    - Measured bands
    - Material analyzed

These schemas ensure the extracted and processed synthesis data maintain a consistent and well-defined format for further analysis and integration into structured chemical databases.




