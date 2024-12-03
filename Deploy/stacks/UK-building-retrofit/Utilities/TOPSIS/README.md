
# TOPSIS Analysis

This project performs a TOPSIS analysis using data from a SPARQL endpoint. The setup includes package installation, configuration for criteria weighting, and selection of the data source URL.

## 1. Prerequisites

Ensure you have Python installed. This project requires a virtual environment.

### Virtual Environment Setup

```bash
# Create and activate a virtual environment
python -m venv venv
source venv/bin/activate  # On Windows use `venv\Scripts\activate`

# Install required packages
pip install -r TOPSIS_requirements.txt
```

## 2. Configuration

The analysis relies on two configuration files:

1. **Criteria Weight Configuration** (`criteria_weight.json`):
   - Defines the method for determining criteria weights.
   - Example:
     ```json
     {
       "weighting_method": "equal_weight" 
     }
     ```
 This file acts as the model selection JSON, with the default setting using "equal_weight" for evenly distributed weights. If you prefer an [entropy-based approach], simply replace "equal_weight" with "entropy_weight_method" to apply the entropy weighting method. 

2. **SPARQL Endpoint Configuration** (`stack_topsis.json`):
   - Specifies the URL of the SPARQL endpoint and the query file.
   - Example:
     ```json
     {
       "sparql_endpoint": "http://174.138.27.240:3838/ontop/sparql",
       "sparql_query_file": "ontop_topsis.sparql"
     }
     ```

## 3. Running the Analysis

Execute the `topsis.py` script to perform the TOPSIS calculation:

```bash
python topsis.py
```

The script will:
- Query the SPARQL endpoint for data.
- Normalize the data and apply the selected weighting method.
- Calculate distances to ideal and negative ideal solutions.
- Output the TOPSIS results to `result/TOPSIS_result.csv`.

## 4. Output

The results are saved as a CSV file with relative closeness scores:

```
result/TOPSIS_result.csv
```

## 5. Notes

- Ensure the SPARQL endpoint and query file are correctly defined in `stack_topsis.json`.
- The default weighting method is `"equal_weight"`, but it can be changed to `"entropy_weight_method"` in `criteria_weight.json`.

<!-- Links -->
[entropy-based approach]: https://www.sciencedirect.com/topics/engineering/entropy-method#:~:text=The%20entropy%20method%20is%20an,indicators%20through%20the%20information%20entropy.