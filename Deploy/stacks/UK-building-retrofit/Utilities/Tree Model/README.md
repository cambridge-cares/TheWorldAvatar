# Household Retrofit Decision Making

The Tree Model Utility applies a machine learning-based approach to predict energy efficiency improvements using a decision tree model (XGBoost). This utility helps in evaluating optimal retrofit strategies by analyzing building characteristics across various local authorities. It is designed to support data-driven decision-making for building energy upgrades.

## 1. Prerequisites

Ensure you have Python installed (version 3.8+ recommended). This project requires API access for external data retrieval and a virtual environment setup. To set up the virtual environment, use the following commands:

```bash
python -m venv venv
source venv/bin/activate  # On Windows use `venv\Scripts\activate`
pip install -r tree_model_requirements.txt
```

Additionally, you need to register for an API token at [EPC Open Data Communities](https://epc.opendatacommunities.org/domestic/search). This token must be included in the configuration file (`xgboost.json`) to access building energy data.

## 2. Configuration

The utility relies on a configuration file (`xgboost.json`), which specifies paths for input data, model storage, and API credentials. Users are expected to provide two CSV files: one for training the model and another for analysis. Both files should contain local authority codes (LA codes) and associated building features.

An example configuration file is shown below:

```json
{
  "TRAINING_LA_CODES_PATH": "path/to/training_la_codes.csv",
  "ANALYSIS_LA_CODES_PATH": "path/to/analysis_la_codes.csv",
  "XGBOOST_MODEL_PATH": "trained_xgboost_model.pkl",
  "SCALER_PATH": "scaler.pkl",
  "API_TOKEN": "your_api_token_here"
}
```

The `TRAINING_LA_CODES_PATH` points to a CSV file containing local authority codes and features used for model training. The `ANALYSIS_LA_CODES_PATH` specifies a CSV file with local authority codes for analysis. The `XGBOOST_MODEL_PATH` and `SCALER_PATH` define where the trained model and scaler object are saved, respectively. The `API_TOKEN` is required for accessing additional building data from external sources.

The training process uses the XGBoost Regressor, a gradient boosting model known for its efficiency with structured data. Key techniques include early stopping, which monitors the validation performance and halts training if no improvement is detected, preventing overfitting. The input features are normalized using `StandardScaler` to ensure consistent performance. Additionally, hyperparameter tuning is performed using `GridSearchCV`, which systematically explores different parameter combinations to identify the optimal model configuration.

## 3. Running the Analysis

The analysis can be performed by executing the `retrofit_decision_making.py` script with the specified configuration file. To train the model, use the following command:

```bash
python retrofit_decision_making.py --mode train --config xgboost.json
```

This command initiates the training process, applying feature scaling and using early stopping for efficient training. The trained model is saved to the specified path.

To analyze retrofit options using the trained model, execute the following command:

```bash
python retrofit_decision_making.py --mode analyze --config xgboost.json
```

This command uses the trained model to generate retrofit recommendations based on the input features provided in the analysis CSV file.

For hyperparameter tuning, use the following command:

```bash
python retrofit_decision_making.py --mode tune --config xgboost.json
```

This command performs hyperparameter optimization using `GridSearchCV`, testing various configurations to find the best performing parameters for the model.

## 4. Output

Upon completion of the analysis, the utility generates several output files. The `trained_xgboost_model.pkl` file contains the serialized model, which can be reused for future predictions. The `scaler.pkl` file stores the scaler object used for feature normalization, ensuring consistent preprocessing of input data. The `evaluation_report.csv` file includes metrics such as the R² score and feature importance, providing insights into the model’s performance and the impact of different features. Additionally, the utility saves visualizations in the `visualizations/` directory, including plots for feature importance and regression performance, such as a predicted vs actual values plot.

The feature importance plot highlights the key predictors identified by the model, helping users understand which building features contribute most significantly to the predicted energy efficiency improvements.

## 5. Notes

Ensure that the API token provided in `xgboost.json` is valid and has the necessary permissions for data access. The input tables for local authority codes must be formatted correctly and match the expected schema for successful execution. The utility uses `XGBRegressor`, a robust tree-based ensemble model that is well-suited for tabular data and handles both regression and classification tasks effectively. If any issues arise during execution, check the logs for detailed error messages. For additional debugging information, consider running the script with the `--verbose` flag as shown below:

```bash
python retrofit_decision_making.py --mode train --config xgboost.json --verbose
```
