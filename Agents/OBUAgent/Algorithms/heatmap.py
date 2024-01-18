import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import csv
import itertools
from pathlib import Path
from typing import List, Tuple, Dict, Set
import os

output_dir = Path('data')

def remove_potential_duplicates(csv_file_path: Path, output_file_path: Path) -> None:
    try:
        df = pd.read_csv(csv_file_path)
        df_unique = df.drop_duplicates(subset=['BS', 'CORE'])
        df_unique.to_csv(output_file_path, index=False)
        print("Duplicates removed and data saved to", output_file_path)
    except Exception as e:
        print("An error occurred:", e)
        
def update_precursor_with_ocn(bs_smarts_file: Path, precursor_file: Path, output_file: Path) -> None:
    try:
        bs_smarts_df = pd.read_csv(bs_smarts_file)
        bs_to_ocn = dict(zip(bs_smarts_df['BS'], bs_smarts_df['OCN']))

        precursor_df = pd.read_csv(precursor_file)
        precursor_df['OCN'] = precursor_df['BS'].map(bs_to_ocn)
        precursor_df.to_csv(output_file, index=False)
        print("Precursor file updated and saved to", output_file)
    except Exception as e:
        print("An error occurred:", e)

def generate_ocn_combinations(input_file_path: Path, output_file_path: Path) -> None:
    try:
        bs_ocn, core_ocn = read_ocn_data(input_file_path)
        combinations = [
            (bs, core, bs_ocn[bs], core_ocn[core])
            for bs, core in itertools.product(bs_ocn.keys(), core_ocn.keys())
            if bs_ocn[bs] != core_ocn[core]
        ]
        write_combinations_to_csv(combinations, output_file_path)
    except Exception as e:
        print(f"Error in generate_ocn_combinations: {e}")

def read_ocn_data(file_path: Path) -> Tuple[Dict[str, str], Dict[str, str]]:
    bs_ocn, core_ocn = {}, {}
    with file_path.open(mode='r') as file:
        reader = csv.DictReader(file)
        for row in reader:
            bs, core, ocn = row['BS'], row['CORE'], row['OCN']
            bs_ocn[bs] = ocn
            core_ocn[core] = ocn
    return bs_ocn, core_ocn

def write_combinations_to_csv(combinations: List[Tuple[str, str, str, str]], output_file_path: Path) -> None:
    with output_file_path.open(mode='w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['BS', 'CORE', 'OCN_BS', 'OCN_CORE'])
        writer.writerows(combinations)

def process_data_and_save_sets(input_dir: Path, output_dir: Path) -> None:
    try:
        alg1_df = pd.read_csv(input_dir / 'Alg1_Output.csv')
        alg2_df = pd.read_csv(input_dir / 'Alg2_Output.csv')
        precursor_df = pd.read_csv(input_dir / 'Precursor_Cleaned.csv')

        set1_df = calculate_set_difference(alg1_df, alg2_df, precursor_df)
        set2_df = calculate_set_difference(alg2_df, alg1_df, precursor_df)
        set3_df = calculate_set_intersection(alg1_df, alg2_df, precursor_df)

        set1_df.to_csv(output_dir / "Set1.csv", index=False)
        set2_df.to_csv(output_dir / "Set2.csv", index=False)
        set3_df.to_csv(output_dir / "Set3.csv", index=False)
    except Exception as e:
        print(f"Error in process_data_and_save_sets: {e}")

def calculate_set_difference(df1: pd.DataFrame, df2: pd.DataFrame, precursor_df: pd.DataFrame) -> pd.DataFrame:
    diff_tuples = set(df1.itertuples(index=False, name=None)) - set(df2.itertuples(index=False, name=None))
    diff_tuples_filtered = diff_tuples - set(precursor_df.itertuples(index=False, name=None))
    return pd.DataFrame(list(diff_tuples_filtered), columns=df1.columns)

def calculate_set_intersection(df1: pd.DataFrame, df2: pd.DataFrame, precursor_df: pd.DataFrame) -> pd.DataFrame:
    intersect_df = pd.merge(df1, df2, how='inner')
    intersect_tuples = set(intersect_df.itertuples(index=False, name=None)) - set(precursor_df.itertuples(index=False, name=None))
    return pd.DataFrame(list(intersect_tuples), columns=intersect_df.columns)

def create_heatmap_data(data: pd.DataFrame, bs_labels: List[str], core_labels: List[str], value: int) -> np.ndarray:
    heatmap_data = pd.DataFrame(np.zeros((len(bs_labels), len(core_labels))), index=bs_labels, columns=core_labels)
    for _, row in data.iterrows():
        if row['BS'] in bs_labels and row['CORE'] in core_labels:
            heatmap_data.at[row['BS'], row['CORE']] = value
    return heatmap_data.to_numpy()

def layer_heatmaps(datasets: List[np.ndarray]) -> np.ndarray:
    return np.maximum.reduce(datasets)

def create_combined_heatmap(dataset_value_pairs: List[Tuple[pd.DataFrame, int]], bs_labels: List[str], core_labels: List[str]) -> np.ndarray:
    return layer_heatmaps([create_heatmap_data(data, bs_labels, core_labels, value) for data, value in dataset_value_pairs])


def create_rankings1(ocn_file: Path, bs_ranking_file: Path, core_ranking_file: Path) -> None:
    try:
        ocn_df = pd.read_csv(ocn_file)
        bs_ranking = ocn_df.groupby(['BS', 'OCN']).size().reset_index(name='Count')
        bs_ranking.sort_values(by=['OCN', 'Count'], ascending=[True, False], inplace=True)
        
        core_ranking = ocn_df.groupby(['CORE', 'OCN']).size().reset_index(name='Count')
        core_ranking.sort_values(by=['OCN', 'Count'], ascending=[True, False], inplace=True)

        bs_ranking.to_csv(bs_ranking_file, index=False)
        core_ranking.to_csv(core_ranking_file, index=False)
        print("Rankings created and saved.")
    except Exception as e:
        print("An error occurred:", e)

def create_rankings(ocn_file, bs_ranking_file, core_ranking_file):
    try:
        # Load the OCN file
        ocn_df = pd.read_csv(ocn_file)

        # Calculate sum of scores for BS
        bs_ranking = ocn_df.groupby(['BS', 'OCN'])['Score'].sum().reset_index(name='Total_Score')
        # Sort first by OCN and then by Total_Score in descending order
        bs_ranking.sort_values(by=['OCN', 'Total_Score'], ascending=[True, False], inplace=True)
        
        # Calculate sum of scores for CORE
        core_ranking = ocn_df.groupby(['CORE', 'OCN'])['Score'].sum().reset_index(name='Total_Score')
        # Sort first by OCN and then by Total_Score in descending order
        core_ranking.sort_values(by=['OCN', 'Total_Score'], ascending=[True, False], inplace=True)

        # Save to CSV files
        bs_ranking.to_csv(bs_ranking_file, index=False)
        core_ranking.to_csv(core_ranking_file, index=False)

        print("Rankings created and saved.")
    except Exception as e:
        print("An error occurred:", e)

def merge_and_update_data(bs_smarts_file, precursor_file, set1, set2, set3, output_file):
    """Merge precursor files, update with scores and OCN, and save to a new CSV file."""
    
    def get_score(provenance):
        """ Return the score based on the provenance. """
        scores = {
            'Precursor_Cleaned': 5,
            'Set3': 2,
            'Set1': 1,
            'Set2': 1
        }
        return scores.get(provenance, 0)  # Default score is 0 if provenance not found

    try:
        # Load BS to OCN mapping from BS_SMARTS.csv
        bs_smarts_df = pd.read_csv(bs_smarts_file)
        bs_to_ocn = dict(zip(bs_smarts_df['BS'], bs_smarts_df['OCN']))

        # Initialize an empty DataFrame for the merged data
        merged_df = pd.DataFrame()

        # Process each precursor file
        for file in [precursor_file, set1, set2, set3]:
            df = pd.read_csv(file)

            # Add a column for the provenance
            # Use Path object's stem attribute to get file name without extension
            provenance = Path(file).stem  
            df['Provenance'] = provenance

            # Add a column for the score
            df['Score'] = df['Provenance'].apply(get_score)

            # Merge with the main DataFrame
            merged_df = pd.concat([merged_df, df], ignore_index=True)

        # Add the OCN column based on the BS column
        merged_df['OCN'] = merged_df['BS'].map(bs_to_ocn)

        # Save the updated data to a new CSV file
        merged_df.to_csv(output_file, index=False)
        print("Merged file updated and saved to", output_file)
    except Exception as e:
        print("An error occurred:", e)
      
def run_heatmap() -> None:
    
    base_dir = Path(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

    # Input and output directories
    input_dir = base_dir / 'Data' / 'InputData'
    output_dir = base_dir / 'Data' / 'OutputData'

    precursor_file = input_dir / 'Precursor.csv'
    output_alg1 = output_dir / 'Alg1_Output.csv'
    output_alg2 = output_dir / 'Alg2_Output.csv'
    bs_smarts_file = input_dir / 'BS_SMARTS.csv'

    precursor_cleaned_file = output_dir / 'Precursor_Cleaned.csv'
    ocn_file = output_dir / 'OCN_file.csv'
    set1 = output_dir / 'Set1.csv'
    set2 = output_dir / 'Set2.csv'
    set3 = output_dir / 'Set3.csv'
    # Remove potential duplicates and generate cleaned precursor file
    remove_potential_duplicates(precursor_file, precursor_cleaned_file)
    # Rest of the main function
    output_file = output_dir / 'OCN_output.csv'
    output_directory = output_dir
    process_data_and_save_sets(output_dir, output_directory)
    # Generate OCN file using the cleaned precursor file
    merge_and_update_data(bs_smarts_file, precursor_cleaned_file, set1, set2, set3, ocn_file)
    update_precursor_with_ocn(bs_smarts_file, precursor_cleaned_file, output_file)
    #generate_ocn_combinations(ocn_file, output_file)
      
    # Generate rankings
    bs_ranking_file = output_dir / 'BS_ranking.csv'
    core_ranking_file = output_dir / 'CORE_ranking.csv'
    create_rankings(ocn_file, bs_ranking_file, core_ranking_file)
   
    
    core_ranking = pd.read_csv(core_ranking_file)
    bs_ranking = pd.read_csv(bs_ranking_file)
    
    dataset_value_pairs = [
        (pd.read_csv(output_dir / filename), value) 
        for filename, value in [('Precursor_Cleaned.csv', 5), ('Set3.csv', 4), ('Set1.csv', 3), ('Set2.csv', 2), ('OCN_output.csv', -10)]
    ]

    core_labels = core_ranking['CORE'].tolist()
    bs_labels = bs_ranking['BS'].tolist()
    heatmap_data = create_combined_heatmap(dataset_value_pairs, bs_labels, core_labels)

    plt.figure(figsize=(12, 8))
    sns.heatmap(heatmap_data, xticklabels=False, yticklabels=bs_labels, cmap='viridis', cbar=True)
    plt.title('Layered Heatmap')
    plt.xlabel('CORE')
    plt.ylabel('BS')
    plt.savefig(output_dir / 'layered_heatmap.svg', format='svg')
    plt.show()