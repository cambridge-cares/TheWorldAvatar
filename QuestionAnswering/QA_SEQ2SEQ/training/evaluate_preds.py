import argparse
import json
import pandas as pd

from core.eval import compute_eval_metrics


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=str)
    parser.add_argument("--multi_domain", action="store_true", default=False)
    args = parser.parse_args()

    df = pd.read_csv(args.input_path)

    print("***Computing evaluation metrics***")
    eval_metrics = compute_eval_metrics(df, multi_domain=args.multi_domain)
    print("Evaluation metrics: ", eval_metrics)

    filename = args.input_path.rsplit(".")[0]
    with open(filename + "_eval.json", "w") as f:
        json.dump(eval_metrics, f, indent=4)

    df["domain_match"] = df["prediction_domain"] == df["groundtruth_domain"]
    df["sparql_match"] = df["prediction_sparql"] == df["groundtruth_sparql"]
    df.to_csv(filename + "_eval.csv", index=False)


if __name__ == "__main__":
    main()
