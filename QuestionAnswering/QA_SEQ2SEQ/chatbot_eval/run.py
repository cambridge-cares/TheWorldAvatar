from argparse import ArgumentParser
import json
import os
import time

from openai import OpenAI
import pandas as pd
from tqdm import tqdm


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--endpoint", type=str, required=True)
    parser.add_argument(
        "--api_key", type=str, default=os.getenv("OPENAI_API_KEY", "placeholder")
    )
    parser.add_argument("--model", type=str, default="placeholder")
    parser.add_argument("--no_system_message", action="store_true", default=False)
    parser.add_argument("--prompt_template", type=str, required=True)
    parser.add_argument("--input_data", type=str, required=True)
    parser.add_argument("--output_file", type=str, required=True)
    args = parser.parse_args()

    df = pd.read_csv(args.input_data)
    with open(args.prompt_template, "r") as f:
        prompt_template = json.load(f)
    openai_client = OpenAI(base_url=args.endpoint, api_key=args.api_key)

    results = []

    for _, row in tqdm(df.iterrows()):
        start = time.time()
        timestamp_first_token = None
        tokens = []

        system_msg = prompt_template["system"]
        user_msg = prompt_template["user"].format(**row.to_dict())
        if args.no_system_message:
            messages = [
                dict(
                    role="user",
                    content="{system}\n\n\n{user}".format(
                        system=system_msg, user=user_msg
                    ),
                )
            ]
        else:
            messages = [
                dict(role="system", content=system_msg),
                dict(role="user", content=user_msg),
            ]
        token_num = 0
        for chunk in openai_client.chat.completions.create(
            model=args.model,
            messages=messages,
            stream=True,
        ):
            if timestamp_first_token is None:
                timestamp_first_token = time.time()
            content = chunk.choices[0].delta.content
            if content is not None:
                tokens.append(content)
                token_num += 1
        end = time.time()

        results.append(
            dict(
                system_message=system_msg,
                user_message=user_msg,
                model=args.model,
                response="".join(tokens),
                time_to_first_token=timestamp_first_token - start,
                time_to_completion=end - start,
                token_num=token_num,
            )
        )

    pd.DataFrame(results).to_csv(args.output_file, index=False)
