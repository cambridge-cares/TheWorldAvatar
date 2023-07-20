import sys
sys.path.append("")
sys.path.append("../../Marie/Util")
sys.path.append("../../Marie")
sys.path.append("../..")
sys.path.append("../../..")
sys.path.append("../../../..")
from Marie.EntityLinking.blink.common.params import BlinkParser
from Marie.EntityLinking.blink.biencoder.eval_biencoder import main

if __name__ == "__main__":
    parser = BlinkParser(add_model_args=True)
    parser.add_eval_args()

    args = parser.parse_args()
    params = args.__dict__
    params['zeshel'] = False
    params['entity_dict_path'] = './data/tbox/tbox.jsonl'
    mode_list = params["mode"].split(',')
    for mode in mode_list:
        new_params = params
        new_params["mode"] = mode
        main(new_params)