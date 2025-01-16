import sys
sys.path.append("")
sys.path.append("../../Marie/Util")
sys.path.append("../../Marie")
sys.path.append("../..")
sys.path.append("../../..")
sys.path.append("../../../..")
from Marie.EntityLinking.blink.common.params import BlinkParser
from Marie.EntityLinking.blink.biencoder.train_biencoder import main
if __name__ == "__main__":
    parser = BlinkParser(add_model_args=True)
    parser.add_training_args()
    parser.add_eval_args()

    # args = argparse.Namespace(**params)
    args = parser.parse_args()
    print(args)
    params = args.__dict__
    params['zeshel'] = False
    main(params)