from blink.common.params import BlinkParser
from blink.biencoder.train_biencoder import main
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