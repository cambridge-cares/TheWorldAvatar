from docopt import docopt, DocoptExit
from oscml.jobhandling import JobHandler


__doc__ = """
oscml

Usage:
    oscml <configFile> [options]

    options:
        --hpo=DOHPO                               Run hyperparameter optimisation flag (default True)
        --contour_plot=DOCONTPLOT                 Make contour plot flag (default False)
        --best_trial_retraining=DOBESTRETR        Run the best trial retraining flag (default True)
        --transfer_learning=DOTRANSLEARN          Run the transfer learning flag (default False)
        --predict=DOMODELPREDICTION               Run the model predict flag (default False)
        --trials=TRIALS                           No. of hpo trials to run (default 10)
        --predict_input=SMILESLIST                List of smiles strings to be passed to saved model for pce prediction (no default)
        --actual_output=PCELIST                   List of pce values associated with defined SMILESLIST (no default)
        --jobs=JOBS                               No. of parallel hpo jobs (default 1)
        --epochs=EPOCHS                           No. of epochs to use during training (if applicable, no default)
        --batch_size=BATCHSIZE                    Batch size to use during training (if applicable, no default)
        --patience=PATIENCE                       Early stopping patience parameter to use during training (if applicable, no default)
        --min_delta=MINDELTA                      Early stopping min delta parameter to use during training (if applicable, no default)
        --cross_validation=CROSSVAL               No. of inner cross validation folds (default 0)
        --nested_cross_validation=NESTCROSSVAL    No. of outer cross validation folds (default 0)
        --study_name=STUDYNAME                    Name of the study (default 'oscml_study')
        --storage=STORAGE                         Sqlite storage for this run (default 'sqlite:///oscml_study.db')
        --timeout=TIMEOUT                         Stop study after the given number of second(s). If this argument is not set,
                                                  the study is executed without time limitation (default None)
        --storage_timeout=STORTIMEOUT             Maximum time in seconds for the storage access operations (default 1000)
        --load_if_exists=LOADIFEXISTS             In case where a study already exists in the storage load it (default True)
        --log_config_file=LOGCONFIG               Logging configuration file (default configuration)
        --log_main_dir=LOGDIR                     Main logging directory (default './logs')
        --log_sub_dir_prefix=LOGSUBDIRPREF        Log subfolder prefix (default 'job_')
        --log_file_name=LOGBASENAME               Log file base name (default 'oscml.log')
        --seed=SEED                               Random seed to use (default 1)
        --z_transform_inverse_prediction=INVPRED  Apply z transformation when making predictions (default False)
"""

def run():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: oscml called with wrong arguments.')

    jobHandler = JobHandler(args)
    jobHandler.runJob()

if __name__ == '__main__':
    run()

