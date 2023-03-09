import logging
import os
import optuna
import optuna.samplers
from pathlib import Path

os.environ["SLURM_JOB_NAME"]="bash"

def get_statistics(study):
    running_trials = [t for t in study.trials if t.state == optuna.trial.TrialState.RUNNING]
    completed_trials = [t for t in study.trials if t.state == optuna.trial.TrialState.COMPLETE]
    pruned_trials = [t for t in study.trials if t.state == optuna.trial.TrialState.PRUNED]
    failed_trials = [t for t in study.trials if t.state == optuna.trial.TrialState.FAIL]
    return {'all': len(study.trials), 'running': len(running_trials), 'completed': len(completed_trials),
            'pruned': len(pruned_trials), 'failed': len(failed_trials)}

def create_study(direction, seed, **kwargs):
    # pruner = optuna.pruners.MedianPruner()
    # pruner = optuna.pruners.MedianPruner(n_startup_trials=5, n_warmup_steps=30, interval_steps=10)
    # pruner = optuna.pruners.PercentilePruner(25.0, n_startup_trials=5, n_warmup_steps=30, interval_steps=10) #keep top 25%
    # #pruner = ThresholdPruner(upper=1.0)
    #pruner = optuna.pruners.HyperbandPruner(min_resource=1, max_resource=9, reduction_factor=3)
    pruner = None
    sampler = optuna.samplers.TPESampler(consider_prior=True, n_startup_trials=10, seed=seed)
    study = optuna.create_study(direction=direction, pruner=pruner, sampler=sampler, **kwargs)
    return study

def callback_on_trial_finished(study, trial):
    statistics = get_statistics(study)
    logging.info('current study statistics: number of trials=%s', statistics)
    if statistics['failed'] >= 50:
        logging.error('THE MAXIMUM NUMBER OF FAILED TRIALS HAS BEEN REACHED, AND THE STUDY WILL STOP NOW.')
        study.stop()

def create_objective_decorator(objective, n_trials):
    def decorator(trial):
        try:
            logging.info('starting trial %s / %s', trial.number, abs(n_trials-1))
            value = objective(trial)
            logging.info('finished trial %s / %s', trial.number, abs(n_trials-1))
            return value
        except optuna.exceptions.TrialPruned as exc:
            logging.info('pruned trial, trial number=%s', trial.number)
            raise exc
        except Exception as exc:
            message = 'failed trial, trial number=' + str(trial.number)
            logging.exception(message, exc_info=True)
            raise exc

    return decorator

def log_and_save(study, log_dir, statistics):
    path = log_dir + '/hpo_result.csv'
    logging.info('Saving HPO results to %s', path)

    if not os.path.exists(log_dir):
        os.makedirs(log_dir)

    df = study.trials_dataframe()
    df.to_csv(path)

    logging.info('final study statistics: number of trials=%s', statistics)

    log_best_trial(study.best_trial)

def log_best_trial(trial):
    logging.info('best trial number=%s', trial.number)
    logging.info('best trial value=%s', trial.value)
    logging.info('best trial params=%s', trial.params)

def runHPO(objective, config, total_number_trials):

    try:
        seed = int(config['numerical_settings']['seed'])
        study_name = config['training']['study_name']
        direction = config['training']['direction']
        storage_url = config['training']['storage']
        storage_timeout = config['training']['storage_timeout']
        load_if_exists = config['training']['load_if_exists']
        n_trials = int(config['training']['trials'])
        n_jobs = int(config['training']['jobs'])
        job_timeout = config['training']['timeout']
        job_timeout = int(job_timeout) if job_timeout is not None else job_timeout
        logDir = objective.objConfig['log_dir']

        if storage_url:
            storage = optuna.storages.RDBStorage(
            url=storage_url,
            engine_kwargs={"connect_args": {"timeout": storage_timeout}},
            )
        else:
            storage = None

        if n_trials==0 and storage is None:
            logging.error('Storage must be specified if the number of hpo trials is set to zero.')
            return

        study = create_study(direction=direction, seed=seed, storage=storage, study_name=study_name, load_if_exists=load_if_exists)
        decorator = create_objective_decorator(objective, total_number_trials)
        logging.info('starting HPO')
        study.optimize(decorator, n_trials=n_trials, n_jobs=n_jobs, timeout=job_timeout,
                catch = (RuntimeError, ValueError, TypeError), callbacks=[callback_on_trial_finished],
                gc_after_trial=True)
        logging.info('finished HPO')
        log_and_save(study, logDir, get_statistics(study))
        best_value = study.best_trial.value

        return best_value

    except BaseException as exc:
        print(exc)
        logging.exception('finished with exception', exc_info=True)
        raise exc
    else:
        logging.info('finished successfully')

def check_for_existing_study(storage, study_name):
    n_previous_trials = 0
    try:
        if storage:
            study_found = False
            summary = optuna.study.get_all_study_summaries(storage=storage)
            for existing_study in summary:
                if existing_study.study_name == study_name:
                    study_found = True
                    n_previous_trials = existing_study.n_trials
                    logging.info('found a study with name=%s and %s trials', storage, n_previous_trials)
                    if existing_study.best_trial:
                        log_best_trial(existing_study.best_trial)
                    else:
                        logging.info('no best trial so far')

        if not study_found:
            logging.info('there is no study with name=%s so far', storage)

    except:
        logging.info('exception - there is no study with name=%s so far', storage)
    return n_previous_trials

def runPostProcTraining(objective, jobConfig):
    study_name = jobConfig['training']['study_name']
    storage_url = jobConfig['training']['storage']
    load_if_exists = jobConfig['training']['load_if_exists']
    storage_timeout = jobConfig['training']['storage_timeout']
    seed = jobConfig['numerical_settings']['seed']
    direction = jobConfig['training']['direction']
    logDir = objective.objConfig['log_dir']
    logHead = objective.objConfig['log_head']

    if storage_url:
        storage = optuna.storages.RDBStorage(
        url=storage_url,
        engine_kwargs={"connect_args": {"timeout": storage_timeout}},
        )
    else:
        storage = None

    study = create_study(seed=seed, direction=direction, storage=storage, study_name=study_name, load_if_exists=load_if_exists)

    logHead = logHead + str(study.best_trial.number) + ']'
    logging.info(logHead)

    Path(logDir).mkdir(parents=True, exist_ok=True)
    objective(study.best_trial)

def runModelPredict(objective, jobConfig):
    logDir = objective.objConfig['log_dir']
    logHead = objective.objConfig['log_head']
    logging.info('['+logHead+']')

    Path(logDir).mkdir(parents=True, exist_ok=True)

    objective(0)