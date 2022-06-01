'''
The experiment on KG embedding will conduct tests on different models, hyper-parameters and training sets.

The target models include: TransE, TransR, TransH, RotatE, Complex, DistMult

Target hyper-parameters are
 - batch_size (16, 32, 64, 128)
 - learning_rate
 - neg_rate ([1 - inf))

Training sets include: Pubchem mini (current size), Pubchem Medium  (500000 species)
All epochs ar set to 500 ...

'''
import os
import shutil
import subprocess

main_dir = r'../../../Dataset'
model_name_list = ['TransE', 'TransR', 'RotatE', 'Complex', 'DistMult']
# dataset_list = ['pubchemini', 'pubchemmedium', 'pubchemmediumsimple']
dataset_list = ['pubchemini','pubchemmediumsimple']
batch_size_list = [16,32,64,128,256,512]


training_data_dir = os.path.join(main_dir, 'TrainingData')

def construct_training_name(config):
    training_name = f'{config.model_name}'


def create_new_directory(dir_name):
    full_path = os.path.join(main_dir, dir_name)
    if os.path.isdir(full_path):
        os.rmdir(full_path)
    # os.mkdir(full_path)
    shutil.copytree(training_data_dir, full_path) # copy all the training dataset to the new folder
    return full_path


def create_test_parameters_list():
    for model_name in model_name_list:
        for dataset in dataset_list:
            for batch_size in batch_size_list:
                dir_name = f'{model_name}_{dataset}_{batch_size}'
                _full_path = create_new_directory(dir_name)
                call_command_line(dataset_dir=_full_path, model_name=model_name, dataset_name=dataset,
                                  batch_size=batch_size)


def try_something():
    # create_new_directory('test')
    call_command_line()


def call_command_line(dataset_dir, model_name, dataset_name, batch_size):

    if batch_size < 128:
        epoch = 100
    else:
        epoch = 500
    batch_size = str(batch_size)
    subprocess.call(['pykg2vec-train',
                     '-dsp', f'{dataset_dir}',
                     '-mn', f'{model_name}',
                     '-ds', f'{dataset_name}',
                     '-device', 'cuda',
                     '-tn', '10',
                     '-ts', '500',
                     '-b', f'{batch_size}',
                     '-l', str(epoch)
                     ])


if __name__ == '__main__':
    create_test_parameters_list()

'''
Generic Hyperparameters:
  -lmda LMBDA           The lmbda for regularization.
  -b BATCH_SIZE         training batch size
  -mg MARGIN            Margin to take
  -opt OPTIMIZER        optimizer to be used in training.
  -s SAMPLING           strategy to do negative sampling.
  -ngr NEG_RATE         The number of negative samples generated per positve one.
  -l EPOCHS             The total number of Epochs
  -lr LEARNING_RATE     learning rate
  -k HIDDEN_SIZE        Hidden embedding size.
  -km ENT_HIDDEN_SIZE   Hidden embedding size for entities.
  -kr REL_HIDDEN_SIZE   Hidden embedding size for relations.
  -k2 HIDDEN_SIZE_1     Hidden embedding size for relations.
  -l1 L1_FLAG           The flag of using L1 or L2 norm.
  -al ALPHA             The alpha used in self-adversarial negative sampling.
  -fsize FILTER_SIZES [FILTER_SIZES ...]
                        Filter sizes to be used in convKB which acts as the widths of the kernals
  -fnum NUM_FILTERS     Filter numbers to be used in convKB and InteractE.
  -fmd FEATURE_MAP_DROPOUT
                        feature map dropout value used in ConvE and InteractE.
  -idt INPUT_DROPOUT    input dropout value used in ConvE and InteractE.
  -hdt HIDDEN_DROPOUT   hidden dropout value used in ConvE.
  -hdt1 HIDDEN_DROPOUT1
                        hidden dropout value used in TuckER.
  -hdt2 HIDDEN_DROPOUT2
                        hidden dropout value used in TuckER.
  -lbs LABEL_SMOOTHING  The parameter used in label smoothing.
  -cmax CMAX            The parameter for clipping values for KG2E.
  -cmin CMIN            The parameter for clipping values for KG2E.
  -fp FEATURE_PERMUTATION
                        The number of feature permutations for InteractE.
  -rh RESHAPE_HEIGHT    The height of the reshaped matrix for InteractE.
  -rw RESHAPE_WIDTH     The width of the reshaped matrix for InteractE.
  -ks KERNEL_SIZE       The kernel size to use for InteractE.
  -ic IN_CHANNELS       The kernel size to use for InteractE.
  -w {serial,parallel}  The way used by AcrE to organize standard convolution and atrous convolutions.
  -fa FIRST_ATROUS      The first layer expansion coefficient to use for Acre
  -sa SECOND_ATROUS     The second layer expansion coefficient to use for Acre
  -ta THIRD_ATROUS      The third layer expansion coefficient to use for Acre
  -ab                   Whether to use bias in the Acre model

Generic:
  -mn MODEL_NAME        Name of model
  -db DEBUG             To use debug mode or not.
  -exp EXP              Use Experimental setting extracted from original paper. (use Freebase15k by default)
  -ds DATASET_NAME      The dataset name (choice: fb15k/wn18/wn18_rr/yago/fb15k_237/ks/nations/umls)
  -dsp DATASET_PATH     The path to custom dataset.
  -ld LOAD_FROM_DATA    The path to the pretrained model.
  -sv SAVE_MODEL        Save the model!
  -tn TEST_NUM          The total number of test triples
  -ts TEST_STEP         Test every _ epochs
  -t TMP                The folder name to store trained parameters.
  -r RESULT             The folder name to save the results.
  -fig FIGURES          The folder name to save the figures.
  -plote PLOT_EMBEDDING
                        Plot the entity only!
  -plot PLOT_ENTITY_ONLY
                        Plot the entity only!
  -device {cpu,cuda}    Device to run pykg2vec (cpu or cuda).
  -npg NUM_PROCESS_GEN  number of processes used in the Generator.
  -hpf HP_ABS_FILE      The path to the hyperparameter configuration YAML file.
  -ssf SS_ABS_FILE      The path to the search space configuration YAML file.
  -mt MAX_NUMBER_TRIALS
                        The maximum times of trials for bayesian optimizer.

'''
