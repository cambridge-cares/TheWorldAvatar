import logging
import pandas as pd
import sklearn
import sklearn.model_selection


class DataTransformer():
    def __init__(self, df, transform_y_cols, transform_x_cols):
        self.transform_y_cols = transform_y_cols
        self.transform_x_cols = transform_x_cols
        self.mean_x = None
        self.std_x = None
        if transform_x_cols is not None:
            self.mean_x = list(df[transform_x_cols].mean())
            self.std_x = list(df[transform_x_cols].std(ddof=0))
            logging.info('calculated features mean_x=%s, std_x=%s', self.mean_x, self.std_x)

        self.mean_y = None
        self.std_y = None
        if transform_y_cols is not None:
            self.mean_y = [df[transform_y_cols].mean()]
            self.std_y = [df[transform_y_cols].std(ddof=0)]
            logging.info('calculated target mean_y=%s, std_y=%s', self.mean_y, self.std_y)

    def transform_x(self, data):
        if self.transform_x_cols:
            return (data - self.mean_x) / self.std_x
        else:
            return data

    def transform_y(self, data):
        if self.transform_y_cols:
            return (data - self.mean_y) / self.std_y
        else:
            return data

    def inverse_transform_x(self, data):
        if self.transform_x_cols:
            return data * self.std_x + self.mean_x
        else:
            return data

    def inverse_transform_y(self, data):
        if self.transform_y_cols:
            return data * self.std_y + self.mean_y
        else:
            return data


def create_transformer(df, transform_y_cols = None, transform_x_cols = None):
    return DataTransformer(
        df = df,
        transform_x_cols=transform_x_cols,
        transform_y_cols=transform_y_cols
    )


def store(df, filepath):
    logging.info('storing %s', filepath)
    # store without the internal index of Pandas Dataframe
    df.to_csv(filepath, index=False)

def read_and_split_by_size(filepath, split_size_array, seed):
    logging.info('reading %s', filepath)
    df = pd.read_csv(filepath)
    #df=(df-df.mean())/df.std()

    train_size, val_size, test_size = split_size_array
    if not train_size:
        train_size = len(df) - val_size - test_size
    elif not val_size:
        val_size = len(df) - train_size - test_size
    elif not test_size:
        test_size = len(df) - train_size - val_size

    train_plus_val_size = train_size + val_size
    df_train, df_test = sklearn.model_selection.train_test_split(df,
                    train_size=train_plus_val_size, shuffle=True, random_state=seed)
    df_train, df_val = sklearn.model_selection.train_test_split(df_train,
                    train_size=train_size, shuffle=True, random_state=seed+1)
    logging.info('train=%s, val=%s, test=%s', len(df_train), len(df_val), len(df_test))

    return df_train, df_val, df_test

def read_and_split(filepath, split_column='ml_phase'):
    logging.info('reading %s', filepath)
    df = pd.read_csv(filepath)
    #df=(df-df.mean())/df.std()
    df_train = df[(df[split_column] == 'train')].copy()
    df_val = df[(df[split_column] == 'val')].copy()
    df_test = df[(df[split_column] == 'test')].copy()
    logging.info('split data into sets of size (train / val / test)=%s / %s / %s', len(df_train), len(df_val), len(df_test))
    return df_train, df_val, df_test

def get_dataframes(dataset, seed=200, cvFold=None, nestedCvFolds=None):

    src = dataset['src']
    x_column = dataset['x_column']
    y_column = dataset['y_column'][0]

    if cvFold is not None:
        split = dataset['split'] + '_fold_'+str(cvFold)
    else:
        split = dataset['split']

    if isinstance(split, str):
        # split is the name of the split column with values train, val and test
        df_train, df_val, df_test = read_and_split(src, split_column=split)
    else:
        # split is an array specifying the number of samples for train, val and test set
        df_train, df_val, df_test = read_and_split_by_size(src, split_size_array=split, seed=seed)

    transformer = create_transformer(df_train, transform_y_cols=y_column, transform_x_cols=x_column)
    return (df_train, df_val, df_test, transformer)


def add_k_fold_columns(df, k, seed, column_name_prefix='ml_phase'):
    kfold = sklearn.model_selection.KFold(n_splits=k, shuffle=True, random_state=seed)
    k=0
    for train_index, test_index in kfold.split(df):
        #print(len(train_index), len(test_index))
        #print(test_index[:20])
        column_name = column_name_prefix + '_fold_' + str(k)
        df[column_name] = ''
        column_index = df.columns.get_loc(column_name)
        #print('COL IND', column_index)
        df.iloc[train_index, column_index] = 'train'
        df.iloc[test_index, column_index] = 'test'
        k += 1