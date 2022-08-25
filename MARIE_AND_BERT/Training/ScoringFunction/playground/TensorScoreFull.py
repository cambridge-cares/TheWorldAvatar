# training set: question with e_h and e_t labelled
# other info: embedding

from keras import Model
from keras.metrics import CategoricalAccuracy
from keras.optimizers import Adam
from keras.losses import CategoricalCrossentropy

from Marie.Util.Models.archived_models.ModelBuilder import ModelBuilder

from livelossplot import PlotLossesKeras
from Training.Util.TrainingDataWarehouse import TrainingDataWarehouse


class CandidateScore():
    def __init__(self):
        # init the model
        # the model shall take the embedding of q, e_h, e_t and produce a 0 to 1 score for the candidate combination
        # set the correct answer to be 1, incorrect answer to be 0 in the training set.
        self.model = Model()
        self.training_data_warehouse = TrainingDataWarehouse()
        self.x_train, self.y_train, self.x_test, self.y_test = self.training_data_warehouse.get_pubchem_score_data(frac=0.2)
        self.model_builder = ModelBuilder()
        self.model = self.build_model()

    def build_model(self):
        return self.model_builder.get_scoring_model()

    def train(self):
        optimizer = Adam(
            learning_rate=0.0000001,  # this learning rate is for bert model , taken from huggingface website
            epsilon=1e-08,
            decay=0.01,
            clipnorm=2)
        # Set loss and metrics
        loss = CategoricalCrossentropy()
        metric = CategoricalAccuracy('balanced_accuracy')
        self.model.compile(optimizer=optimizer, loss=loss, metrics=metric)
        train_history = self.model.fit(
            x=self.x_train,
            y=self.y_train,
            validation_data=(
                self.x_test, self.y_test
            ),
            epochs=120,
            callbacks=[PlotLossesKeras()],
            batch_size=32
        )
        self.model.save_weights('model_weight.h5')


if __name__ == '__main__':
    TSF = CandidateScore()
    TSF.train()
