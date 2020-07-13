import sys
from caresjpsutil import PythonLogger
from config import Constants
from adms_apl_builder import AplDirector, AdmsAplPlantBuilder, AdmsAplShipBuilder
from adms_input_retriever import CliInputContext

pythonLogger = PythonLogger('adms_processor.py')


class AdmsProcessor(object):
    def __init__(self):
        self.input = None

    def get_input(self, args):
        context = CliInputContext(args)
        self.input = context.get_input()

    def save_apl(self, args):
        self.get_input(args)
        builder = None
        if args[1] == Constants.ENTITY_TYPE_PLANT:
            builder = AdmsAplPlantBuilder(self.input)
        elif args[1] == Constants.ENTITY_TYPE_SHIP:
            builder = AdmsAplShipBuilder(self.input)
        director = AplDirector()
        director.set_builder(builder)
        apl = director.get_apl()
        spec = apl.specification()
        with open(str(args[5]) + Constants.FILE_NAME_APL, 'w') as file:
            file.write(spec)
        file.close()


def main(args):
    try:
        processor = AdmsProcessor()
        processor.save_apl(args)
    except Exception as e:
        pythonLogger.postErrorToLogServer(e)


if __name__ == "__main__":
    main(sys.argv)
