import logging

def config_logging(log_file, log_file_name, no_logging):
    logging.basicConfig(
            level=logging.DEBUG,
            format='%(asctime)s [%(threadName)s] [%(levelname)s] %(message)s',
            handlers=[
                        logging.FileHandler(filename=log_file, mode='w'),
                        logging.StreamHandler()
                    ]
            )