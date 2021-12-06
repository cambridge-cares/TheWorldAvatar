from z_manager.assembly_workflow import workflow

def start():
    print("Running Discovery Workflow")
    searchanalytics = workflow()
    print("Overall results from R1")
    print(searchanalytics[0])
    print("Overall results from R2")
    print(searchanalytics[1])


if __name__ == '__main__':
    start()