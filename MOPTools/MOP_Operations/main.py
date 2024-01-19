from workflow_manager.assembly_workflow import workflow


def start():
    print("############################")
    print("For Assembly Models in KG = 1")
    print("To construct search radius = 2")
    x = input("Select the operations you would like to run:")
    options = ['1','2']
    if x not in options:
        print("Wrong Entry")
        pass
    else:
        display = workflow(x)
        print(display)

if __name__ == '__main__':
    start()