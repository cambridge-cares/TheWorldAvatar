from b_lib1_creation.assembly_lib import cbu_gbu_lib

def lib1_Creation(uniques):
    for model in uniques: # The function accesses saved json files
        cbu_gbu_lib(model)
        print(model+"_____________"+"conveted to json")

    