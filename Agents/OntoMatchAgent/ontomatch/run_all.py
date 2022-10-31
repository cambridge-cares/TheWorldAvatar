import os


def run_all():
    
    main_dir = './experiments'

    configs_20 = (main_dir + '/' + 'autocal_mtf_20', 
        ['conf_kg_auto_20.json', 'conf_dg_auto_20.json', 'conf_fz_auto_20.json', 'conf_ag_auto_20.json', 'conf_ds_auto_20.json',  'conf_da_auto_20.json']) 
    
    configs_50 = (main_dir + '/' + 'autocal_mtf_50', 
        ['conf_kg_auto_50.json', 'conf_dg_auto_50.json', 'conf_fz_auto_50.json', 'conf_ag_auto_50.json', 'conf_ds_auto_50.json',  'conf_da_auto_50.json']) 
    
    configs_100 = (main_dir + '/' + 'autocal_mtf_100', 
        ['conf_kg_auto_100.json', 'conf_dg_auto_100.json', 'conf_fz_auto_100.json', 'conf_ag_auto_100.json', 'conf_ds_auto_100.json',  'conf_da_auto_100.json']) 

    configs_200 = (main_dir + '/' + 'autocal_mtf_200', 
        ['conf_kg_auto_200.json', 'conf_dg_auto_200.json', 'conf_fz_auto_200.json', 'conf_ag_auto_200.json', 'conf_ds_auto_200.json',  'conf_da_auto_200.json']) 
    
    #configs = [configs_20, configs_50, configs_100, configs_200]
    configs = [configs_50]

    cmd_template = """python ontomatch/coordinator.py --logconfdir ./conf --logdir {0} --datadir {1} --config {2}"""

    data_dir = main_dir + '/data'
    for dir, files in configs:
        for config_name in files:
            log_dir = dir
            config_path = dir + '/' + config_name
            cmd = cmd_template.format(log_dir, data_dir, config_path)
            print(cmd)
            os.system(cmd)


if __name__ == '__main__': 
    run_all() 