import logging
import optuna

# returns value(s) of a parameter defined in the config file
def set_config_param(trial, param_name, param, all_params):
    # trial      - optuna trial object
    # param_name - base paramter name you wish to use
    # param      - parameter from the config, can be a single number, string, list or dictionary
    # all_params - dictionary of all processed parameters so far

    # if param is a list, process it as list
    if isinstance(param, list):
        # 1. e.g. param : [1,2,3,...]
        param_suggestion = set_config_param_list(trial, param_name, param)
    elif isinstance(param, dict):
        starting_value = get_starting_value(param, all_params)
        length = get_length_value(param, all_params)
        if 'values' in param:
            # 2. param : {"values": [1,3,4], "length":5}
            #    param : {"values": [1,3,4]} - also supported, but it is better then to just use param : [1,3,4]
            param_suggestion = set_config_param_list(trial, param_name, param['values'], length, starting_value)
        elif 'type' in param:
            if length is not None:
                # 3. param : {"optuna params and values", "length":5}
                #    param : {"optuna params and values", "length":5, "starting_value":...}
                param_suggestion = set_config_param_list(trial, param_name, param, length, starting_value)
            else:
                # 4. param : {"optuna params and values"}
                #    param : {"optuna params and values","starting_value":...}
                param_suggestion = set_config_param_single(trial, param_name, param, starting_value)
        elif 'length' in param and 'starting_value' in param:
            if isinstance(starting_value, list):
                param['values'] = starting_value
            else:
                param['values'] = [starting_value]
            starting_value = None
            param_suggestion = set_config_param_list(trial, param_name, param['values'], length, starting_value)
        else:
            # 5. param : {"starting_value":".."} - sinlge valued param linked with some other param
            param_suggestion = set_config_param_single(trial, param_name, param, starting_value)
    else:
        # this is a single value, fixed parameter:
        # 6. param : 1 or param : "Adam" etc..
        param_suggestion = set_config_param_single(trial, param_name, param)
    return param_suggestion


def get_length_value(param, all_params):
    length = param.get('length')
    if length is not None:
        if isinstance(length, str):
            length, val_idx = get_key_and_idx(length)
            if isinstance(all_params[length], list):
                if val_idx is not None:
                    length = all_params[length][val_idx]
                else:
                    length = len(all_params[length])
            else:
                length = all_params[length]
    return length

def get_key_and_idx(key_idx_str):
    idx = None
    key_str = key_idx_str
    if key_str is not None:
        if '[' in key_idx_str and ']' in key_idx_str:
            key_str, idx = key_idx_str.split('[')
            idx = int(idx.split(']')[0])
    return key_str, idx

def get_starting_value(param, all_params):
    starting_value = param.get('starting_value')
    if starting_value is not None:
        if isinstance(starting_value, str):
            starting_value, value_idx = get_key_and_idx(starting_value)
            if value_idx is not None:
                # case when "starting_value": "a_string[idx]" and "a_string" is a key in the "all_params" dictionary
                starting_value = all_params[starting_value][value_idx]
            else:
                if starting_value in all_params:
                    # case when "starting_value": "a_string" and "a_string" is a key in the "all_params" dictionary
                    starting_value = all_params[starting_value]
                #else:
                #    # case when "starting_value": "a_string" where "a_string" is not a key in the "all_params" dictionary
                #    return starting_value
        #else:
        #    # case when e.g. "starting_value": 4 or "starting_value": 1.3 etc..
        #    return starting_value
    #else:
    return starting_value

# sets model parameter that can have mulitple values (list)
def set_config_param_list(trial, param_name, param, length=None, starting_value=None):
    if length is None:
        length = len(param)
    param_suggestions = []
    i = 0
    while i < length:
        suggested_value = set_config_param_single(trial=trial,param_name=param_name+'_{}'.format(i),param=param, starting_value=starting_value)
        if starting_value is not None:
            if isinstance(starting_value, list):
                for j in range(len(starting_value)):
                    param_suggestions.append(suggested_value[j])
                    if j >= length:break
                i = j
            else:
                param_suggestions.append(suggested_value)

            starting_value = None
            param = apply_direction(param,param_suggestions[-1])
            i = i + 1
        else:
            suggested_value = process_list_param(suggested_value,i)
            param_suggestions.append(suggested_value)
            param = apply_direction(param,suggested_value)
            i = i + 1
    return param_suggestions

# sets a single model parameter value from the config file
def set_config_param_single(trial, param_name, param, starting_value=None):
    if starting_value is not None:
        # remove starting_value from param
        # so it is processed / applied only once
        return starting_value
    elif isinstance(param, dict):
        if 'type' in param:
            try:
                param_local = param.copy()
                param_local.pop('direction', None) # remove "direction" if exists
                param_local.pop('length', None) # remove "length" if exists
                param_local.pop('starting_value', None) # remove "starting_value" if exists
                param_type = param_local.pop('type')
                if param_type == 'categorical':
                    # name, choices
                    return trial.suggest_categorical(name=param_name, **param_local)
                elif param_type == 'discrete_uniform':
                    # name, low, high, q
                    return trial.suggest_discrete_uniform(name=param_name, **param_local)
                elif param_type == 'float':
                    # name, low, high, *[, step, log]
                    return trial.suggest_float(name=param_name, **param_local)
                elif param_type == 'int':
                    # name, low, high[, step, log]
                    return trial.suggest_int(name=param_name, **param_local)
                elif param_type == 'loguniform':
                    # name, low, high
                    return trial.suggest_loguniform(name=param_name, **param_local)
                elif param_type == 'uniform':
                    # name, low, high
                    return trial.suggest_uniform(name=param_name, **param_local)
            except KeyError as exc:
                logging.exception('', exc_info=True)
                raise exc
            except TypeError as exc:
                logging.exception('', exc_info=True)
                raise exc
            except ValueError as exc:
                logging.exception('', exc_info=True)
                raise exc
        else:
            return param
    else:
        return param

def process_list_param(param,i):
    if isinstance(param,list):
        if i < len(param):
            param = param[i]
        else:
            param = param[-1]
    return param

def apply_direction(param,prev_value):
    if isinstance(param, dict):
        if 'direction' in param:
            if param['direction']=="decreasing":
                param['high'] = prev_value
                if param['high'] < param['low']:
                    param['low'] = param['high']
            elif param['direction']=="increasing":
                param['low'] = prev_value
                if param['low'] > param['high']:
                    param['high'] = param['low']
            elif param['direction']=="constant":
                param = prev_value
    return param