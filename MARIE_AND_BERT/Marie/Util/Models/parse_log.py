import math

log = open('training_log').read()

dropout_dict = {}
negative_rate = {}
learning_rate = {}
attempt_list = log.split('=========================================')
for attempt in attempt_list:
    for line in attempt.split('\n'):
        if 'learning rate' in line:
            l_r = float(line.replace('learning rate ', '').strip())
        elif 'dropout' in line:
            dp = float(line.replace('dropout ', '').strip())
        elif 'train loss' in line:
            t_l = float(line.replace('train loss ', '').strip())
        elif 'val loss' in line:
            v_l = float(line.replace('val loss ', '').strip())
        elif 'init_train_loss' in line:
            i_t_l = float(line.replace('init_train_loss ', '').strip())
        elif 'init_val_loss' in line:
            i_v_l = float(line.replace('init_val_loss ', '').strip())
        elif 'delta train' in line:
            delta_train = float(line.replace('delta train ', '').strip())
        elif 'delta val' in line:
            delta_val = float(line.replace('delta val ', '').strip())
        elif 'negative rate' in line:
            n_r = float(line.replace('negative rate ', '').strip())

    delta_val = v_l

    if n_r in negative_rate:
        negative_rate[n_r].append(delta_val)
    else:
        negative_rate[n_r] = [delta_val]

    if dp in dropout_dict:
        dropout_dict[dp].append(delta_val)
    else:
        dropout_dict[dp]= [delta_val]

    if l_r in learning_rate:
        learning_rate[l_r].append(delta_val)
    else:
        learning_rate[l_r] = [delta_val]

# TODO: focus 1, the relation between n_r and delta train


print('Negative rate ======')
for n_r in negative_rate:
    delta_val_list = [x for x in negative_rate[n_r] if not math.isnan(x)]
    print(n_r, ' average : ', sum(delta_val_list) / len(delta_val_list))
    print(n_r, ' max : ', max(delta_val_list))
    print(n_r, ' min : ', min(delta_val_list))
    print('---------------------------')

# TODO: focus 2, the relation between dropout and delta train
print('==================================================')

print('Dropout rate ======')
for n_r in dropout_dict:
    delta_val_list = [x for x in dropout_dict[n_r] if not math.isnan(x)]
    print(n_r, ' average : ', sum(delta_val_list) / len(delta_val_list))
    print(n_r, ' max : ', max(delta_val_list))
    print(n_r, ' min : ', min(delta_val_list))
    print('---------------------------')
# TODO: focus 3, the relation between learning rate and delta train
print('==================================================')

print('Learning rate ======')
for l_r in learning_rate:
    delta_val_list = [x for x in learning_rate[l_r] if not math.isnan(x)]
    print(l_r, ' average : ', sum(delta_val_list) / len(delta_val_list))
    print(l_r, ' max : ', max(delta_val_list))
    print(l_r, ' min : ', min(delta_val_list))
    print('---------------------------')