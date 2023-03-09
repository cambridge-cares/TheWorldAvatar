
def logger(*arg):
    string = ''
    for one_arg in arg:
        string = string + '---' + str(one_arg)

    print('Log: ', string)
    with open('Log.html', 'a') as file:
        file.write('<div class="line">' + string + '</div>')
