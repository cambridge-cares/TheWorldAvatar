import os



def test_tensorflow():
    import tensorflow as tf
    if tf.test.gpu_device_name():

        print('Default GPU Device:{}'.format(tf.test.gpu_device_name()))

    else:

       print("Please install GPU")


def test_pytorch():
    import torch
    print(f'torch version {torch.__version__}')
    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")

    print(f'=========== USING {device} ===============')


if __name__ == '__main__':
    test_pytorch()

