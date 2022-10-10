import yagmail

"""
The ``Marie and Bert`` a.k.a `Marie 3.0` project is developed by [Xiaochi Zhou](xz378@cam.ac.uk)
and [Shaocong Zhang](sz375@cam.ac.uk).
[Kok Foong Lee](kflee@cmclinnovations.com) and [Michael Hillman](mdhillman@cmclinnovations.com) will provide technical support from CMCL and will be charge of deployment and monitoring of performance.

"""

# TODO: 
class Messenger:
    def __init__(self):
        self.yag = yagmail.SMTP('mariebert.maintenance', 'mariemaintenancepassword')
        # self.default_receivers = ['xz378@cam.ac.uk', 'jimmyzhou.ntu@gmail.com', 'kflee@cmclinnovations.com',
        # 'mdhillman@cmclinnovations.com']
        self.default_receivers = ['xz378@cam.ac.uk', 'jimmyzhou.ntu@gmail.com']
        self.main_developer = ['xz378@cam.ac.uk', 'sz375@cam.ac.uk']

    def send_message(self, receiver, subject, message):
        self.yag.send(to=receiver, subject=f'MARIE 3.0 - AUTOMATED MESSAGE {subject}', contents=message)

    # def send_message(self, receiver, message):
    #     self.yag.send(to=receiver, subject='TEST MESSAGE FROM MESSENGER', contents='THIS IS A TEST MESSAGE')


if __name__ == '__main__':
    my_messenger = Messenger()
    my_messenger.send_message(my_messenger.default_receivers, "TEST MESSAGE", "This is a message sent from Marie")
