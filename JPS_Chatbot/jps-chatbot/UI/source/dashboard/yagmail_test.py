import yagmail
# yagmail.register('error-message-marie', 'somerandompasswordformarie')
# yag = yagmail.SMTP('error-message-marie')
# receiver = ['xz378@cam.ac.uk']
# # subject = 'Test message from marie email module'
# # body = 'You got the message!'
# #
# # yag.send(to = receiver[0], subject = subject, contents = body)
#
#
# import yagmail
# yag = yagmail.SMTP()
# contents = [
#     "This is the body, and here is just text http://somedomain/image.png",
#     "You can find an audio file attached.", '/local/path/to/song.mp3'
# ]
# yag.send(receiver[0], 'subject', contents)

# Alternatively, with a simple one-liner:
yagmail.SMTP('marie.maintenance.message', 'somerandompasswordformarie').send('xz378@cam.ac.uk', 'Error message from Marie', 'something')