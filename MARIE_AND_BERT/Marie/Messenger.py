# import smtplib
# conn =smtplib.SMTP('smtp.office365.com',587)
# type(conn)
# conn.ehlo()
# conn.starttls()
# conn.login('marie.bert.message','tvDm=Zt24')
# conn.sendmail('from','CcBcc','Subject:')
# conn.quit()

import smtplib



import smtplib


def print_hi(name):
    sender = 'marie.bert.message@outlook.com'
    receivers = ['xz378@cam.ac.uk']
    message = """some msg"""

    server = smtplib.SMTP('smtp.office365.com', 587)
    server.set_debuglevel(1)
    server.starttls()
    server.ehlo()
    server.login('marie.bert.message', 'tvDm=Zt24')

    server.sendmail(sender, receivers, message)
    server.quit()


if __name__ == '__main__':
    print_hi('PyCharm')