################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# This module will record your CEDA username and password (as Encrypted text) through a popped window, 
# and saved in the ./downloads/.env path, which can be retrieved as environmental variable. 
# Note that this module will try to login in to CEDA using the username and password you provided, 
# only if the login success, .env will be saved. If the login failed for more than 5 times which will raise 
# an InvalidInputError and stop the module.

import PySimpleGUI as sg
import os
from cryptography.fernet import Fernet
import requests
from bs4 import BeautifulSoup 

import agentlogging
from agent.errorhandling.exceptions import InvalidInput

# Initialise logger
logger = agentlogging.get_logger("prod")

def login_trival():
    '''
    Try to login to CEDA and test if the username/password is correct
    '''
    url = 'https://auth.ceda.ac.uk/account/signin/?r=http%3A%2F%2Fservices.ceda.ac.uk%2Fcedasite%2Fmyceda%2Fuser%2F'
    # Create a session to persist the login
    session = requests.Session()
    
    CEDA_USERNAME = os.getenv('CEDA_USERNAME')
    # Retrieve the encrypted password from the environment variable
    # Decrypt the password
    encrypted_password = os.getenv('CEDA_PASSWORD')
    CEDA_PASSWORD = str(cipher.decrypt(encrypted_password), 'utf-8')
    
    # Define the credentials
    credentials = {
        "username": CEDA_USERNAME,
        "password": CEDA_PASSWORD
    }

    # Get the login page to get the csrf token
    response = session.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    csrf_token = soup.find('input', {'name': 'csrfmiddlewaretoken'}).get('value')
    # Add the csrf token to the credentials
    credentials['csrfmiddlewaretoken'] = csrf_token

    # Make a post request to the login url with the credentials
    response = session.post(url, data=credentials)
    if response.status_code != 200:
        sg.PopupError(f'Response {response.status_code} CEDA platform fail to connect! -- Try to login later')
        logger.error(f'Response {response.status_code} CEDA platform fail to connect! -- Try to login later')
    if "auth_tkt" in session.cookies:
        statue = "Logged in"
    else:
        statue = "Not logged in"

    return statue

def record_login_info():
    attempts = 0
    while True:
        layout = [
            [sg.Text('NOTE: The login process make take a bit time, please be patient...')],
            [sg.Text('Enter your CEDA username:'), sg.Input()],
            [sg.Text('Enter your CEDA password:'), sg.Input(password_char='*')],
            [sg.Button('Submit'), sg.Cancel()]
        ]

        window = sg.Window('Password Input', layout)
        event, values = window.read()
        window.close()

        if event == 'Submit':
            username = values[0]
            password = bytes(values[1], "utf-8")
            # Generate a key for encrypting/decrypting the password
            key = Fernet.generate_key()
            global cipher
            cipher = Fernet(key)

            # Encrypt the password
            cipher_text = cipher.encrypt(password)

            # store the username in an environment variable
            os.environ["CEDA_USERNAME"] = username
            os.environ['CEDA_KEY'] = key.decode()
            # Store the encrypted password in an environment variable
            os.environ["CEDA_PASSWORD"] = cipher_text.decode()

            statue = login_trival()
            if statue == "Logged in":
                # Save those three env vars into .env file
                with open('./downloads/.env', 'w') as env_file:
                    for key in ['CEDA_USERNAME','CEDA_KEY',"CEDA_PASSWORD"]:
                        env_file.write("{}={}\n".format(key, os.getenv(key)))
                logger.info('logging to CEDA successfully performed! .env have been saved at ./downloads folder')
                sg.Popup('logging to CEDA successfully performed! .env have been saved at ./downloads folder')
                break
            else:
                attempts +=1
                if attempts >= 5:
                    logger.error('logging to CEDA Failed too many times! Please make sure you have the valid username/password')
                    raise InvalidInput('logging to CEDA Failed too many times! Please make sure you have the valid username/password')
                else:
                    sg.PopupError(f'logging to CEDA Failed! you have {5 - attempts} chances left')
                    logger.error(f'logging to CEDA Failed! you have {5 - attempts} chances left')

        else:
            print('loggin Cancelled')
            logger.info('loggin Cancelled')
            break
record_login_info()