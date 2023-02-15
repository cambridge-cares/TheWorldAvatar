################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# This module will record your CEDA username and password (as Encrypted text) and saved 
# in the ./downloads/.env path, which can be retrieved as environmental variable. 
# Note that this module will try to login in to CEDA using the username and password you provided, 
# only if the login success, .env will be saved. If the login failed for more than 5 times which will raise 
# an InvalidInputError and stop the module.

import os
from cryptography.fernet import Fernet
import requests
from bs4 import BeautifulSoup 
from dotenv import load_dotenv

import agentlogging
from agent.errorhandling.exceptions import InvalidInput

# Initialise logger
logger = agentlogging.get_logger("prod")

def read_file(file_path):
    with open(file_path, 'r') as file:
        return file.read().strip()

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
        logger.error(f'Response {response.status_code} CEDA platform fail to connect! -- Try to login later')
    if "auth_tkt" in session.cookies:
        statue = "Logged in"
    else:
        statue = "Not logged in"

    return statue

def record_login_info():
    if not os.path.exists('./downloads'):
        os.makedirs('./downloads')
    
    username = read_file('./secrets/CEDA_username')
    password = bytes(read_file('./secrets/CEDA_password'), "utf-8")
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
        print('logging to CEDA successfully performed! .env have been saved at ./downloads folder')
        logger.info('logging to CEDA successfully performed! .env have been saved at ./downloads folder')
        
    else:
        logger.error('logging to CEDA Failed too many times! Please make sure you have the valid username/password')
        raise InvalidInput('logging to CEDA Failed too many times! Please make sure you have the valid username/password')

def retrieve_settings():
    global CEDA_USERNAME, CEDA_PASSWORD
    
    load_dotenv('./downloads/.env')
    key = os.getenv('CEDA_KEY').encode()
    cipher = Fernet(key)
    # Retrieve the CEDA_USERNAME
    CEDA_USERNAME = os.getenv('CEDA_USERNAME')    
    if CEDA_USERNAME is None:
        logger.error('"CEDA_USERNAME" is missing in environment variables.')
        raise ValueError('"CEDA_USERNAME" is missing in environment variables.')
    if CEDA_USERNAME == '':
        logger.error('No "CEDA_USERNAME" value has been provided in environment variables.')
        raise ValueError('No "CEDA_USERNAME" value has been provided in environment variables.')

    # Retrieve the CEDA_PASSWORD
    encrypted_password = os.getenv('CEDA_PASSWORD')
    CEDA_PASSWORD = str(cipher.decrypt(encrypted_password), 'utf-8')
    if CEDA_PASSWORD is None:
        logger.error('"CEDA_PASSWORD" is missing in environment variables.')
        raise ValueError('"CEDA_PASSWORD" is missing in environment variables.')
    if CEDA_PASSWORD == '':
        logger.error('No "CEDA_PASSWORD" value has been provided in environment variables.')
        raise ValueError('No "CEDA_PASSWORD" value has been provided in environment variables.')

    return CEDA_USERNAME, CEDA_PASSWORD
