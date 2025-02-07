from setuptools import setup, find_packages

setup(
    name='Fenland Trajectory Agent',  # Change this to your project's name
    version='0.1.1',  # Increment version as needed
    author='Jiying Chen',  
    author_email='jc2341@cam.ac.uk',  
    packages=find_packages(),  # Automatically find all packages 
    install_requires=[
        'pyproj==3.4.0',
        'Flask==2.2.2',
        'flask-cors==3.0.10',
        'SPARQLWrapper==1.8.5',
        'pytz==2024.1',
        'configobj==5.0.6',
        'APScheduler==3.9.1',
        'numpy>=1.22',
        'tabulate==0.8.10',
        'uuid==1.30',
        'datetime==4.7',
        'openpyxl==3.0.10',
        'geomet==1.0.0',
        'geopandas==1.0.1',
        'shapely==2.0.0',
        'netCDF4==1.6.2',
        'matplotlib==3.6.2',
        'scipy==1.9.3',
        'seaborn==0.12.2',
        'beautifulsoup4==4.11.1',
        'urllib3==1.26.13',
        'PySimpleGUI',
        'cryptography==39.0.0',
        'python-dotenv==0.21.0',
        'docopt==0.6.2',
        'pandas==1.3.5',
        'JayDeBeApi==1.2.3',
        'twa',
        'requests==2.28.1',
        'gunicorn==20.1.0',
        'click>=8.0',
        'werkzeug==2.2.2',
        'psycopg2-binary==2.9.5'
    ],
    extras_require={
        'dev': [
            'pytest==7.2.0',
            'pytest-mock==3.10.0',
            'testcontainers==3.7.0'
        ]
    }
)
