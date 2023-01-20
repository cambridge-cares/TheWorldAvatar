from setuptools import setup, find_packages

setup(name='myproject', version='1.0', 
      packages=find_packages(exclude=("tests")),
      install_requires= [
        'tqdm==4.64.1',
        'numpy==1.21.6',
        'tabulate==0.8.10',
        'uuid==1.30',
        'datetime==4.7',
        'openpyxl==3.0.10',
        'geomet==1.0.0',
        'geopandas==0.12.2',
        'shapely==2.0.0',
        'netCDF4==1.6.2',
        'matplotlib==3.6.2',
        'scipy==1.9.3',
        'seaborn==0.12.2',
        'datetime==4.7',
        'bs4==0.0.1',
        'beautifulsoup4==4.11.1',
        'urllib3==1.26.13',
        'PySimpleGUI ==4.60.4',
        'cryptography==39.0.0',
        'python-dotenv ==0.21.0',
        'APScheduler==3.9.1',
        'docopt==0.6.2',
        'Flask==2.2.2',
        'pandas==1.3.5',
        'JayDeBeApi==1.2.3',
        'py4jps==1.0.27', 
        'requests==2.28.1',
        'gunicorn==20.1.0',
    ],
    extras_require={
        "dev": [
            "pytest==7.2.0",
            "pytest-mock==3.10.0",
            "testcontainers==3.7.0",
            "psycopg2==2.9.5"
        ]
    }
      )