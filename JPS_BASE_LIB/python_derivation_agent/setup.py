from setuptools import setup, find_packages

setup(
    name='pyderivationagent',
    version='1.1.0',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.5',
    description="pyderivationagent is a python wrapper for derivation agents as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=['tests','tests.*']),
    install_requires=['py4jps>=1.0.22', 'flask==2.1.0', 'gunicorn==20.0.4', 'Flask-APScheduler', 'rdflib', 'python-dotenv'
    # 'agentlogging @ git+https://github.com/cambridge-cares/TheWorldAvatar@develop#subdirectory=Agents/utils/python-utils'
    ],
    extras_require={
        "dev": [
            "testcontainers>=3.4.2",
            "pytest>=6.2.3",
            "pytest-docker-compose>=3.2.1",
            "pytest-rerunfailures>=10.2"
        ],
    },
    include_package_data= True
)
