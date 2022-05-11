from setuptools import setup, find_packages

setup(
    name='pyderivationagent',
    version='0.0.1',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.5',
    description="pyderivationagent is a python wrapper for derivation agents as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/152-dev-pyderivationagent/JPS_BASE_LIB/python_derivation_agent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=('tests')),
    install_requires=['py4jps>=1.0.20', 'flask==1.1.2', 'gunicorn==20.0.4', 'Flask-APScheduler', 'rdflib',
    # 'agentlogging @ git+https://github.com/cambridge-cares/TheWorldAvatar@develop#subdirectory=Agents/utils/python-utils'
    ],
    extras_require={
        "dev": [
            "testcontainers>=3.4.2",
            "pytest>=6.2.3",
        ],
    },
    include_package_data= True
)
