from setuptools import setup, find_namespace_packages

setup(
    name='pyderivationagent',
    version='1.6.0',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.5',
    description="pyderivationagent is a python wrapper for derivation agents as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    # Werkzeug version is fixed as its 3.0.0 version breaks flask 2.x https://werkzeug.palletsprojects.com/en/3.0.x/changes/#version-3-0-0
    install_requires=['py4jps>=1.0.38', 'flask==2.1.0', 'gunicorn==20.0.4', 'Flask-APScheduler', 'rdflib', 'python-dotenv', 'yagmail', 'Werkzeug~=2.2.2'],
    extras_require={
        "dev": [
            "testcontainers>=3.4.2",
            "pytest>=6.2.3",
            "pytest-docker-compose>=3.2.1",
            "pytest-rerunfailures>=10.2"
        ],
    },
    include_package_data=True
)
