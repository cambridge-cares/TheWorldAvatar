from setuptools import setup, find_namespace_packages

setup(
    name='chemistry_and_robots',
    version='1.6.2',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.8',
    description="chemistryandrobots contains dataclasses and sparql queries for concepts related to chemistry_and_robots as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/utils/chemistry_and_robots",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    install_requires=['pyderivationagent>=1.4.1', 'pydantic==1.9.0', 'pandas', 'xlrd', 'xlwt'],
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
