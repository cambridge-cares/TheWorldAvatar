from setuptools import setup, find_namespace_packages

setup(
    name='rxnoptgoaliteragent',
    version='0.0.1',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.8',
    description="rxnoptgoaliteragent is capable of conducting one iteration of pursuring the reaction optimisation goal as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RxnOptGoalIterAgent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    install_requires=['pyderivationagent>=1.4.1', 'pandas', 'pydantic==1.9.0', 'chemistry_and_robots>=1.2.1'],
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
