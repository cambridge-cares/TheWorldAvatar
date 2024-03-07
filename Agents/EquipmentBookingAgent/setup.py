from setuptools import setup, find_namespace_packages

setup(
    name='equipmentbookingagent',
    version='0.0.1',
    author='Simon Rihm',
    author_email='sdr39@cam.ac.uk',
    license='MIT',
    python_requires='>=3.8',
    description="equipmentbookingagent is capable of instantiating and managing bookings of specific equipment by users as part of The World Avatar project.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/EquipmentBookingAgent",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_namespace_packages(exclude=['tests','tests.*']),
    install_requires=['pyderivationagent>=1.4.3', 'pandas==1.3.5', 'pydantic==1.9.0'],
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
