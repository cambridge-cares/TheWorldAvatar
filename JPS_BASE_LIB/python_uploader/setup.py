from setuptools import setup, find_packages

setup(
    name='pyuploader',
    version='1.1.0',
    author='Daniel Nurkowski',
    author_email='danieln@cmclinnovations.com',
    license='MIT',
    python_requires='>=3.5',
    description="pyuploader is a simple command line tool for uploading data to a triple store and a file server.",
    url="https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/python_uploader",
    long_description=open('README.md').read(),
    long_description_content_type="text/markdown",
    packages=find_packages(exclude=('tests')),
    install_requires= ['py4jps>=1.0.6, <=1.0.14','docopt','requests'],
    include_package_data= True,
    entry_points={
        'console_scripts': [
            'pyuploader = pyuploader.driver:start',
        ]
    }
)