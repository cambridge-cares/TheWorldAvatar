from setuptools import setup, find_packages

setup(
    name='chemaboxwriters',
    version='1.0.0',
    author='Daniel Nurkowski',
    author_email='',
    license='MIT',
    python_requires='>=3.7',
    packages=find_packages(exclude=('tests')),
    long_description=open('README.md').read(),
    install_requires= ["docopt"],
    include_package_data=True
)