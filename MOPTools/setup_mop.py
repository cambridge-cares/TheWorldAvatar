from setuptools import setup, find_packages

setup(
    name='mopcsvtojson',
    version='1.0.0',
    author='Aleksandr Kondinski, Daniel Nurkowski, Angiras Menon',
    author_email='',
    license='MIT',
    python_requires='>=3.7',
    packages=find_packages(include=['MOP_to_omJSON', 'MOP_to_omJSON.*']),
    install_requires= ["docopt", "py4jps", "pubchempy", "entityrdfizer"],
    include_package_data=True,
    entry_points={
        'console_scripts': [
			'moptojson=MOP_to_omJSON.main:start'
        ],
    }
)
