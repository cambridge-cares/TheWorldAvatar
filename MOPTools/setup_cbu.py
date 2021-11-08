from setuptools import setup, find_packages

setup(
    name='cbucsvtojson',
    version='1.0.0',
    author='Aleksandr Kondinski, Daniel Nurkowski, Angiras Menon',
    author_email='',
    license='MIT',
    python_requires='>=3.7',
    packages=find_packages(include=['CBU_to_os_JSON', 'CBU_to_os_JSON.*']),
    install_requires= ["docopt", "py4jps", "pubchempy", "entityrdfizer"],
    include_package_data=True,
    entry_points={
        'console_scripts': [
			'cbutojson=CBU_to_os_JSON.main:start'
        ],
    }
)
