from setuptools import setup, find_packages

setup(
    name='pyasyncagent',
    version='0.0.2',
    author='Jiaru Bai',
    author_email='jb2197@cam.ac.uk',
    license='MIT',
    python_requires='>=3.5',
    description="pyasyncagent is a python wrapper for asynchronous agents as part of The World Avatar project.",
    # py_modules=['AsyncAgent'], #
    # package_dir={'': 'pyasyncagent'}, #
    packages=find_packages(exclude=('tests')),
    # url="https://github.com/cambridge-cares/TheWorldAvatar/tree/142-dev-python-async-agent/JPS_BASE_LIB/python_async_agent",
    # long_description=open('README.md').read(),
    # long_description_content_type="text/markdown",
    # packages=find_packages(exclude=('tests')),
    # install_requires= ['py4jps>=1.0.6, <=1.0.14'],
    # include_package_data= True,
    # entry_points={}
)
