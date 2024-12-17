Collaboration is at the heart of The World Avatar project. For details, please visit the [contributing guide](https://github.com/cambridge-cares/TheWorldAvatar/wiki/How-to-Contribute).

## Package release

> **NOTE: Before making the package release, please remove all sub-folders and `resources_registry.json` file in `python_wrapper/twa/resources` folder to prevent incorrect packing of Java resources. For more information, please refer to this [issue](https://github.com/cambridge-cares/TheWorldAvatar/issues/800).**

Maintainers who package and publish the most up-to-date codes from the feature branches handle the distribution of package `twa` on PyPI and Test-PyPI. If you want to release the package independently, i.e. become a maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your Test-PyPI and PyPI account and password
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- The feature branch you are working on is ready to be merged back to the main branch (a Pull Request should already be created and reviewed by a senior developer)

Please merge the newest main branch to your feature branch once these details are ready. The release process can then be started by using the commands below, depending on the operating system you're using.
> **REMEMBER TO CHANGE THE CORRECT VALUES IN THE COMMANDS BELOW!**

`(Windows)`

```cmd
cd \absolute_path_to\TheWorldAvatar\JPS_BASE_LIB\python_wrapper
release_twa_to_pypi.sh -v x.x.x
```

`(Linux)`
```sh
cd /absolute_path_to/TheWorldAvatar/JPS_BASE_LIB/python_wrapper
./release_twa_to_pypi.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, two changes should be done automatically during the release process, specifically in python script `JPS_BASE_LIB/python_wrapper/twa/__init__.py`
```
__version__ = "x.x.x"
```

and `JPS_BASE_LIB/python_wrapper/setup.py`
```
version='x.x.x',
```

where `x.x.x` matches the version number used in your release.

These two changes should be committed to the feature branch.

Finally, merge the Pull Request of the feature branch back into the `main` branch.
