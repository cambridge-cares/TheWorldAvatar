import pkg_resources
import json
import os
import shutil
import keyword
import builtins
import textwrap

_RES_DIR = pkg_resources.resource_filename(__name__,os.path.join('..','resources'))
_RES_REG_FILE = 'resources_registry.json'
_DEF_RES_META_FILE = 'default_resources.json'
_URL_ID = 'url://'
_INIT_FILE = '__init__.py'

class resRegistry:
    """
    Registry class for managing jps resources.

    Attributes
    ----------
    resReg : dict
        resource registry dictionary

    """
    def __init__(self):
        """
        Constructs the registry object. If regsitry file does not exists, it creates one on the fly.
        """
        try:
            self.resReg = json.load(pkg_resources.resource_stream(__name__,os.path.join('..','resources',_RES_REG_FILE)))
        except FileNotFoundError:
            self.resReg = {'resources':{}}
            self._updateRegFile()
        # TODO default resources
        # - enable default resources info once we have a proper resource repo available
        #==============
        #try:
        #    self.resDefMeta = json.load(pkg_resources.resource_stream(__name__,os.path.join('..','resources',_DEF_RES_META_FILE)))
        #except FileNotFoundError:
        #    self.resDefMeta = {
        #    'resources':{
        #            "jpsBaseLib": {
        #                "mainFile": "jps-base-lib.jar",
        #                "getFrom": "file://D:/Projects/Chemistry_KG/Repositories/JParkSimulator-git/JPS_BASE_LIB/python_wrapper/jps-base-lib"
        #                    }
        #                }}
        #    self._updateResDefMetaFile()
        #==============
    def addResToReg(self, resName, resLoc, resMainJarFile=None):
        """
        Adds a resource to the registry.

        Arguments:
        ----------
        resName : str
            resource name
        resLoc : str
            path to the resource directory
        resMainJarFile : str, optional
            resource main jar file, if not provided the first jar found
            in the resource dir will be selected, if no jars are present
            it is set to None
        """
        if not self._isResInReg(resName):
            print("Info: Adding the {0} resource...".format(resName))
            self._checkResName(resName)

            # TODO default resources
            #======================
            #if self._isResInDefReg(resName):
            #    print("Info: {0} is a default resource. Retrieving the resource metadata...".format(resName))
            #    resMeta = self._getResDefMeta(resName)
            #    resLoc = resMeta['getFrom']
            #    resMainJarFile = resMeta['mainFile']
            ##======================
            self._addResRegEntry(resName)
            if resMainJarFile is None:
                resMainJarFile = self._checkForMainJar(resLoc)
            self._updateResRegEntry(resName, {'mainJarFile':resMainJarFile})
            self._addResFiles(resName, resLoc)
            self._addResMetaFile(resName)
            self._updateRegFile()
        else:
            print('Info: Resource already exist.')

    def removeResFromReg(self, resName):
        """
        Removes a resource from the registry.
        Note, it removes all the resource files as well.

        Arguments:
        ----------
        resName : str
            resource name
        """
        if self._isResInReg(resName):
            print("Info: Removing {0} resource...".format(resName))
            self._removeResRegEntry(resName)
            self._removeResFiles(resName)
            self._updateRegFile()
        else:
            print("Info: {0} resource is not on the registry!".format(resName))

    def listRes(self):
        """
        Lists all currently installed resources.
        """
        print('\n'.join(list(self.resReg['resources'].keys())))

    def cleanReg(self):
        """
        Cleans the registry.

        Use with caution. The command removes:
            - all registry entries with no corresponding resource files
            - all resource files with no corresponding registry entries
        """

        print("Info: Removing resources that do not exist on the registry...")
        for f in os.scandir(_RES_DIR):
            if f.is_dir():
                if not self._isResInReg(f.name):
                    print("Info: Found {} directory that is not on the registry. Removing...".format(f.name))
                    shutil.rmtree(f.path)
        print("Info: Removing resources complete.")
        print("Info: Removing registry entries that have no corresponding resource files...")
        if not self._isEmpty():
            for resName in self.resReg['resources']:
                if not os.path.exists(self._getResPath(resName)):
                    print("Info: Found {} registry entry with no resource files.".format(resName))
                    self._removeResRegEntry(resName)
        print("Info: Removing registry entries complete.")

    def getResMainFilePath(self, resName):
        """
        Returns an absolute path to the resource main jar file.

        Arguments:
        ----------
        resName : str
            resource name
        """
        if resName in self.resReg['resources']:
            mainFile = self.resReg['resources'][resName]['mainJarFile']
            return os.path.join(self._getResPath(resName),mainFile)
        else:
            return None

    @staticmethod
    def _checkResName(resName=None):
        if not resName:
            raise ValueError(textwrap.dedent("""
                Error: Resource name cannot be empty.
                       Please choose another name. Aborting the installation."""))
        elif keyword.iskeyword(resName):
            raise ValueError(textwrap.dedent("""
                Error: Resource name '{0}' is a Python's keyword.
                       Please choose another name. Aborting the installation.""".format(resName)))
        elif resName in dir(builtins):
            raise ValueError(textwrap.dedent("""
                Error: Resource name '{0}' is a Python's builtins name.
                       Please choose another name. Aborting the installation.""".format(resName)))
        elif not resName[0].isalpha():
            raise ValueError(textwrap.dedent("""
                Error: Resource name '{0}' does not start with a letter.
                       Please choose another name. Aborting the installation.""".format(resName)))
        elif not resName.isidentifier():
            # checks for the missed things above, such as, '-', '--', leading/trailing white spaces etc..
            raise ValueError(textwrap.dedent("""
                Error: Resource name '{0}' is not a valid Python identifier.
                       Please choose another name. Aborting the installation.""".format(resName)))

    def _isResInReg(self, resName):
        return resName in self.resReg['resources']

    def _getResPath(self, resName):
        return os.path.abspath(os.path.join(_RES_DIR,resName))

    def _getResName(self, resDict):
        return list(resDict.keys())[0]

    def _isEmpty(self):
        return len(self.resReg['resources'])==0

    def _isResInDefReg(self, resName):
        return resName in self.resDefMeta['resources']

    def _getResDefMeta(self, resName):
        if self._isResInDefReg(resName):
            return self.resDefMeta['resources'][resName]
        else:
            return {}

    def _getResRegMeta(self, resName):
        if self._isResInReg(resName):
            return self.resReg['resources'][resName]
        else:
            return {}

    def _addResRegEntry(self, resName):
        print("Info: Adding {0} resource to the registry.".format(resName))
        self.resReg['resources'].update({resName:{}})

    def _updateResRegEntry(self, resName, content):
        self.resReg['resources'][resName].update(content)

    def _removeResRegEntry(self, resName):
        print("Info: Removing {0} resource from the registry.".format(resName))
        del self.resReg['resources'][resName]

    def _addResFiles(self, resName, resLoc):
        print("Info: Fetching {0} resource files.".format(resName))
        if not _URL_ID in resLoc:
            try:
                shutil.copytree(resLoc, os.path.join(_RES_DIR,resName))
                self._addResClass(resName)
                self._addResInfoFile(resName)
            except FileExistsError as e:
                print("Warning: {0} resource files already exist! Cleaning...".format(resName))
                self._removeResFiles(resName)
                print("Info: Copying {0} resource files.".format(resName))
                shutil.copytree(resLoc, os.path.join(_RES_DIR,resName))
                self._addResClass(resName)
                self._addResInfoFile(resName)

    def _addResClass(self, resName):
        mainFile = self.getResMainFilePath(resName).replace('\\','\\\\')
        # create the class definition and init file
        _new_init_doc_str = r"__init__.__doc__ = JPSGateway.__init__.__doc__.replace('        resName : str\n            name of the Java resource'\
            +'\n        jarPath : str\n            absolute path to the main jar file of the java resource\n','')"

        _class_template = """
        from py4jps import JPSGateway
        class {0}(JPSGateway):
        ----def __init__(self,**JGkwargs):
        --------super({0}, self).__init__(resName='{0}',**JGkwargs)""".format(resName).replace('  ','').replace('-',' ')

        _class_template = _class_template + '\n    ' + _new_init_doc_str

        _ini_class_template="from py4jps.resources.{0}.{0} import {0}".format(resName)

        # write class definition and init
        print('Info: Installing {} files...'.format(resName))
        with open(os.path.join(self._getResPath(resName), resName+'.py'),'w') as classfile:
            classfile.write(_class_template)
        with open(os.path.join(self._getResPath(resName), _INIT_FILE),'w') as inifile:
            inifile.write(_ini_class_template)

        print('Info: Installing {} files complete.'.format(resName))

    def _removeResFiles(self, resName):
        if os.path.isdir(os.path.join(_RES_DIR,resName)):
            print("Info: Removing {0} resource files.".format(resName))
            shutil.rmtree(os.path.join(_RES_DIR,resName))
       # update resource module init file
        with open(os.path.abspath(os.path.join(_RES_DIR, _INIT_FILE)),'r') as inifile:
            lines = inifile.readlines()

    def _updateRegFile(self):
        print("Info: Saving the registry.")
        self._dumpJsonFile(os.path.join(_RES_DIR,_RES_REG_FILE),self.resReg)

    def _updateResDefMetaFile(self):
        print("Info: Saving the default resources metadata file.")
        self._dumpJsonFile(os.path.join(_RES_DIR,_DEF_RES_META_FILE),self.resDefMeta)

    def _addResInfoFile(self,resName):
        resMeta = {resName: self._getResRegMeta(resName)}
        self._dumpJsonFile(os.path.join(_RES_DIR,resName,'info.txt'),resMeta)

    def _addResMetaFile(self, resName):
        resMeta = self._getResRegMeta(resName)
        self._dumpJsonFile(os.path.join(self._getResPath(resName),'info.txt'),{resName:resMeta})

    def _checkForMainJar(self, resLoc):
        for f in os.listdir(resLoc):
            if os.path.isfile(os.path.join(resLoc, f)):
                if '.jar' in f:
                    return f
        raise Exception("Error: Main jar file could not be located. Aborting installation!")

    @staticmethod
    def _dumpJsonFile(jsonPath,contentDict):
        with open(jsonPath, "w") as outfile:
            json.dump(contentDict, outfile, indent=4)