# Create a folder basing on currentTime
import os
import shutil
import datetime
from os import listdir
from shutil import copyfile
from os.path import isfile, join


timeStamp = datetime.datetime.now().strftime('%D-%H:%M:%S').replace('/','_').replace(':','_')


backupDir = 'C:/BMSDataBackup' + timeStamp + '/'
templateDir = '../DES'


os.mkdir(backupDir)
 
# Move all the files in SRC to Backup folder

templates = [f for f in listdir(templateDir) if isfile(join(templateDir, f))]
files = [f for f in listdir('./') if isfile(join('./', f))]

for file in files:
	if('.owl' in file):
		print('filename: ',file)
		shutil.move('./' + file, backupDir + file)
	 
 
	 
	 
# Replace all the owl files in SRC with files in DES
print('here we are')
for template in templates:
	print('template',template)
	if('.owl' in template):
		print('filename',template)
		shutil.copyfile('../DES/' + template,'./' + template)

