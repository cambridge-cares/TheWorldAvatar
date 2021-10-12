"""This script expects an instance of the FileServer to be running locally on port 58090.
   To start the server on that port, run 'docker-compose up' in the directory above this one.

   N.B. These examples are intended to illustrate use of the server via one particular
   (Python-based) client and shouldn't be treated as unit tests.
"""

import filecmp
import os.path
import requests
from requests import status_codes


# Port must match the one specified in docker-compose.yml
server_URL = 'http://localhost:58090/FileServer/'
upload_URL = server_URL+'upload'
download_URL = server_URL+'download/'

# Default credentials for authentication. If you supply a different password via a secret, modify 'fs_pass' on the next line accordingly
auth=('fs_user', 'fs_pass')

# Paths to some dummy data files
base_dir   = os.path.join(os.path.dirname(__file__),"data")
owl_fpath  = os.path.join(base_dir,'dummy.owl')
log_fpath  = os.path.join(base_dir,'dummy.log')
text_fpath = os.path.join(base_dir,'dummy.txt')

# Remote sub-directory used for some uploads
remote_subdir = 'my_namespace'

# Path used to download a file and compare it to the original that was uploaded
downloaded_txt_fpath  = os.path.join(base_dir,'downloaded_dummy.txt')


# Test multi-file upload
with open(owl_fpath,'rb') as file_obj1, open(log_fpath,'rb') as file_obj2, open(text_fpath,'rb') as file_obj3:
    # Set the subdirectory in which to store files (optional header)
    headers= {'subDir': remote_subdir}
    print("Uploading multiple files to subdir [%s]" % headers['subDir'])

    # Aggregrate file objects in a dict
    # Key names can be anything - but whatever is set will be re-used in the response to return actual filenames
    files = {'file1': file_obj1,'file2': file_obj2,'file3': file_obj3}

    # Post request
    response = requests.post(upload_URL, auth=auth, headers=headers, files=files)
    # Extract actual filenames from the response
    if (response.status_code == status_codes.codes.OK):
        for key in files.keys():
            print(" %s uploaded with filename [%s]" % (key,response.headers[key]))
    else:
        print("  ERROR: File upload failed with code %d " % response.status_code)


# Test single file upload
text_remote_upload_path=None
with open(text_fpath,'rb') as file_obj:
    print("\nUploading single file without a subdir")
    files={'file':file_obj}
    response = requests.post(upload_URL, auth=auth, files=files)
    if (response.status_code == status_codes.codes.OK):
        text_remote_upload_path = response.headers['file']
        print(" Uploaded with filename [%s]" % text_remote_upload_path)
    else:
        print("  ERROR: File upload failed with code %d " % response.status_code)


# Test single file upload with nested sub-dir
with open(text_fpath,'rb') as file_obj:
    nested_subdir="%s/group1" % remote_subdir
    print("\nUploading single file to nested subdir (%s)" % nested_subdir)
    response = requests.post(upload_URL, auth=auth, headers= {'subDir': nested_subdir}, files={'file':file_obj})
    if (response.status_code == status_codes.codes.OK):
        for key in files.keys():
            print(" %s uploaded with filename [%s]" % (key,response.headers[key]))
    else:
        print("  ERROR: File upload failed with code %d " % response.status_code)


# Test single file download
if text_remote_upload_path is not None:
    print("\nRe-downloading file")
    response = requests.get(download_URL+text_remote_upload_path, auth=auth)
    if (response.status_code == status_codes.codes.OK):
        with open(downloaded_txt_fpath, 'wb') as file_obj:
            for chunk in response.iter_content(chunk_size=128):
                file_obj.write(chunk)
        if filecmp.cmp(text_fpath,downloaded_txt_fpath):
            print("  Downloaded file matches original")
        else:
            print("  ERROR: Downloaded file differs from original")
    else:
        print("  ERROR: File download failed with code %d " % response.status_code)


# Try GET from upload URL (should fail)
print("\nAttempting GET from upload URL")
response = requests.get(upload_URL, auth=auth)
if (response.status_code != status_codes.codes.bad_request):
    print("  ERROR: GET from upload URL: expected status code %d, but got %d" % (status_codes.codes.bad_request,response.status_code) )
else:
    print("  Rejected, as expected")


# Try POST to download URL (should fail)
print("\nAttempting POST to download URL")
fname="my_namespace/dummy.owl"
response = requests.post(download_URL+fname, auth=auth)
if (response.status_code != status_codes.codes.bad_request):
    print("  ERROR: POST to download URL: expected status code %d, but got %d" % (status_codes.codes.bad_request,response.status_code) )
else:
    print("  Rejected, as expected")