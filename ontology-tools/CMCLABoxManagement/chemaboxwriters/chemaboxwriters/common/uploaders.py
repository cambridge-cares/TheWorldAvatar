from pyuploader.uploaders import TS_UPLOADER, FS_UPLOADER
import pyuploader.uploaders.uploader_factory as uploader_factory
from pyuploader.uploaders.uploader import Uploader
from typing import List, Dict, Optional

__doc__ = """
This modules contains the code that abstracts all the handlers upload calls
"""

Upload_History = Dict[str, Dict[str, str]]


class UploaderClient:
    """Client that is added to each handler that wraps around file server
    or triple store uploader.
    """

    def __init__(
        self,
        uploader: Uploader,
        uploader_type: str,
        upload_file_types: List[str],
        url: str,
        auth_file: Optional[str] = None,
        no_auth: bool = False,
        subdirs: Optional[str] = None,
    ) -> None:
        self._uploader_type = uploader_type
        self._upload_file_types = upload_file_types
        self._url = url
        self._auth_file = auth_file
        self._no_auth = no_auth
        self._subdirs = subdirs
        self._uploader = uploader
        self._uploads = {}

    def info(self) -> None:
        print("--------------------------------------------------")
        print(f"{self._uploader_type} upload client")
        print(f"url: {self._url}")
        print(f"auth_file: {self._auth_file}")
        print(f"no_auth: {self._no_auth}")
        if self._subdirs is not None:
            print(f"subdirs: {self._subdirs}")
        print(f"upload_file_types: {self._upload_file_types}")

    def get_upload_configs(self) -> Dict:
        return {
            "url": self._url,
            "auth_file": self._auth_file,
            "no_auth": self._no_auth,
            "subdirs": self._subdirs,
            "upload_file_types": self._upload_file_types,
        }

    def do_uploads(
        self,
        inputs: List[str],
        input_type: str,
        dry_run: bool,
    ) -> Upload_History:

        uploads_local = {}
        if input_type in self._upload_file_types:
            for file in inputs:
                # skip already uploaded files
                if file in self._uploads:
                    continue
                file_ext = "owl" if "owl" in input_type else input_type
                location = self._uploader.upload(
                    file_ext=file_ext, file_or_dir=file, dry_run=dry_run
                )
                # additionally add input_type info to the upload location
                # output as it may be useful in the future, e.g. some handlers
                # may need info on upload files location of a particular type
                if location.get(file) is not None:
                    location_and_type = {
                        "location": location.get(file),
                        "input_type": input_type,
                    }
                    # this adds the upload location info to the local dict that is then
                    # returned. It becomes useful if one wishes to use the upload client
                    # locally
                    uploads_local[file] = location_and_type
                    # this adds the upload location info to the _uploads dict,
                    # this dict can be set via the init_uploads_history method.
                    # In a pipeline, all handlers write to the same _uploads dict
                    # by calling the init_uploads_history method and passing the
                    # same dict to it for all the handlers.
                    self._uploads[file] = location_and_type
        return uploads_local

    def init_uploads_history(self, uploads_history: Upload_History) -> None:
        self._uploads = uploads_history

    def get_upload_location(
        self, upload_file: str, upload_stage: Optional[str] = None
    ) -> Optional[str]:

        upload_location = self._uploads.get(upload_file)
        if upload_location is None:
            return
        if upload_stage is not None:
            if upload_stage not in upload_location:
                return
        return upload_location["location"]


def get_triple_store_uploader(
    upload_file_types: List[str],
    url: str,
    auth_file: Optional[str] = None,
    no_auth: bool = False,
) -> UploaderClient:

    uploader = uploader_factory.get_uploader(
        uploader_type=TS_UPLOADER,
        url=url,
        auth_file=auth_file,
        no_auth=no_auth,
    )

    return _get_uploader_client(
        uploader=uploader,
        uploader_type=TS_UPLOADER,
        upload_file_types=upload_file_types,
        url=url,
        auth_file=auth_file,
        no_auth=no_auth,
    )


def get_file_server_uploader(
    upload_file_types: List[str],
    url: str,
    auth_file: Optional[str] = None,
    no_auth: bool = False,
    subdirs: Optional[str] = None,
) -> UploaderClient:

    uploader = uploader_factory.get_uploader(
        uploader_type=FS_UPLOADER,
        url=url,
        auth_file=auth_file,
        no_auth=no_auth,
        subdirs=subdirs,
    )

    return _get_uploader_client(
        uploader=uploader,
        uploader_type=FS_UPLOADER,
        upload_file_types=upload_file_types,
        url=url,
        auth_file=auth_file,
        no_auth=no_auth,
        subdirs=subdirs,
    )


def _get_uploader_client(
    uploader: Uploader,
    uploader_type: str,
    upload_file_types: List[str],
    url: str,
    auth_file: Optional[str] = None,
    no_auth: bool = False,
    subdirs: Optional[str] = None,
) -> UploaderClient:

    return UploaderClient(
        uploader=uploader,
        uploader_type=uploader_type,
        upload_file_types=upload_file_types,
        url=url,
        auth_file=auth_file,
        no_auth=no_auth,
        subdirs=subdirs,
    )
