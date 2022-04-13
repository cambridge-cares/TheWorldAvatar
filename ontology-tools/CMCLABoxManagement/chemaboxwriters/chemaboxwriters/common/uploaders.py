from pyuploader.uploaders import TS_UPLOADER, FS_UPLOADER
import pyuploader.uploaders.uploader_factory as uploader_factory
from typing import Literal, List, Dict, Optional


Upload_History = Dict[str, Dict[str, str]]


class UploaderClient:
    def __init__(
        self,
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
        self._uploader = uploader_factory.get_uploader(
            uploader_type=self._uploader_type,
            url=self._url,
            auth_file=self._auth_file,
            no_auth=self._no_auth,
            subdirs=self._subdirs,
        )
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
                if location.get(file) is not None:
                    location_and_type = {
                        "location": location.get(file),
                        "input_type": input_type,
                    }
                    uploads_local[file] = location_and_type
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

    return _get_uploader_client(
        uploader_type="triple_store",
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

    return _get_uploader_client(
        uploader_type="file_server",
        upload_file_types=upload_file_types,
        url=url,
        auth_file=auth_file,
        no_auth=no_auth,
        subdirs=subdirs,
    )


def _get_uploader_client(
    uploader_type: Literal["triple_store", "file_server"],
    upload_file_types: List[str],
    url: str,
    auth_file: Optional[str] = None,
    no_auth: bool = False,
    subdirs: Optional[str] = None,
) -> UploaderClient:

    if uploader_type == "triple_store":
        _uploader_type = TS_UPLOADER
    else:
        _uploader_type = FS_UPLOADER

    return UploaderClient(
        uploader_type=_uploader_type,
        upload_file_types=upload_file_types,
        url=url,
        auth_file=auth_file,
        no_auth=no_auth,
        subdirs=subdirs,
    )
