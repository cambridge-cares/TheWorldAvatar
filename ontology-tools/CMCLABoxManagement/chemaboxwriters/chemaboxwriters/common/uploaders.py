from pyuploader.uploaders import TS_UPLOADER, FS_UPLOADER
from pyuploader.uploaders.uploader_factory import get_uploader
import os
from typing import Literal, List, Dict, Optional


triple_store_uploader: str = TS_UPLOADER
fs_upl: str = FS_UPLOADER


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
        self._uploader = get_uploader(
            uploader_type=self._uploader_type,
            url=self._url,
            auth_file=self._auth_file,
            no_auth=self._no_auth,
            subdirs=self._subdirs,
        )

    def info(self) -> None:
        print("--------------------------------------------------")
        print(f"{self._uploader_type} upload client")
        print(f"url: {self._url}")
        print(f"auth_file: {self._auth_file}")
        print(f"no_auth: {self._no_auth}")
        if self._subdirs is not None:
            print(f"subdirs: {self._subdirs}")
        print(f"upload_file_types: {self._upload_file_types}")

    def get_upload_configs(self)->Dict:
        return {
            'url': self._url,
            'auth_file': self._auth_file,
            'no_auth': self._no_auth,
            'subdirs': self._subdirs,
            'upload_file_types': self._upload_file_types
        }

    def do_uploads(
        self,
        inputs: List[str],
        input_type: str,
        dry_run: bool,
        uploads: Optional[Dict] = None,
    ) -> None:

        uploads_local = {}
        if input_type in self._upload_file_types:
            for file in inputs:
                file_ext = "owl" if "owl" in input_type else input_type
                location = self._uploader.upload(
                    file_ext=file_ext, file_or_dir=file, dry_run=dry_run
                )
                uploads_local.update(location)
        if uploads is not None:
            for source, location in uploads_local.items():
                uploads[source] = {'location': location, 'input_type': input_type}


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
