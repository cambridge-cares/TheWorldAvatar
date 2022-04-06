import json
import yaml
import re
import csv
import chemaboxwriters.common.utilsfunc as utilsfunc
from typing import Dict, List, Optional

ENTRY_TYPES = {
    "<IMP>": "imports",
    "<INS>": "instance",
    "<OBJ>": "obj_prop",
    "<DAT>": "dat_prop",
}


class Schema_Entry:
    def __init__(self):
        self.imports = []
        self.instances = []
        self.obj_prop = []
        self.data_prop = []
        self.variables = []
        self.loop_range = None
        self.write_entry = False
        self.imports_write = []
        self.instances_write = []
        self.obj_prop_write = []
        self.data_prop_write = []

    def add_entry(
        self,
        entry_type: str,
        entry_str: str,
        entry_vars: Optional[List] = None,
    ) -> None:

        if entry_type == "imports":
            if entry_str not in self.imports:
                self.imports.append(entry_str)
        elif entry_type == "instance":
            if entry_str not in self.instances:
                self.instances.append(entry_str)
        elif entry_type == "obj_prop":
            if entry_str not in self.obj_prop:
                self.obj_prop.append(entry_str)
        elif entry_type == "dat_prop":
            if entry_str not in self.data_prop:
                self.data_prop.append(entry_str)
        else:
            raise KeyError

        if entry_vars is not None:
            for var in entry_vars:
                if var not in self.variables:
                    self.variables.append(var)

    def process(self, schema_variables: Dict) -> None:
        self._find_loop_range(schema_variables)
        if self.loop_range is None:
            self._process_entries(schema_variables)
        else:
            for i in self.loop_range:
                self._process_entries(schema_variables, loop_iter=i)

    def _process_entries(
        self, schema_variables: Dict, loop_iter: Optional[int] = None
    ) -> None:
        for imp in self.imports:
            self._replace_variables(
                write_entry=self.imports_write,
                entry_item=imp,
                schema_variables=schema_variables,
                loop_iter=loop_iter,
            )

        for inst in self.instances:
            self._replace_variables(
                write_entry=self.instances_write,
                entry_item=inst,
                schema_variables=schema_variables,
                loop_iter=loop_iter,
            )

        for obj_prop in self.obj_prop:
            self._replace_variables(
                write_entry=self.obj_prop_write,
                entry_item=obj_prop,
                schema_variables=schema_variables,
                loop_iter=loop_iter,
            )

        for data_prop in self.data_prop:
            self._replace_variables(
                write_entry=self.data_prop_write,
                entry_item=data_prop,
                schema_variables=schema_variables,
                loop_iter=loop_iter,
            )

    def _replace_variables(
        self,
        write_entry: List[str],
        entry_item: str,
        schema_variables: Dict,
        loop_iter: Optional[int],
    ) -> None:
        for var_key, var_value in schema_variables.items():
            if var_key not in self.variables:
                continue
            if loop_iter is not None:
                if isinstance(var_value, list):
                    var_value = var_value[loop_iter]
                entry_item = re.sub(r"\%\{(i)\}", str(loop_iter + 1), entry_item)
            entry_item = re.sub(r"\$\{(" + var_key + r")\}", str(var_value), entry_item)
        if entry_item not in write_entry:
            write_entry.append(entry_item)

    def _find_loop_range(self, schema_variables: Dict) -> None:
        for var_key, var_value in schema_variables.items():
            if var_key in self.variables and isinstance(var_value, list):
                self.loop_range = range(len(var_value))
                return

    def _write_entry(self, writer: utilsfunc.Abox_csv_writer) -> None:
        for imp in self.imports_write:
            name, rel, importing = imp.split()
            writer.write_imports(name=name, importing=importing, rel=rel)

        for inst in self.instances_write:
            inst_splitted = self._split_line(inst)
            iri, rel, _type = inst_splitted[0], inst_splitted[1], inst_splitted[2]

            if rel == "rdf:type":
                rel = ""
            writer.write_inst(iri=iri, type=_type, rel=rel)

        for obj_prop in self.obj_prop_write:
            obj_prop_splitted = self._split_line(obj_prop)
            src_iri, rel, trg_iri = (
                obj_prop_splitted[0],
                obj_prop_splitted[1],
                obj_prop_splitted[2],
            )
            writer.write_obj_prop(src_iri=src_iri, rel=rel, trg_iri=trg_iri)

        for dat_prop in self.data_prop_write:
            dat_prop_splitted = self._split_line(dat_prop)
            iri, rel, value = (
                dat_prop_splitted[0],
                dat_prop_splitted[1],
                dat_prop_splitted[2],
            )
            writer.write_data_prop(iri=iri, rel=rel, value=value)

    @staticmethod
    def _split_line(line: str, delimiter: str = " ") -> List[str]:
        splitted_line = []
        for row in csv.reader([line], delimiter=delimiter):
            splitted_line = row
        return splitted_line


class JSON_TO_CSV_CONVERTER:
    def __init__(self, schema_yml_file: str) -> None:
        with open(schema_yml_file, "r") as stream:
            schema_dict = yaml.safe_load(stream)
        self._schema = schema_dict
        self._schema_variables = {}
        self._schema_entries: List[Schema_Entry] = []

    def json_to_csv(self, json_data_file: str, out_file: str) -> None:
        with open(json_data_file, "r") as fjson:
            abox_data = json.load(fjson)

        self._abox_data = abox_data
        self._set_schema_variables()
        self._set_schema_entries()
        self._process_schema_entries()
        self._write_csv(out_file=out_file)

    def _write_csv(self, out_file: str) -> None:
        with utilsfunc.Abox_csv_writer(file_path=out_file) as writer:
            writer.write_header()
            prefixes = self._schema.get("prefixes", {})
            for prefix_key, prefix_value in prefixes.items():
                writer.register_prefix(name=prefix_key, value=prefix_value)

            for entry in self._schema_entries:
                entry._write_entry(writer=writer)

    def _set_schema_variables(self) -> None:
        schema_variables = self._schema.get("schema_to_json_vars")
        self._schema_variables = {}
        for var, var_info in schema_variables.items():
            self._schema_variables[var] = self._abox_data.get(var_info["jsonKey"])

    def _set_schema_entries(self) -> None:
        abox_schema = self._schema.get("abox_schema")
        self._schema_entries = []
        for abox_entries in abox_schema:
            schema_entry = Schema_Entry()
            for entry in abox_entries:
                entry_type_str = entry.split()[0]
                entry_type = ENTRY_TYPES.get(entry_type_str)
                if entry_type is None:
                    continue

                entry_str = entry.split(entry_type_str)[1].strip()
                variables = self._find_variables(entry_str)
                schema_entry.add_entry(
                    entry_type=entry_type, entry_str=entry_str, entry_vars=variables
                )
            self._schema_entries.append(schema_entry)

    def _find_variables(self, str) -> List:
        entry_variables = []
        for var in self._schema_variables.keys():
            match = re.findall(r"\$\{(" + var + r")\}", str)
            if match:
                entry_variables.extend(match)
        return list(set(entry_variables))

    def _process_schema_entries(self) -> None:
        for entry in self._schema_entries:
            entry.process(self._schema_variables)