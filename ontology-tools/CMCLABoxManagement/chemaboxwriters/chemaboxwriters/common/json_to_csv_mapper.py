import json
import yaml
import re
import csv
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from typing import Dict, List, Optional, Literal

ENTRY_TYPES = {
    "<IMP>": "imports",
    "<INS>": "instance",
    "<OBJ>": "obj_prop",
    "<DAT>": "dat_prop",
}


class Abox_CSV_Builder:
    def __init__(self, file_path: str, configs: Optional[Dict] = None) -> None:
        self.file_path = file_path
        self._num_cols = 6
        if configs is None:
            configs = {}
        self.instance_field = configs.get("instance", "Instance")
        self.data_property_field = configs.get("data_property", "Data Property")
        self.object_property_field = configs.get("object_property", "Instance")
        self.ontology_field = configs.get("ontology", "Ontology")
        self.default_prefix = configs.get("prefix", "")
        self._current_inst = None
        self._prefixes = {}
        self._current_name = None
        self._write_history = []

    def register_prefix(self, name: str, value: str) -> None:
        self._prefixes[name] = value

    def write_header(self) -> "Abox_CSV_Builder":
        self._write_row(
            "Source",
            "Type",
            "Target",
            "Relation",
            "Value",
            "Data Type",
        )
        return self

    def write_imports(
        self,
        name: str,
        importing: str,
        rel: Optional[str] = None,
        store_name: bool = True,
    ) -> "Abox_CSV_Builder":
        if rel is None:
            rel = "http://www.w3.org/2002/07/owl#imports"

        name, rel, importing = self._apply_prefixes(name, rel, importing)
        self._write_row(name, self.ontology_field, importing, rel)

        if store_name:
            self._current_name = name
        return self

    def write_inst(
        self, iri: str, type: str, rel: str = "", store_inst: bool = True
    ) -> "Abox_CSV_Builder":

        iri, rel, type = self._apply_prefixes(iri, rel, type)

        self._write_row(iri, self.instance_field, type, rel)
        if store_inst:
            self._current_inst = iri
        return self

    def write_data_prop(
        self,
        iri: str,
        rel: str,
        value: str,
        data_type: Literal["String", "Integer", "Float"] = "String",
        store_inst: bool = True,
    ) -> "Abox_CSV_Builder":

        iri, rel, value = self._apply_prefixes(iri, rel, value)
        self._write_row(rel, self.data_property_field, iri, "", value, str(data_type))

        if store_inst:
            self._current_inst = iri
        return self

    def write_obj_prop(
        self,
        src_iri: str,
        rel: str,
        trg_iri: str,
        store_inst: Literal["src", "trg", ""] = "",
    ) -> "Abox_CSV_Builder":

        src_iri, rel, trg_iri = self._apply_prefixes(src_iri, rel, trg_iri)

        self._write_row(src_iri, self.object_property_field, trg_iri, rel)
        if store_inst == "src":
            self._current_inst = src_iri
        elif store_inst == "trg":
            self._current_inst = trg_iri
        return self

    def add_obj_prop(
        self, rel: str, iri: str, store_inst: bool = False, reverse: bool = False
    ) -> "Abox_CSV_Builder":
        if self._current_inst is None:
            raise app_exceptions.MissingInstance(
                "No instance created. Cannont add object property."
            )

        src_iri, trg_iri = iri, self._current_inst
        if reverse:
            src_iri, trg_iri = self._current_inst, iri

        store_inst_lit = "src" if store_inst else ""

        self.write_obj_prop(
            src_iri=src_iri, rel=rel, trg_iri=trg_iri, store_inst=store_inst_lit
        )
        return self

    def add_data_prop(
        self,
        rel: str,
        value: str,
        data_type: Literal["String", "Integer", "Float"] = "String",
    ) -> "Abox_CSV_Builder":

        if self._current_inst is None:
            raise app_exceptions.MissingInstance(
                "No instance created. Can not add data property."
            )
        self.write_data_prop(
            iri=self._current_inst, rel=rel, value=value, data_type=data_type
        )
        return self

    def add_imports(
        self, importing: str, rel: Optional[str] = None
    ) -> "Abox_CSV_Builder":

        if self._current_name is None:
            raise app_exceptions.MissingOntologyName(
                "No ontology name defined. Can not add import statement."
            )

        self.write_imports(name=self._current_name, importing=importing, rel=rel)
        return self

    def _write_row(self, *args: str) -> None:
        content = [args[i] if i < len(args) else "" for i in range(self._num_cols)]
        # if content not in self._write_history:
        self.csvwriter.writerow(content)
        #    self._write_history.append(content)

    def _apply_prefixes(self, *args: str) -> List[str]:
        items = []
        for item in args:
            prefix_name = item.split(":")[0]
            prefix_value = self._prefixes.get(prefix_name)
            if prefix_value is not None:
                item = item.replace(f"{prefix_name}:", prefix_value)
            items.append(item)
        return items

    def __enter__(self):
        self._file_obj = open(self.file_path, "w", newline="").__enter__()
        self.csvwriter = csv.writer(
            self._file_obj, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
        )
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback) -> bool:
        self._file_obj.__exit__(exc_type, exc_value, exc_traceback)
        return True


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
        # if entry_item not in write_entry:
        write_entry.append(entry_item)

    def _find_loop_range(self, schema_variables: Dict) -> None:
        for var_key, var_value in schema_variables.items():
            if var_key in self.variables and isinstance(var_value, list):
                self.loop_range = range(len(var_value))
                return

    def _write_entry(self, writer: Abox_CSV_Builder) -> None:
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
            data_type = dat_prop_splitted[3] if len(dat_prop_splitted) > 3 else "String"
            writer.write_data_prop(
                iri=iri, rel=rel, value=value, data_type=data_type  # type: ignore
            )

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
        with Abox_CSV_Builder(file_path=out_file) as writer:
            writer.write_header()
            prefixes = self._schema.get("prefixes", {})
            for prefix_key, prefix_value in prefixes.items():
                writer.register_prefix(name=prefix_key, value=prefix_value)

            for entry in self._schema_entries:
                if entry.write_entry:
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
            if self._all_entry_vars_present(entry=entry):
                entry.process(self._schema_variables)
                entry.write_entry = True

    def _all_entry_vars_present(self, entry: Schema_Entry) -> bool:
        for var_name in entry.variables:
            if self._schema_variables.get(var_name) is None:
                return False
        return True
