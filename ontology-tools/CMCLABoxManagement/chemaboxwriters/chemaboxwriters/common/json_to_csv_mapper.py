import json
import yaml
import re
import csv
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from typing import Dict, List, Optional, Literal

__doc__ = """
This module contains classes that are used to abstract abox csv file
creation using the json data file and the schema file.
"""

ENTRY_TYPES = {
    "<IMP>": "imports",
    "<INS>": "instance",
    "<OBJ>": "obj_prop",
    "<DAT>": "dat_prop",
}


class Abox_CSV_Builder:
    """
    This class implements an abox csv builder that simplifies the abox csv
    file creation. Any write_ methods require all the iris to be passed and then
    store the main instance iri for the later use in the add_ methods. The builder
    can be then used as follows:

    with Abox_CSV_Builder(file_path=out_file) as writer:
        writer.register_prefix(name=prefix_key, value=prefix_value) # register prefixes
        writer.write_header() # writes the main abox csv header line

        # imports chaining example
        writer.write_imports( # writes the imports line and stores the abox name for
            name=abox_name,   # the next add_imports call
            importing=ontology1_to_import,
            rel=import_relation,
            store_name=True
        ).add_imports(       # this is how to chain the import statements
            importing=ontology2_to_import, # the add_imports call re-uses stored
            rel=import_relation,           # abox_name, import relation is optional
        )
        # instance obj chaining exmaple
        writer.write_inst(
            iri=instance_iri,
            type=instance_type,
            store_inst=True
        ).add_obj_prop(             # adds the following triple:
            target_iri=target_iri,  # instance_iri rel target_iri
            rel=obj_rel,            # if reverse True, then the triple is:
            reverse=False,          # target_iri rel instance_iri
            store_target_inst=False # if store_target_inst is True
        )                           # then the stored instance will be
                                    # the target instance from now on
        # note that reverse and store_target_inst are False by default so there is
        # no need to pass them, unless one wishes to change them to True
        # similarly one could attach add_data_prop to the above chain which would
        # also reuse the stored instances_iri
    """

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
        """Writes the abox csv file header."""
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
        """Writes an import statement. If rel is missing it uses a default one."""

        if rel is None:
            rel = "http://www.w3.org/2002/07/owl#imports"

        name, rel, importing = self._apply_prefixes(name, rel, importing)
        self._write_row(name, self.ontology_field, importing, rel)

        if store_name:
            self._current_name = name
        return self

    def write_inst(
        self, iri: str, type: str, store_inst: bool = True
    ) -> "Abox_CSV_Builder":
        """Writes an instance statement. It stores the instances iri by default
        so that any add_ method call can automatically reuse it.
        """

        iri, type = self._apply_prefixes(iri, type)

        self._write_row(iri, self.instance_field, type)
        if store_inst:
            self._current_inst = iri
        return self

    def write_data_prop(
        self,
        iri: str,
        rel: str,
        value: str,
        data_type: Literal["String", "Integer", "Float", "Literal"] = "String",
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
        self,
        target_iri: str,
        rel: str,
        store_target_inst: bool = False,
        reverse: bool = False,
    ) -> "Abox_CSV_Builder":
        if self._current_inst is None:
            raise app_exceptions.MissingInstance(
                "No instance created. Cannont add object property."
            )

        src_iri, trg_iri = self._current_inst, target_iri
        if reverse:
            src_iri, trg_iri = target_iri, self._current_inst

        store_inst_lit = "trg" if store_target_inst else ""

        self.write_obj_prop(
            src_iri=src_iri, rel=rel, trg_iri=trg_iri, store_inst=store_inst_lit
        )
        return self

    def add_data_prop(
        self,
        rel: str,
        value: str,
        data_type: Literal["String", "Integer", "Float", "Literal"] = "String",
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
        if content not in self._write_history:
            self.csvwriter.writerow(content)
            self._write_history.append(content)

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
    """Encapsulates a single schema entry and provides methods processing it."""

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
        """Process the entry, checking if it contains any list
        variables and if so, determines the loop range.
        """
        self._find_loop_range(schema_variables)
        if self.loop_range is None:
            self._process_entries(schema_variables)
        else:
            for i in self.loop_range:
                self._process_entries(schema_variables, loop_iter=i)

    def _process_entries(
        self, schema_variables: Dict, loop_iter: Optional[int] = None
    ) -> None:
        """This processes the entry, replacing all the variables with their actual
        values. In case of loop entries, it expands them.
        """
        entries_to_process = [
            self.imports,
            self.instances,
            self.obj_prop,
            self.data_prop,
        ]
        entries_to_accumulate = [
            self.imports_write,
            self.instances_write,
            self.obj_prop_write,
            self.data_prop_write,
        ]

        # spliting schema entry line first and then replacing all vars in it is deliberate
        # I know that one could replace all vars first and then split, however, that could
        # cause problems if e.g. some variables contain special characters such as double
        # quotes or commas, as then it would be tricky to split such strings after the
        # replacement has been done.
        for entry_list, entry_write in zip(entries_to_process, entries_to_accumulate):
            for entry in entry_list:
                entry_splitted = self._split_line(entry)
                entry_piece_processed = []
                for entry_piece in entry_splitted:
                    entry_piece_processed.append(
                        self._replace_variables(
                            entry_item=entry_piece,
                            schema_variables=schema_variables,
                            loop_iter=loop_iter,
                        )
                    )
                entry_write.append(entry_piece_processed)

    def _replace_variables(
        self,
        entry_item: str,
        schema_variables: Dict,
        loop_iter: Optional[int],
    ) -> str:
        """Replaces variables with their values"""
        for var_key, var_value in schema_variables.items():
            if var_key not in self.variables:
                continue
            if loop_iter is not None:
                if isinstance(var_value, list):
                    var_value = var_value[loop_iter]
                # replaces a special %{i} loop variable with the current loop
                # index, starting from 1
                entry_item = re.sub(r"\%\{(i)\}", str(loop_iter + 1), entry_item)
            entry_item = re.sub(r"\$\{(" + var_key + r")\}", str(var_value), entry_item)
        return entry_item

    def _find_loop_range(self, schema_variables: Dict) -> None:
        loop_lengths = []
        loop_vars = []
        for var_key, var_value in schema_variables.items():
            if var_key in self.variables and isinstance(var_value, list):
                loop_lengths.append(len(var_value))
                loop_vars.append(var_key)
        if loop_lengths:
            if len(set(loop_lengths)) > 1:
                var_lengths = {
                    var: length for var, length in zip(loop_vars, loop_lengths)
                }
                raise app_exceptions.MismatchedSchemaListVariablesLength(
                    (
                        f"Schema subsection contains list variables with different "
                        f"lengths: {var_lengths}."
                    )
                )

            self.loop_range = range(loop_lengths[0])

    def _write_entry(self, writer: Abox_CSV_Builder) -> None:
        for imp in self.imports_write:
            name, rel, importing = imp
            writer.write_imports(name=name, importing=importing, rel=rel)

        for inst in self.instances_write:
            iri, _type = inst
            writer.write_inst(iri=iri, type=_type)

        for obj_prop in self.obj_prop_write:
            src_iri, rel, trg_iri = obj_prop
            writer.write_obj_prop(src_iri=src_iri, rel=rel, trg_iri=trg_iri)

        for dat_prop in self.data_prop_write:
            iri, rel, value = dat_prop[:3]
            data_type = "String"
            if len(dat_prop) > 3:
                data_type = dat_prop[3]
            writer.write_data_prop(
                iri=iri, rel=rel, value=value, data_type=data_type  # type: ignore
            )

    @staticmethod
    def _split_line(line: str) -> List[str]:
        splitted_line = [
            p for p in re.split("( |\\\".*?\\\"|'.*?')", line) if p.strip()
        ]
        return splitted_line


class JSON_TO_CSV_CONVERTER:
    """Main class that perofmrs json to csv conversion based on the schem file"""

    def __init__(self, schema_yml_file: str) -> None:
        with open(schema_yml_file, "r") as stream:
            self._schema = yaml.safe_load(stream)
        self._schema_variables = {}
        self._schema_entries: List[Schema_Entry] = []
        self._schem_file = schema_yml_file
        self._json_data_file = None

    def json_to_csv(self, json_data_file: str, out_file: str) -> None:
        with open(json_data_file, "r") as fjson:
            abox_data = json.load(fjson)

        self._abox_data = abox_data
        self._json_data_file = json_data_file
        self._set_schema_variables()
        self._set_schema_entries()
        self._process_schema_entries()
        self._write_csv(out_file=out_file)

    def _write_csv(self, out_file: str) -> None:
        """Use Abox_CSV_Builder to write the abox csv file."""

        with Abox_CSV_Builder(file_path=out_file) as writer:
            writer.write_header()
            prefixes = self._schema.get("prefixes", {})
            for prefix_key, prefix_value in prefixes.items():
                writer.register_prefix(name=prefix_key, value=prefix_value)

            # write each schema entry one by one
            for entry in self._schema_entries:
                if entry.write_entry:
                    entry._write_entry(writer=writer)

    def _set_schema_variables(self) -> None:
        """Sets dictionary storing all schema variable names and their values."""
        schema_variables = self._schema.get("schema_to_json_vars")
        self._schema_variables = {}
        for var, var_info in schema_variables.items():
            var_required = var_info.get("required", False)
            var_name = var_info.get("jsonKey")
            if var_name is None:
                raise app_exceptions.MissingRequiredInput(
                    (
                        f"Error: Missing jsonKey for {var} variable "
                        f"in the {self._schem_file} file."
                    )
                )
            var_value = self._abox_data.get(var_name)

            if var_required and var_value is None:
                raise app_exceptions.MissingRequiredInput(
                    (
                        f"Error: Undefined required variable {var} "
                        f"in the {self._json_data_file} file."
                    )
                )
            self._schema_variables[var] = var_value

    def _set_schema_entries(self) -> None:
        """Converts schema entries into Schem_Entry objects."""
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
        """Process all schema entries. By the end each entry will have all
        the variables replaces. In case of loop entries, lines will be
        expanded according to the loop range. This does not yet write the
        csv file.
        """
        for entry in self._schema_entries:
            if self._all_entry_vars_present(entry=entry):
                entry.process(self._schema_variables)
                entry.write_entry = True

    def _all_entry_vars_present(self, entry: Schema_Entry) -> bool:
        for var_name in entry.variables:
            if self._schema_variables.get(var_name) is None:
                return False
        return True
