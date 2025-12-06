from rdkit import Chem
from rdkit.Chem.inchi import MolFromInchi, MolToInchiKey
from urllib.parse import quote
import requests
import json

UNICHEM_POST = "https://www.ebi.ac.uk/unichem/api/v1/inchikey"
UNICHEM_GET  = "https://www.ebi.ac.uk/unichem/rest/inchikey/{ikey}"
OLS_BASE = "https://www.ebi.ac.uk/ols4/api/ontologies/chebi"
RO_HAS_ROLE = "http://purl.obolibrary.org/obo/RO_0000087"


# ---------- Utility helpers ----------

def _safe_json(response):
    """Return response.json() or {} if it's invalid or a string."""
    try:
        data = response.json()
        if isinstance(data, (dict, list)):
            return data
        print(f"⚠️ Unexpected JSON type: {type(data)} -> returning empty dict")
        return {}
    except Exception:
        print(f"⚠️ Could not parse JSON from {response.url} (status {response.status_code})")
        return {}


def _inchikey_from_inchi(inchi: str) -> str:
    mol = MolFromInchi(inchi)
    if mol is None:
        raise ValueError(f"RDKit could not parse InChI: {inchi}")
    return MolToInchiKey(mol)


def _chebi_ids_from_inchikey(ikey: str, timeout=20):
    """Return list of ChEBI IDs (src_id = 7) from UniChem."""
    try:
        r = requests.post(UNICHEM_POST, json={"inchikey": ikey}, timeout=timeout)
        r.raise_for_status()
        rows = _safe_json(r)
    except Exception as e:
        print(f"⚠️ POST failed for UniChem, trying GET ({e})")
        r = requests.get(UNICHEM_GET.format(ikey=quote(ikey)), timeout=timeout)
        r.raise_for_status()
        rows = _safe_json(r)

    if not isinstance(rows, list):
        print(f"⚠️ Expected list from UniChem, got {type(rows)}")
        return []

    chebi_ids = []
    for row in rows:
        if isinstance(row, dict) and str(row.get("src_id")) == "7":
            chebi_ids.append(row.get("src_compound_id"))
    return sorted(set(chebi_ids))


def _chebi_curie(chebi_id: str) -> str:
    return chebi_id if chebi_id.startswith("CHEBI:") else f"CHEBI:{chebi_id}"


def _chebi_iri(chebi_id: str) -> str:
    num = chebi_id.split(":")[-1]
    return f"http://purl.obolibrary.org/obo/CHEBI_{num}"


def _ols_term_parents(iri: str, timeout=20):
    """Return parent terms (is_a) via OLS4."""
    url = f"{OLS_BASE}/terms/{quote(iri, safe='')}/parents"
    try:
        r = requests.get(url, timeout=timeout)
        r.raise_for_status()
        data = _safe_json(r)
        if not isinstance(data, dict):
            return []
        embedded = data.get("_embedded", {})
        terms = embedded.get("terms", [])
        return terms if isinstance(terms, list) else []
    except Exception as e:
        print(f"⚠️ OLS parent query failed for {iri}: {e}")
        return []


def _ols_term_relations(iri: str, related_iri: str, timeout=20):
    """Return related terms (e.g., has role) via OLS4."""
    url = f"{OLS_BASE}/terms/{quote(iri, safe='')}/relations"
    try:
        r = requests.get(url, params={"property": related_iri}, timeout=timeout)
        if r.status_code == 404:
            return []
        r.raise_for_status()
        data = _safe_json(r)
        if not isinstance(data, dict):
            return []
        embedded = data.get("_embedded", {})
        terms = embedded.get("terms", [])
        return terms if isinstance(terms, list) else []
    except Exception as e:
        print(f"⚠️ OLS relation query failed for {iri}: {e}")
        return []


# ---------- Main function ----------

def chebi_request(inchi: str) -> dict:
    """
    Returns a dict like:
      {0: {'key': 'ChebiId', 'type': 'identifier', 'value': '18276', ...},
       1: {'key': 'ChemicalClass', 'type': 'classification', 'value': '...', 'chebiID': 'CHEBI:...'},
       2: {'key': 'Use', 'type': 'use', 'value': '...', ...}}
    """
    chebi_prop = {}
    reference_url = "https://www.ebi.ac.uk/chebi/"

    try:
        ikey = _inchikey_from_inchi(inchi)
    except Exception as e:
        print(f"❌ Failed to convert InChI to InChIKey: {e}")
        return {}

    ids = _chebi_ids_from_inchikey(ikey)
    if not ids:
        print(f"⚠️ No ChEBI IDs found for InChIKey: {ikey}")
        return {}

    primary_id = ids[0]
    curie = _chebi_curie(primary_id)
    iri = _chebi_iri(primary_id)

    i = 0
    chebi_prop[i] = {
        "key": "ChebiId",
        "type": "identifier",
        "value": primary_id,
        "description": "",
        "reference": reference_url,
    }
    i += 1

    # ---- Parents ----
    parents = _ols_term_parents(iri)
    if isinstance(parents, list):
        for p in parents:
            if not isinstance(p, dict):
                continue
            label = p.get("label") or p.get("short_form") or ""
            short_form = p.get("short_form") or ""
            chebi_parent_id = short_form.replace("CHEBI_", "CHEBI:") if short_form.startswith("CHEBI_") else ""
            chebi_prop[i] = {
                "key": "ChemicalClass",
                "type": "classification",
                "value": str(label),
                "description": "ChEBI Classification",
                "chebiID": chebi_parent_id,
                "reference": reference_url,
            }
            i += 1

    # ---- Roles ----
    roles = _ols_term_relations(iri, RO_HAS_ROLE)
    if isinstance(roles, list):
        for r in roles:
            if not isinstance(r, dict):
                continue
            label = r.get("label") or ""
            chebi_prop[i] = {
                "key": "Use",
                "type": "use",
                "value": str(label),
                "description": "ChEBI Role",
                "reference": reference_url,
            }
            i += 1

    return chebi_prop
