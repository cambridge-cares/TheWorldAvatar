'use client'

import * as React from 'react'
import { createColumnHelper } from '@tanstack/react-table'

import { getZeoliticMaterialsMany } from '@/lib/api/ontozeolite'
import { ZeoliticMaterialBase } from '@/lib/model/ontozeolite'
import {
  LinkButtonToIRIPage,
  SearchResults,
} from '@/components/search/search-result'

const COL_HELPER = createColumnHelper<ZeoliticMaterialBase>()
const COLS = [
  COL_HELPER.accessor('IRI', {
    header: '',
    cell: cell => (
      <LinkButtonToIRIPage
        IRI={cell.getValue()}
        prefixPath='/zeolitic-materials'
      />
    ),
  }),
  COL_HELPER.accessor('ChemicalFormula', {
    header: 'Formula',
  }),
]

export const ZeoliticMaterialResults = () => (
  <SearchResults dataGetter={getZeoliticMaterialsMany} columns={COLS} />
)
