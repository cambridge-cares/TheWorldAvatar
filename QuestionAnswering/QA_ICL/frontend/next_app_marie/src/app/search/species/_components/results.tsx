'use client'

import * as React from 'react'
import { createColumnHelper } from '@tanstack/react-table'

import { getSpeciesMany } from '@/lib/api/ontospecies'
import { SpeciesBase } from '@/lib/model/ontospecies'
import {
  LinkButtonToIRIPage,
  SearchResults,
} from '@/components/search/search-result'

const COL_HELPER = createColumnHelper<SpeciesBase>()
const COLS = [
  COL_HELPER.accessor('IRI', {
    header: '',
    cell: cell => (
      <LinkButtonToIRIPage IRI={cell.getValue()} prefixPath='/species' />
    ),
  }),
  COL_HELPER.accessor('label', { header: 'Label' }),
  COL_HELPER.accessor('IUPACName', { header: 'IUPAC name' }),
  COL_HELPER.accessor('InChI', { header: 'InChI' }),
]

export const SpeciesSearchResults = () => (
  <SearchResults dataGetter={getSpeciesMany} columns={COLS} />
)
