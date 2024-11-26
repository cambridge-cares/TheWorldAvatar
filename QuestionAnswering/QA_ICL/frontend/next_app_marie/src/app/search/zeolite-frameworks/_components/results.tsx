'use client'

import * as React from 'react'
import { createColumnHelper } from '@tanstack/react-table'

import { ZeoliteFrameworkBase } from '@/lib/model/ontozeolite'
import { getZeoliteFrameworksMany } from '@/lib/api/ontozeolite'
import {
  LinkButtonToIRIPage,
  SearchResults,
} from '@/components/search/search-result'

const COL_HELPER = createColumnHelper<ZeoliteFrameworkBase>()
const COLS = [
  COL_HELPER.accessor('IRI', {
    header: '',
    cell: cell => (
      <LinkButtonToIRIPage
        IRI={cell.getValue()}
        prefixPath='/zeolite-frameworks'
      />
    ),
  }),
  COL_HELPER.accessor('code', { header: 'Code' }),
]

export const ZeoFrameworkResults = () => (
  <SearchResults dataGetter={getZeoliteFrameworksMany} columns={COLS} />
)
