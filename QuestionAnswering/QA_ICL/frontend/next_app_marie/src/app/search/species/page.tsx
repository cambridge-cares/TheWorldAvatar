import { getChemicalClasses, getUses } from '@/lib/api/ontospecies'
import { SpeciesForm } from './_components/species-form'
import { useSearchParams } from 'next/navigation'

export default async function SpeciesPage() {
  const [chemicalClasses, uses] = await Promise.all([
    getChemicalClasses(),
    getUses(),
  ])

  const searchParams = useSearchParams()

  return <SpeciesForm allChemicalClasses={chemicalClasses} allUses={uses} />
}
