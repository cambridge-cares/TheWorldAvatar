import { getChemicalClasses, getUses } from "@/lib/api"
import { SpeciesForm } from "./_components/species-form"

export default async function SpeciesPage() {
  const [chemicalClasses, uses] = await Promise.all([getChemicalClasses(), getUses()])

  return (
    <SpeciesForm allChemicalClasses={chemicalClasses} allUses={uses} />
  )
}
