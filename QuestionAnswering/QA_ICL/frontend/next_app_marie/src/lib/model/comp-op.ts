export const OComparisonOperator = {
  EQ: 'eq',
  LT: 'lt',
  GT: 'gt',
  LTE: 'lte',
  GTE: 'gte',
}
export type ComparisonOperator =
  (typeof OComparisonOperator)[keyof typeof OComparisonOperator]
