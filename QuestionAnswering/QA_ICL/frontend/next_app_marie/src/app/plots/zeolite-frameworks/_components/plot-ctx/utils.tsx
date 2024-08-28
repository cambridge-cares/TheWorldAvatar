const reduceMin = (acc: number | undefined, cur: number) =>
  acc === undefined || cur < acc ? cur : acc
const reduceMax = (acc: number | undefined, cur: number) =>
  acc === undefined || cur > acc ? cur : acc
