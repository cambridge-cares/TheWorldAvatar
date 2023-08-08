#
# from pyspark.sql import SparkSession
# spark = SparkSession.builder \
#     .master("local[1]") \
#     .appName("SparkByExamples.com") \
#     .getOrCreate()
#
# df=spark.range(100)
# print(df.sample(0.06).collect())

l1 = [1,2,3,4]
l2 = [2,3,4,5]

o = set(l1) ^ set(l2)
print(o)
print(len(o))