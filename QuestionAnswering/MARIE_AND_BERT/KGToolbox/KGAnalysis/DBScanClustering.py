import pickle
from pyspark.ml.clustering import DBScan
from pyspark.sql.functions import rand
from pyspark.sql import SparkSession

# load the precomputed distance matrix from the pickle file
with open("matrix.pkl", "rb") as file:
    distance_matrix = pickle.load(file)

#set up SparkSession
spark = SparkSession.builder.appName("DBScanExample").getOrCreate()

# convert the distance matrix into a PySpark dataframe
df = spark.createDataFrame(distance_matrix)

for i in range(10):
    # randomly sample a subset of the data
    sampled_df = df.sample(False, 0.1)

    # create an instance of the DBScan model
    dbscan = DBScan(eps=2, minPoints=5000, predictionCol="cluster")

    # fit the model to the distance matrix
    model = dbscan.fit(sampled_df)

    # obtain the cluster predictions
    result = model.transform(sampled_df)

    # show the results
    result.show()
    result.write.parquet("dbscan_result.parquet")