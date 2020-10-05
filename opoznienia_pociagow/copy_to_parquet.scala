import org.apache.spark.sql.types._

val df = spark.read.json("dane/json/*")

val df2 = df.select($"train_id", $"train_name", explode($"schedules").as("schedules"))

val df3 = df2.select($"train_id", $"train_name", $"schedules.schedule_id", $"schedules.schedule_date", explode($"schedules.info").as("info"))

val df4 = df3.select($"train_id".cast(IntegerType), $"train_name", $"schedule_id".cast(IntegerType), $"schedule_date".cast(TimestampType), $"info.arrival_delay".cast(IntegerType), $"info.arrival_time".cast(TimestampType), $"info.departure_delay".cast(IntegerType), $"info.departure_time".cast(TimestampType), $"info.station_name")

df4.write.parquet("trains_parquet")

