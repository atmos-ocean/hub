# qsub

  

```
-a date_time
```

​		  Declares the time after which	the job	is eligible
​		  for execution.

		  The date_time	argument is in the form:
		  [[[[CC]YY]MM]DD]hhmm[.SS]

2020623
if the day (DD) is not specified, it will default to today if the time (hhmm) is in the future. Otherwise, the day will be set to **tomorrow**.

For example, if you submit a job at 11:15 am with a time of `-a 1110`, the job will be eligible to run at **11:10 am tomorrow**.
