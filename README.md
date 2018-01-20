# FMKe [![Build Status](https://travis-ci.org/goncalotomas/FMKe.svg?branch=master)](https://travis-ci.org/goncalotomas/FMKe) [![Coverage Status](https://coveralls.io/repos/github/goncalotomas/FMKe/badge.svg?branch=master)](https://coveralls.io/github/goncalotomas/FMKe?branch=master)

FMKe is an extendable real world benchmark for distributed key-value stores.  
This repository contains code for the application server and a set of scripts for orchestrating deployment and local execution of micro-benchmarks.

## Why?
Here is a comparison of available benchmark specifications that we analyzed, with FMKe for comparison:

| Benchmark        | Target Systems           | Workload type  |
| ------------- |:-------------:| -----:|
| [TPC-C][6]      | SQL-Based databases ❌ | **realistic ✔️ |
| [TPC-E][7]      | SQL-Based databases ❌      |   **realistic ✔️ |
| [YCSB][5] | Key-value stores ✔️     |    synthetic ❌ |
| FMKe | Key-value stores ✔️     | **realistic ✔️ |

** Emulates real application patterns

## Backing the realistic claims

FMKe was one of the final contributions of the [SyncFree][3] European research project. It was designed to benchmark its reference platform, [AntidoteDB][2], by closely emulating a realistic application. One of the industrial partners of the project, Trifork, provided statistical data about _Fælles Medicinkort_ (FMK), a sub-system relative to the Danish National Joint Medicine Card. The real system is backed by a distributed key value store to ensure high availability, which validates the decision to use it as a benchmark (originally) for AntidoteDB.

## System description
The real world FMK system, and FMKe alike are designed to store patient health data, mostly revolving around medical prescriptions. Here is the ER diagram:  

![Build Status](http://i.imgur.com/q6ByEFs.png)  

There are 4 core entities: **treatment facilities**, **patients**, and **pharmacies**. Other records appear as relations between these entities, but it will become apparent that the workload focuses heavily on prescription records. More information about the system operations and data model can be found in [this document][8].

## Architecture
![Build Status](http://i.imgur.com/rLZSFMb.png)  
Consider FMKe as a general application server that contains the logic mimicking the real FMK system. We decided not to release FMKe as a single monolithic application, since there are multiple benefits in separating it in these 3 components.  
Firstly, separating the application server from the workload generation component doesn't require us to reinvent the wheel, since many good workload generation tools already exist. On the other hand, making the application logic independent of the database allows for collaboration in supporting a broader set of data stores.  
We have a generic interface for key-value stores (implemented as an Erlang behaviour) that is well specified, which makes supporting a new database as simple as writing a driver for it. Furthermore, pull requests with new drivers or optimizations for existing ones are accepted and welcomed.

## Supported data stores
- AntidoteDB (using nested CRDTs)
- AntidoteDB (with a normalized data model)
- Riak (using nested CRDTs)
- Redis (with a normalized data model)
### In a future release
- Lasp


## How the benchmark is deployed
By default FMKe keeps a connection pool to a single database node, and the workload generation is performed by [Lasp Bench][4].  
To benchmark clustered databases with _n_ nodes, _n_ FMKe instances can be deployed, or alternatively one FMKe node can connect to multiple nodes (the exact number is dependent on the connection pool size).  
To avoid network and CPU bottlenecks that could impact the result of the benchmark, it is advised to use different servers for each one of the components. Having said that, a number of scripts are available for development that enable local execution of micro benchmarks.

### Use case: AntidoteDB evaluation
FMKe was used in January 2017 to evaluate the performance of AntidoteDB. The evaluation took place in Amazon Web Services using `m3.xlarge` instances which have 4 vCPUs, 15GB RAM and 2x40GB SSD storage.  
The biggest test case used 36 AntidoteDB instances spread across 3 data centers (Germany, Ireland and United States), 9 instances of FMKe and 18 instances of (former Basho Bench) Lasp Bench that simulated 1024 concurrent clients performing operations as quickly as possible.  
Before the benchmark, AntidoteDB was populated with over 1 million patient keys, 50 hospitals, 10.000 doctors and 300 pharmacies.

## Testing out FMKe locally
FMKe requires [Erlang/OTP][9] and [rebar3][10]. You need at least Erlang 20, FMKe will not compile in previous versions.  
You can test out FMKe locally by cloning the repository:

```bash
git clone https://github.com/goncalotomas/FMKe.git
```

Once you have a local copy of the repository, the first step is to choose your target data store:

```bash
make select-TARGET_DB
```

Where TARGET_DB should be one of the supported databases. From now on let's assume that we chose `riak`.  
You don't need to have any databases installed, since local benchmarks use Docker images.  
Finally, you can run a micro-benchmark by using the following command:

```bash
make bench-riak
```

Alternatively, you can also validate that your FMKe copy is functional by running unit tests with your desired database as backend:

```bash
make eunit-riak
```

This command will run a battery of unit tests that ensure that all functionality related to the benchmark is able to performed in the database you have previously selected.

[1]: https://syncfree.lip6.fr/
[2]: https://antidotedb.eu
[3]: https://github.com/SyncFree
[4]: https://github.com/lasp-lang/lasp-bench
[5]: https://b9f6702a-a-62cb3a1a-s-sites.googlegroups.com/site/brianfrankcooper/home/publications/ycsb.pdf?attachauth=ANoY7cplFQg1yGsPe1xDRwV2JKPCI7OffNZnUyNOVBMecaBZIlPPuWBV0oB4T5RJEIPJLn3OwUP_Tlawws8YIeHYdTLEf3E1lcJGYqzFIxIVEXxHujMqxEyioMP_w4dRMlxUPpjx6nlwOW6R9Di9f30VKXnEX5a6qwJgAaUhSEN_zbTAuzZs_VONffsO7jSa8Hr-24O1kkMwPFWot8ouhbmJSHwSE0F44V_AYEV7sAsvbWp9iWD9Kp0%3D&attredirects=0
[6]: http://www.tpc.org/tpcc/default.asp
[7]: http://www.tpc.org/tpce/
[8]: https://github.com/goncalotomas/FMKe/blob/master/doc/FMK_DataModel.pdf
[9]: http://www.erlang.org/downloads
[10]: http://www.rebar3.org/
