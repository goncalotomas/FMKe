# FMKe HTTP API

FMKe has a total of 7 entities: `patient`, `facility`, `pharmacy`, `staff`, `event`, `prescription` and `treatment`.  
Once an FMK node is running, an HTTP server will be started on the port defined for the environment variable `HTTP_PORT`.  
**Important notes:**  

1. The order of the parameters listed in each request matters since pattern matching imposes order on the list of body parameters that is passed in.
2. The casing (upper/lower case) for each of the parameters here listed matters.

The HTTP endpoints that will be available are:

## Patients ```/patients[/:id]```

### GET
A patient with id `7` can be fetched by sending a GET request to `/patients/7`.
### POST
It's possible to create a patient by sending a POST request to `/patients` with the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| id            | integer       |
| name          | string        |
| address       | string        |
### PUT
You can update an existing patient by sending a PUT request to `/patients/:patient_id`, where `:patient_id` is an ID of an already existent patient. You need to supply the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| name          | string        |
| address       | string        |

## Facilities ```/facilities[/:id]```
### GET
A facility with id `7` can be fetched by sending a GET request to `/facilities/7`.
### POST
It's possible to create a facility by sending a POST request to `/facilities` with the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| id            | integer       |
| name          | string        |
| address       | string        |
| type          | string        |
### PUT
You can update an existing facility by sending a PUT request to `/facilities/:facility_id`, where `:facility_id` is an ID of an already existent facility. You need to supply the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| name          | string        |
| address       | string        |
| type          | string        |

## Pharmacies ```/pharmacies[/:id]```
### GET
A pharmacy with id `7` can be fetched by sending a GET request to `/pharmacies/7`.
### POST
It's possible to create a pharmacy by sending a POST request to `/pharmacies` with the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| id            | integer       |
| name          | string        |
| address       | string        |
### PUT
You can update an existing pharmacy by sending a PUT request to `/pharmacies/:pharmacy_id`, where `:pharmacy_id` is an ID of an already existent pharmacy. You need to supply the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| name          | string        |
| address       | string        |

## Staff ```/staff[/:id]```
### GET
A staff member with id `7` can be fetched by sending a GET request to `/staff/7`.
### POST
It's possible to create a staff member by sending a POST request to `/staff` with the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| id            | integer       |
| name          | string        |
| address       | string        |
| speciality    | string        |

### PUT
You can update an existing staff member by sending a PUT request to `/staff/:staff_id`, where `:staff_id` is an ID of an already existent staff member. You need to supply the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| name          | string        |
| address       | string        |
| speciality    | string        |

## Events ```/events[/:id]```
### GET
An event with id `7` can be fetched by sending a GET request to `/events/7`.
### POST
It's possible to create an event by sending a POST request to `/events` with the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| id            | integer       |
| treatment_id  | integer       |
| staff_id      | integer       |
| timestamp     | string        |
| description   | string        |

### PUT
Events cannot currently be updated.

## Prescriptions ```/prescriptions[/:id]```
### GET
A prescription with id `7` can be fetched by sending a GET request to `/prescriptions/7`.
### POST
It's possible to create a prescription by sending a POST request to `/prescriptions` with the following parameters:

| Parameter       | Type          |
|  -------------  |:-------------:|
|  id             | integer       |
|  patient_id     | integer       |
|  prescriber_id  | integer       |
|  pharmacy_id    | integer       |
|  facility_id    | integer       |
| date_prescribed | string        |
|   drugs         | string        |

**Important note:** `drugs` is a String made up of comma separated values (CSV) for each drug in the prescription.

### PUT
You can update an existing staff member by sending a PUT request to `/prescriptions/:prescription_id`, where `:prescription_id` is an ID of an already existent prescription. You need to supply the following parameters:

| Parameter     | Type          |
| ------------- |:-------------:|
| date_processed| string        |
| drugs         | string        |

**Important note:** `drugs` is a String made up of comma separated values (CSV) for each drug in the prescription.  
Despite both parameters having to be present in order for it to be considered a valid PUT request, the system will behave in the following way:

- If `date_processed` is set to a value, it will assume that you're trying to process the prescription.
- If `date_processed` is empty and `drugs` is not, it will add all CSV to as new drugs of the prescription.

## Treatments ```/treatments[/:id]```
### GET
A treatment with id `7` can be fetched by sending a GET request to `/treatments/7`.
### POST
It's possible to create a treatment by sending a POST request to `/treatments` with the following parameters:

| Parameter       | Type          |
|  -------------  |:-------------:|
|  id             | integer       |
|  patient_id     | integer       |
|  prescriber_id  | integer       |
|  facility_id    | integer       |
| date_prescribed | string        |

### PUT
Treatments cannot currently be updated.
