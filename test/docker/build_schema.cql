CREATE KEYSPACE IF NOT EXISTS fmke
WITH REPLICATION = {
    'class': 'SimpleStrategy',
    'replication_factor': 1
};

CREATE TABLE IF NOT EXISTS fmke.patients (
    ID int PRIMARY KEY,
    Name text,
    Address text,
);

CREATE TABLE IF NOT EXISTS fmke.pharmacies (
    ID int PRIMARY KEY,
    Name text,
    Address text,
);

CREATE TABLE IF NOT EXISTS fmke.medical_staff (
    ID int PRIMARY KEY,
    Name text,
    Address text,
    Speciality text,
);

CREATE TABLE IF NOT EXISTS fmke.treatment_facilities (
    ID int PRIMARY KEY,
    Name text,
    Address text,
    Type text,
);

CREATE TABLE IF NOT EXISTS fmke.prescriptions (
    ID int,
    PatID int,
    DocID int,
    PharmID int,
    DatePrescribed timestamp,
    DateProcessed timestamp,
    PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS fmke.patient_prescriptions (
    PatientID int,
    PrescriptionID int,
    PRIMARY KEY (PatientID, PrescriptionID)
);

CREATE TABLE IF NOT EXISTS fmke.pharmacy_prescriptions (
    PharmacyID int,
    PrescriptionID int,
    PRIMARY KEY (PharmacyID, PrescriptionID)
);

CREATE TABLE IF NOT EXISTS fmke.staff_prescriptions (
    StaffID int,
    PrescriptionID int,
    PRIMARY KEY (StaffID, PrescriptionID)
);

CREATE TABLE IF NOT EXISTS fmke.prescription_drugs (
    PrescriptionID int,
    Drug text,
    PRIMARY KEY (PrescriptionID, Drug)
);

exit
