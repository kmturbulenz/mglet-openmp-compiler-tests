### 06-module-variable

Tests if a data environment can be created and used to copy data from derived types between the host and device.

Use case: MGLET uses a dictionary-like data structure to store fields by name. All fields must be accessible in kernel functions on the device efficiently. Thus, we want to have a data environment for each field, and especially the buffer content of each field. One must also be able to update data from and to the device in between calling different kernels for features that still run on the CPU, disregarding any performance concerns at this point.
