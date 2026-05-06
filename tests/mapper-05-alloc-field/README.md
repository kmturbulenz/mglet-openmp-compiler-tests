### mapper-05-alloc-field

Tests if a derived type containing two allocatable member variables can be mapped with a custom mapper, where one member variable is allocated and the other member variable is mapped to the device. 

Use case: When creating temporary fields in MGLET, we want to allocate the large data without copying data to the device, while always copying the field metadata to the device if initialized.
