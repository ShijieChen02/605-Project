universe = vanilla
log = job_$(Cluster).log
error = job_$(Cluster)_$(Process).err
output = job_$(Cluster)_$(Process).out

executable = continent.sh

should_transfer_files = YES
when_to_transfer_output = ON_EXIT
transfer_input_files = http://proxy.chtc.wisc.edu/SQUID/chtc/el8/R413.tar.gz, packages.tar.gz, continent.R, glcp.csv

request_cpus = 5
request_memory = 5GB
request_disk = 10GB

queue 1