# job.sub

universe = vanilla
log = job_$(Process).log
error = job_$(Process).err
output = job_$(Process).out

executable = time_analysis.sh
arguments = glcp_part$(Process).csv

request_cpus = 8
request_memory = 16GB 
request_disk = 10GB

should_transfer_files = YES
when_to_transfer_output = ON_EXIT

queue 50