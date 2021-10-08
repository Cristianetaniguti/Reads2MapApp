# Adapt this line according to your HPC
module load R

grep -rw "Submitted batch job" * > jobs

echo -e '
df <- read.table("jobs", sep = "\t") \n
tasks <- sapply(strsplit(df$V1, "/"), function(x) paste0(x[grep("call-", x)], collapse = "/")) \n
ids <- sapply(strsplit(df$V1, "/"), function(x) x[length(x)]) \n
ids <- sapply(strsplit(ids, " "), function(x) x[length(x)]) \n
tasks <- gsub("call-", "", tasks) \n
tasks_ids <- data.frame(tasks, ids) \n
write.table(ids, quote = F, file = "jobs_id", col.names = F, row.names = F) \n
write.table(tasks_ids, quote = F, file = "tasks_id.txt", col.names = F, row.names = F)
'  > $(pwd)/script_efficiency.R

Rscript script_efficiency.R

for i in $(cat jobs_id);do
  seff $i >> efficiency.infos.txt
done