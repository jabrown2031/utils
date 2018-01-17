
## Password Analyzer

library(data.table)
library(stringr)

args <- commandArgs(TRUE)

pass_list <- args[1]
out_basename <- args[2]

passwords <- data.table(pass=readLines(file(pass_list)))
total <- nrow(passwords)

# top 10 passwords
top.10 <- as.data.frame(head(sort(table(passwords$pass), decreasing=TRUE), 10))
top.10$Password <- rownames(top.10)
rownames(top.10) <- NULL
top.10 <- top.10[,c(1,2)]
colnames(top.10) <- c("Password", "Count")
top.10$Percent <- sprintf("%3.2f%%", ((top.10$Count / total) * 100))
write.csv(top.10, paste(out_basename, "top10.csv", sep="_"), row.names=FALSE)
print("Top 10 Passwords", row.names=FALSE)
print(top.10, row.names=FALSE)

# password length
passwords$len <- nchar(passwords$pass)
pass.length <- as.data.frame(table(passwords$len))
colnames(pass.length) <- c("Password", "Count")
pass.length$Percent <- sprintf("%3.2f%%", ((pass.length$Count / total) * 100))
print("Password Length")
#print(pass.length, row.names=FALSE)

length_table <- table(passwords$len)
#summary(factor(passwords$len, levels = names(length_table[order(length_table, decreasing = TRUE)])))
pass.freq <- as.data.frame(table(factor(passwords$len, levels = names(length_table[order(length_table, decreasing = TRUE)]))))
colnames(pass.freq) <- c("Password Length", "Count")
pass.freq$Percent <- sprintf("%3.2f%%", ((pass.freq$Count / total) * 100))
write.csv(pass.freq, paste(out_basename, "len.csv", sep="_"), row.names=FALSE)
print(pass.freq, row.names=FALSE)

# pasword composition
lower.alpha <- sum(grepl("^[[:lower:]]*$",passwords$pass))
upper.alpha <- sum(grepl("^[[:upper:]]*$",passwords$pass))
numeric_char <- sum(grepl("^[[:digit:]]*$",passwords$pass))
alpha_char <- sum(grepl("^[[:alpha:]]*$",passwords$pass))
alpha_numeric <- sum(grepl("^[[:alnum:]]*$",passwords$pass))
punct_char <- sum(grepl("^[[:punct:]]*$",passwords$pass))

print(sprintf("lowercase alpha characters = %d, (%3.3f%%)", lower.alpha, (lower.alpha/total)*100))
write(sprintf("lowercase alpha characters = %d, (%3.3f%%)", lower.alpha, (lower.alpha/total)*100), file=paste(out_basename, "comp.txt", sep="_"))
print(sprintf("uppercase alpha characters = %d, (%3.3f%%)", upper.alpha, (upper.alpha/total)*100))
write(sprintf("uppercase alpha characters = %d, (%3.3f%%)", upper.alpha, (upper.alpha/total)*100), file=paste(out_basename, "comp.txt", sep="_"), append=TRUE)
print(sprintf("numeric characters = %d, (%3.3f%%)", numeric_char, (numeric_char/total)*100))
write(sprintf("numeric characters = %d, (%3.3f%%)", numeric_char, (numeric_char/total)*100), file=paste(out_basename, "comp.txt", sep="_"), append=TRUE)
print(sprintf("alpha characters = %d, (%3.3f%%)", alpha_char, (alpha_char/total)*100))
write(sprintf("alpha characters = %d, (%3.3f%%)", alpha_char, (alpha_char/total)*100), file=paste(out_basename, "comp.txt", sep="_"), append=TRUE)
print(sprintf("alphanumeric characters = %d, (%3.3f%%)", alpha_numeric, (alpha_numeric/total)*100))
write(sprintf("alphanumeric characters = %d, (%3.3f%%)", alpha_numeric, (alpha_numeric/total)*100), file=paste(out_basename, "comp.txt", sep="_"), append=TRUE)
print(sprintf("punctuation characters = %d, (%3.3f%%)", punct_char, (punct_char/total)*100))
write(sprintf("punctuation characters = %d, (%3.3f%%)", punct_char, (punct_char/total)*100), file=paste(out_basename, "comp.txt", sep="_"), append=TRUE)
