
library(xlsx)
df1=data.frame(x=c(1,5,5),variable=c("a","b","c"))
df2=df1
df2["x"]=0

print(df1[df1["variable"]=="a","x"])



print(class(personnalFile))
